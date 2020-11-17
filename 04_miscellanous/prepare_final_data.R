## Author: Andres Camilo Mendez Alzate
##### script to extract data from a shapefile


pacman::p_load(raster,sp, exactextractr, sf, tidyverse,lattice, rasterVis, 
               maptools, latticeExtra, sp, RColorBrewer, future, furrr, data.tree,
               collapsibleTree, stringi, writexl, biscale, cowplot, stars)


### ftp://ftp.ciat.cgiar.org/DAPA/Projects/shared_results/               

### function para calcular metricas por cada cultivo

gap_crop_coverage <- function(i, file_path = NULL, save.raster = FALSE ){
  
  mt <- matrix(c(0, 2, 0, 2, 3, 1), 2,3, byrow = T)
  status <- tryCatch(expr = {
  
    #cargar los rasters 
    
    rst <- i %>%  
      lapply(., raster)
    
    #calcular coverages 
    
    upper <- lapply(rst, function(k){
      rst <- k
      rst[rst[] != 3] <- NA
      
      return(rst)
    }) %>% 
      raster::stack(.) %>%
      sum(., na.rm = T)
    
    upper[upper[] == 0] <- NA
    
    lower <- lapply(rst, function(k){
      rst <- k
      rst[rst[] < 2] <- NA
      return(rst)
    }) %>% 
      raster::stack(.) %>%
      sum(., na.rm = T)
    
    lower[lower[] == 0] <- NA
    
    area_tot <- rst %>% 
      raster::stack(.) +1 
    area_tot <- sum(area_tot, na.rm = T)
    area_tot[area_tot == 0] <- NA
    
    covg_upp <- 100 - length(which(!is.na(upper[])))/length(which(!is.na(area_tot[])))*100
    covg_low <- 100 - length(which(!is.na(lower[])))/length(which(!is.na(area_tot[])))*100
    
    rich <- rst %>% 
      raster::stack(.) %>% 
      reclassify(., mt) %>% 
      sum(., na.rm = T) 
    
    rich[rich[] == 0] <- NA
    
    
    if(save.raster){
      if(is.null(file_path)){
        stop("If save raster == TRUE, file path must be provided.")
      }else{
        writeRaster(rich, file_path, overwrite = T, NAflag = 0)
      }
      
    }
    
    
    status <- data.frame(lower = covg_low, upper = covg_upp, status = "Done" , stringsAsFactors = F)
    
  
}, error = function(e){
  
 
  status <- data.frame(lower= NA, upper = NA, status = "ERROR", stringsAsFactors = F)
  
})

 return(status) 
}

########## cargar shapefile del mundo con zonas en disputa para realizar el cargue de información

shp <-shapefile("Z:/gap_analysis_landraces/runs/input_data/shapefiles/new_world_shapefile/ne_50m_admin_0_countries.shp")

shp@data <- shp@data %>% 
  dplyr::select(LEVEL, ADMIN, ADM0_A3, NAME, NAME_LONG, FORMAL_EN, POP_EST,POP_RANK, GDP_MD_EST, FIPS_10_, ISO_A2, ISO_A3, ADM0_A3_IS,
                CONTINENT, SUBREGION, REGION_WB)

##############################################################
######## 1. calcular gap metrics para todos los cultivos #######
##############################################################

root_dir <- "Z:/gap_analysis_landraces/runs/results/"
#get crop folder paths
crops_paths <- data.frame(pth = paste0(list.dirs(path = root_dir, full.names = T, recursive = F), "/lvl_1") ,
                          name = list.dirs(path = root_dir, full.names = F, recursive = F),
                          stringsAsFactors = F)


# get race folder paths
baseDir <- "Z:/gap_analysis_landraces/runs"
crops <- list.dirs(paste0(baseDir, "/results"), full.names = F, recursive = F)

gap_maps_pths <- lapply(crops, function(i){
  pths <- list.files(paste0(baseDir, "/results/", i, "/lvl_1"), 
             pattern = "gap_class_final_new.tif$",
             recursive = T,
             full.names = T)
  if(length(pths) == 0){
  pths <- "Not_found"  
  race_name <- NA_character_
  }else{
    race_name <- stringr::str_extract(pths , pattern = "(lvl_1)(/)*[a-zA-Z0-9_-]+") %>% 
      gsub(pattern = "(lvl_1)(/)*|(/)", replacement = "", .)
  }
  pths <- as.character(pths)
  return(data.frame(paths = pths, crop_name = i, race_name = race_name))
}) %>% 
  dplyr::bind_rows() %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate_all(., as.character())


# gap_maps_pths %>% 
#   do.call(rbind, .) %>% 
#   write.csv(., "D:/LGA_dashboard/www/crop_races_paths.csv", row.names = F)

#gap_maps_pths <- read.csv("Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/crop_races_paths.csv", header = T, stringsAsFactors = F)

#metrics <- read.csv("Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/coverage_metrics.csv", header = T, stringsAsFactors = F)

shp <-shapefile(paste0(baseDir, "/input_data/shapefiles/new_world_shapefile/ne_50m_admin_0_countries.shp"))

shp@data$ISO_A2[shp@data$ISO_A2 == "-99"] <- c("SX", "NO", "FR", "NC", "-99", "-99", "-99")

shp@data <- shp@data %>% 
  dplyr::select(LEVEL, ADMIN, ADM0_A3, NAME, NAME_LONG, FORMAL_EN, POP_EST,POP_RANK, GDP_MD_EST, FIPS_10_, ISO_A2, ISO_A3, ADM0_A3_IS,
                CONTINENT, SUBREGION, REGION_WB)

shp_sf <- sf::st_as_sf(shp)

### creación de objeto con clase S4 para guardar resultados
setClass("results", slots = list(race = "list", 
                                 crop = "list", 
                                 combined_results = "data.frame", 
                                 diversity_tree = "data.frame",
                                 country_area  = "data.frame",
                                 coverage_totals = "data.frame"))

consolidado <- new("results", race = list(), crop = list(), combined_results = data.frame(), diversity_tree = data.frame(), country_area = data.frame(), coverage_totals = data.frame())


future::plan(future::multisession, workers = 10)

consolidado@race <- gap_maps_pths %>% 
  filter(!grepl("old", crop_name) & !grepl("old", race_name) & !is.na(race_name)) %>% 
  dplyr::mutate(summary_metrics_race = furrr::future_map(.x = paths, .f = function(.x){
    cat("Processing: ", .x, "\n")
    if(.x != "Not_found"){
      covg <- tryCatch(expr = {
        rs_gaps <-  raster::raster(.x) 
        covg_ul <- 100 - length(which(rs_gaps[]==3))/length(which(!is.na(rs_gaps[])))*100
        covg_ll <- 100 - length(which(rs_gaps[]>=2))/length(which(!is.na(rs_gaps[])))*100
        data.frame(covg_ul, covg_ll)
      }, error = function(e){
        covg_ul <- NA
        covg_ll <- NA
        data.frame(covg_ul, covg_ll)
      })
      
      auc_avg <- list.files(paste0(.x, "/../../gap_validation/buffer_100km"), full.names = TRUE, pattern = ".csv$") %>% 
        purrr::map(.x = ., function(.x){
          
          val <- tryCatch(expr = {
            val <-  read.csv(.x, header = T, stringsAsFactors = F) %>% 
              filter(pnt == "Mean") %>% 
              pull(auc.mean)
          }, error = function(e){
            val <- NA
          })
          
          return(val*100)
        }) %>% 
        unlist() %>% 
        mean(., na.rm = T)
      
      res <- data.frame(lower = covg$covg_ll, upper = covg$covg_ul, auc_avg = auc_avg)
      
      
    }else{ res <- data.frame(lower = NA, upper = NA, auc_avg = NA)}
    
    return(res)
  }),
  
  gap_area_country_race = furrr::future_pmap(.l = list(x = paths, y = crop_name, z = race_name), .f = function(x, y, z){
    
    cat("Calculating area for: ", y, "-", z, "\n" )
    rst <- raster::raster(x)
    gp_area_min <- rst
    gp_area_max <- rst
    total_area <- rst
    
  
    gp_area_max[gp_area_max[] != 3] <- NA
    
    
    gp_area_min[ !(gp_area_min[] %in% c(2,3))] <- NA
    
    
    total_area[!is.na(total_area)] <- 1
    
    gap_area_min <- raster::area(gp_area_min) %>% 
      raster::mask(., mask = gp_area_min) %>% 
      exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE)
    
    gap_area_max <- raster::area(gp_area_max) %>% 
      raster::mask(., mask = gp_area_max) %>% 
      exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE)
    
    total_area <- raster::area(total_area) %>% 
      raster::mask(., mask = total_area) %>% 
      exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE)
    
    name <- paste0(y, ".", z)
    
    res <- tibble(country= shp@data$ADMIN, 
                  ISO2 = shp@data$ISO_A2, 
                  gap_area_min =  unlist(gap_area_min),
                  gap_area_maxn =  unlist(gap_area_max),
                  sdm_area =  unlist(total_area)) %>% 
      dplyr::mutate(coverage_lower = round(1-gap_area_min/sdm_area, 3),
                    coverage_upper = round(1-gap_area_max/sdm_area, 3),
                    coverage = round((coverage_lower+coverage_upper)/2, 3) )
    
    suppressWarnings( names(res)[3:ncol(res)] <- paste0(names(res[3:ncol(res)]), "_", y, "_", z ) )
    
    return(res)
    
    
  })
  )# end consolidado race 


## Consolidado por cultivo


consolidado@crop <- crops_paths %>% 
  as_tibble %>% 
  dplyr::mutate(
  
    final_map_paths = furrr::future_map(.x = pth, .f = function(.x){
      pths <- list.files(path = as.character(.x), recursive = T, full.names = T, pattern = "gap_class_final_new.tif$")
      pths <-  pths[!grepl(pattern = "old", pths)]
      
      if(length(pths) == 0){
        pths <- "Not_found"
      }
      return(pths)
    }),
    
  crops_coverage =  furrr::future_pmap(.l = list(.x = final_map_paths, .y = name, .z = pth), .f = function(.x, .y, .z){
  cat("Caculating coverage for: ", .x, " \n")

      pths <- unlist(.x)

    
    if(("Not_found" %in% pths)){x <- NA}else{
      file_path <- paste0(as.character(.z),"/gap_richness_", .y, ".tif") 
      x <- gap_crop_coverage(i = pths, file_path = file_path, save.raster = T)} 

    return(x)
  }),
  
  country_coverage = furrr::future_map(.x = final_map_paths, .f = function(.x){
    
    pths <- unlist(.x)
    if(("Not_found" %in% pths)){
      x <- tibble(country = NA_character_, ISO2 = NA_character_, gap_area = NA, sdm_area = NA, coverage = NA)
      }else{
      
      rst <- pths %>%  
        lapply(., raster)
      
      gap_max <-  lapply(rst, function(k){
        rst <- k
        rst[rst[] != 3] <- NA
        return(rst)
      }) %>% 
        raster::stack(.) %>%
        sum(., na.rm = T)
      
      gap_min <-  lapply(rst, function(k){
        rst <- k
        rst[!(rst[] %in% c(2,3))] <- NA
        return(rst)
      }) %>% 
        raster::stack(.) %>%
        sum(., na.rm = T)
      
      
      gap_max[gap_max == 0] <- NA
      gap_max[!is.na(gap_max)] <- 1
      
      gap_min[gap_min == 0] <- NA
      gap_min[!is.na(gap_min)] <- 1
      
      area_tot <- rst %>% 
        raster::stack(.) +1 
      area_tot <- sum(area_tot, na.rm = T)
      
      area_tot[area_tot == 0] <- NA
      area_tot[!is.na(area_tot)] <- 1
      
      gp_area_min <- exactextractr::exact_extract(gap_min, shp_sf, fun = 'sum', progress = FALSE)
      gp_area_max <- exactextractr::exact_extract(gap_max, shp_sf, fun = 'sum', progress = FALSE)
      sdm_tot <- exactextractr::exact_extract(area_tot, shp_sf, fun = 'sum', progress = FALSE)
      
      x <- tibble(country = shp@data$NAME, 
                  ISO2 = shp@data$ISO_A2, 
                  gap_area_min = gp_area_min, 
                  gap_area_max = gp_area_max,
                  sdm_area = sdm_tot) %>% 
        dplyr::mutate_if(is.numeric, round) %>% 
        dplyr::mutate(coverage_min = 1 - (gap_area_min/sdm_area),
                      coverage_max = 1 - (gap_area_max/sdm_area),
                      coverage     = round((coverage_min+coverage_max)/2, 3) ,
                      coverage     = ifelse(is.nan(coverage), 0, coverage))  
        
      
      }
    return(x)
  }),
  
  accessions_count = furrr::future_map(.x = name, .f = function(.x) {
    cat("Calculating metrics for: ", .x, "\n")
    crop_db_path <- paste0(baseDir, "/input_data/by_crop/", .x, "/lvl_1/classification/", .x, "_bd_identifiers.csv")
    
    if(file.exists(crop_db_path)){
      identifers <- suppressMessages( readr::read_csv(crop_db_path))
      
      used_freq <- identifers %>% 
        group_by(ensemble, used) %>%
        tally() %>%
        dplyr::mutate(freq_in_gr = n/sum(n)) %>% 
        ungroup %>% 
        dplyr::mutate(total        = sum(n),
                      total_used   = identifers %>% filter(used) %>% nrow(),
                      freq_overall = n/sum(n)
                      )
        
      source_db_summ <- identifers %>% 
        dplyr::filter(used) %>% 
        group_by(ensemble, source_db) %>%
        tally() %>% 
        dplyr::mutate(freq_in_gr = n/sum(n) ) %>% 
        ungroup() %>% 
        dplyr::mutate(source_db = toupper(source_db), freq_overall = n/sum(n))
      
      
      source_db_freq <- identifers %>% 
        dplyr::filter(used) %>% 
        group_by( source_db) %>%
        tally() %>% 
        dplyr::mutate(source_db = toupper(source_db),freq_in_gr = n/sum(n) )
      
      status_db_freq <- identifers %>% 
        dplyr::filter(used) %>% 
        group_by( status) %>%
        tally() %>% 
        dplyr::mutate(status = toupper(status),freq_in_gr = n/sum(n) )
      
     res <- list("used_freq" = used_freq, "source_db_summ" = source_db_summ, "source_db_freq" = source_db_freq, "status_db_freq" = status_db_freq)
    }else{
      
      res <- list(used_freq = NA, source_db_summ = NA, source_db_freq = NA, status_db_freq = NA)
    }
    
    return(res)
  }),
  
  accessions_count_per_country = furrr::future_map(.x = name, .f = function(.x) {
   
     crop_db_path <- paste0(baseDir, "/input_data/by_crop/", .x, "/lvl_1/classification/", .x, "_bd_identifiers.csv")
    
    if(file.exists(crop_db_path)){
      identifers <- suppressMessages( readr::read_csv(crop_db_path))
      
      used_occ <- identifers %>% 
        dplyr::filter(used) %>% 
        dplyr::select(matches("latit"), matches("long"))
      
      coordinates(used_occ) <- ~Longitude+Latitude
      crs(used_occ) <- crs(shp)
      
      ext <- suppressWarnings(raster::extract(shp, used_occ))  %>% 
        dplyr::select(NAME, ISO_A2) %>% 
        dplyr:::group_by(NAME, ISO_A2) %>% 
        dplyr::tally() %>% 
        dplyr::ungroup() %>% 
        dplyr::select("country" = NAME, "ISO2" = ISO_A2, occ_count = n)
      
      res <- tibble(country = shp@data$NAME, ISO2 = shp@data$ISO_A2 ) %>% 
        left_join(., ext, by = c("country", "ISO2")) 
      

    }else{
      
      res <- tibble(country = NA, ISO2 = NA, occ_count = NA)
      
    }
    return(res)
  }),
  
  gap_area_country_crop = furrr::future_map2(.x = pth, .y = name, .f = function(.x, .y){
    
    pths <- list.files(path = as.character(.x), recursive = F, full.names = T, pattern = "^gap_richness_[a-zA-z0-9]*.tif$")
    pths <-  pths[!grepl(pattern = "old", pths)]
    
    if(length(pths) == 0){
      pths <- "Not_found"
      res <- tibble(country= NA, ISO2 =NA, area =  NA )
    }else if(length(pths) >= 2){
      warning("More than one gap richness map found, using the first one \n")
      pths <- pths[1]
    }
    
    if(pths != "Not_found"){
      
      cat("Calculating area for: ", .y, "\n" )
      rst <- raster::raster(pths)

      gap_area_rst <- raster::area(rst) %>% 
        raster::mask(., mask = rst) %>% 
        exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE)
      
      name <- paste0(.y)
      
      ar <- unlist(gap_area_rst)
      qq <- quantile(ar[ar != 0], probs = 0.75)
      
      res <- tibble(country= shp@data$ADMIN, 
                    ISO2 = shp@data$ISO_A2, 
                    area =  ar) %>% 
        dplyr::mutate(quantile = ifelse(ar >= qq, 1, 0)) #Julian's method for prioritizing
      
      suppressWarnings( names(res)[3:4] <- c(name , paste0(name, ".quantile")) )
      
    }
    
    return(res)
    
  })
  
    ) #end mutate


future::plan(future::sequential)


#calculating quantiles for gap areas and combine with diversity tree

country_gp <- consolidado@crop$gap_area_country_crop
country_gp <- country_gp[!sapply(country_gp, function(x) any(is.na(x)))]

country_gp<- base::Reduce(dplyr::full_join, country_gp)

sgp_priorities <- country_gp %>% 
  dplyr::select(country, ISO2, matches("quantile")) 

sgp_priorities$suma_spatial <- sgp_priorities[, 3:ncol(sgp_priorities)] %>% rowSums()
sgp_priorities <- sgp_priorities %>% 
  dplyr::select(country, ISO2, suma_spatial)

dv_tree_gp <- read_csv(paste0(baseDir, "/LGA_documents/diversity_tree_results/diversity_tree_priorities.csv"))
consolidado@diversity_tree <- dv_tree_gp

dv_tree_gp$suma_dvTree <- dv_tree_gp[, 4:15] %>% rowSums()
dv_tree_priorities <- dv_tree_gp %>% 
  dplyr::select(Country, suma_dvTree) %>% 
  dplyr::mutate(Country_id =  stringi::stri_trans_general(str = Country, id = "Latin-ASCII"), 
                Country_id = tolower(Country_id))


# combining spatial results with diversity tree priorities
consolidado@combined_results <- sgp_priorities %>% 
  dplyr::mutate(country_id =  stringi::stri_trans_general(str = country, id = "Latin-ASCII"), 
                country_id = tolower(country_id)) %>% 
  dplyr::left_join(., dv_tree_priorities, by = c("country_id" = "Country_id")) %>% 
  dplyr::select(country, ISO2, suma_spatial, suma_dvTree) %>% 
  dplyr::mutate(Total = rowSums(.[,3:4], na.rm = T))

#save countries area
consolidado@country_area <- shp@data %>% 
  dplyr::select("country" =  NAME, "ISO2" = ISO_A2) %>% 
  dplyr::mutate(area_tot = round(raster::area(shp)/1000000)) %>% 
  as_tibble()

#Calculate country gap area for gap richnnes world map
tmp_maps <- consolidado@crop %>% 
  dplyr::mutate(temp_maps = purrr::map2(.x = final_map_paths, .y = name, .f = function(.x, .y){
pths <- unlist(.x)
if(("Not_found" %in% pths)){
  ret <- list(gap = NA, sdm_area = NA)
}else{
  
  rst <- pths %>%  
    lapply(., raster)
  
  gap_max <-  lapply(rst, function(k){
    rst <- k
    rst[rst[] != 3] <- NA
    return(rst)
  }) %>% 
    raster::stack(.) %>%
    sum(., na.rm = T)
  
  gap_min <-  lapply(rst, function(k){
    rst <- k
    rst[!(rst[] %in% c(2, 3))] <- NA
    return(rst)
  }) %>% 
    raster::stack(.) %>%
    sum(., na.rm = T)
  
  gap_max[gap_max == 0] <- NA
  gap_max[!is.na(gap_max)] <- 1
  
  gap_min[gap_min == 0] <- NA
  gap_min[!is.na(gap_min)] <- 1
  
  area_tot <- rst %>% 
    raster::stack(.) +1 
  area_tot <- sum(area_tot, na.rm = T)
  area_tot[area_tot == 0] <- NA
  area_tot[!is.na(area_tot)] <- 1
  
  ret <- list(gap_min = gap_min, gap_max = gap_max, sdm_area = area_tot)
  removeTmpFiles(h=0)
}#end else
return(ret)
})) %>% 
  dplyr::pull(temp_maps)

sum_sdm <- lapply(tmp_maps, function(k){
  if("Raster" %in% is(k$sdm_area)){
    ret  <- k$sdm_area %>%
      raster::crop(x = ., y = extent(c(-180, 180, -59.5, 83.625)) ) 
  }else{  ret  <- k$sdm_area}
  return(ret)
})
sum_gap_min <- lapply(tmp_maps, function(k){
  if("Raster" %in% is(k$gap)){
    ret  <- k$gap_min %>%
      raster::crop(x = ., y = extent(c(-180, 180, -59.5, 83.625)) ) 
  }else{  ret  <- k$gap_min}
  return(ret)
})
sum_gap_max <- lapply(tmp_maps, function(k){
  if("Raster" %in% is(k$gap)){
    ret  <- k$gap_max %>%
      raster::crop(x = ., y = extent(c(-180, 180, -59.5, 83.625)) ) 
  }else{  ret  <- k$gap_max}
  return(ret)
})

#rm(tmp_maps)

sum_sdm <- sum_sdm[!is.na(sum_sdm)]
sum_sdm$fun <- sum
sum_sdm$na.rm <- TRUE
sdm_total <- do.call(raster::mosaic, sum_sdm)
sdm_total[sdm_total[] == 0] <- NA
writeRaster(sdm_total, paste0(baseDir, "/results/sdm_all_crops_sum.tif"), overwrite = T)
#rm(sum_sdm)

sum_gap_min <- sum_gap_min[!sapply(sum_gap_min, is.null, simplify = TRUE)]
sum_gap_min$fun <- sum
sum_gap_min$na.rm <- TRUE
gap_total_min <- do.call(raster::mosaic, sum_gap_min)
gap_total_min[gap_total_min[] == 0] <- NA
#rm(sum_gap_min)

sum_gap_max <- sum_gap_max[!sapply(sum_gap_max, is.null, simplify = TRUE)]
sum_gap_max$fun <- sum
sum_gap_max$na.rm <- TRUE
gap_total_max <- do.call(raster::mosaic, sum_gap_max)
gap_total_max[gap_total_max[] == 0] <- NA
#rm(sum_gap_max)

consolidado@coverage_totals <- tibble( country       = shp@data$NAME, 
                                       ISO2       = shp@data$ISO_A2, 
                                       gap_total_min  = raster::area(gap_total_min) %>% 
                                         raster::mask(., gap_total_min) %>% 
                                         exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE) %>% 
                                         round() , 
                                       gap_total_max  = raster::area(gap_total_max) %>% 
                                         raster::mask(., gap_total_max) %>% 
                                         exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE) %>% 
                                         round() ,
                                       sdm_total  = raster::area(sdm_total) %>% 
                                         raster::mask(., sdm_total) %>% 
                                         exactextractr::exact_extract(., shp_sf, fun = 'sum', progress = FALSE) %>% 
                                         round()) 
  

#########################################
### CALCULAR GAP RICHNESS MAP ###########
########################################

if(TRUE){
  maps_pths <- consolidado@crop$pth %>% 
    list.files(., pattern = "^gap_richness_[a-zA-z0-9]*.tif$", full.names = T)
  #list.files("Z:/gap_analysis_landraces/runs/results", pattern = "^gap_richness_[a-zA-z0-9]*.tif$", recursive = T)
  #list.files("Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/scripts", pattern = ".tif$", full.names = T)
  
  msk <- raster("Z:/gap_analysis_landraces/runs/input_data/mask/mask_world.tif")
  
  rst <- lapply(maps_pths, function(i){
    cat("Processing:", i, "\n")
    r <- raster(i) 
    
    r[r[] > 0] <- 1
    r[r[] <= 0] <- NA 
    
    r <- raster::crop(r, extent(msk))
    return(r)
  })
  
  rst$fun <- sum
  rst$na.rm <- TRUE
  gp_rich <- do.call(mosaic, rst)
  gp_rich[gp_rich[] == 0] <- NA
  
  writeRaster(gp_rich, "Z:/gap_analysis_landraces/runs/results/all_crops_gap_richness.tif", overwrite =T)
  
}

save(consolidado, file = paste0(baseDir, "/results/results_all_crops.RData"))


#####################################################
############## update table metrics in FTP server
####################################################

root_ftp <- "Z:/gap_analysis_landraces/shared_results"

## Race coverage metrics

for(i in 1:nrow(consolidado@race)){
  var <- consolidado@race %>% 
    slice(i) %>% 
    dplyr::select(crop_name ,race_name, summary_metrics_race, gap_area_country_race) 
  
  cat("Extracting metrics for",  as.character(var$crop_name), "... in group:",as.character(var$race_name),  "\n")
  
  ftp_pth <- paste0(root_ftp, "/spatial_lga/", var$crop_name,"/results/", var$race_name)
  
  mtrs <- tibble(coverage_lower = unlist(var$summary_metrics_race)[1],
                 coverage_upper = unlist(var$summary_metrics_race)[2]) %>% 
    dplyr::mutate(coverage = round((coverage_lower+coverage_upper)/2, 3))
  
  covg_mtrs <- var$gap_area_country_race %>% 
    purrr::pluck(., 1) %>% 
    dplyr::mutate_if(is.numeric,
                     function(i){
                       x <- ifelse(is.nan(i), 0, i)
                       return(x)
                     })
  
  lst <- list(coverage_metrics = mtrs , country_coverage = covg_mtrs)
  
  writexl::write_xlsx(lst, path = paste0( ftp_pth,"/",as.character(var$crop_name), "_" ,as.character(var$race_name), "_coverage",  ".xlsx"))
  
  
}#end for

## Race per Crop

for(i in 1:nrow(consolidado@crop)){
  
  
  var <- consolidado@crop %>% 
    dplyr::slice(i) %>% 
    dplyr::select(name, crops_coverage, country_coverage)
  
  cat("Extracting metrics for",  as.character(var$name), "... ",  "\n")
  
  
  ftp_pth <- paste0(root_ftp, "/spatial_lga/", var$name,"/results/")
  
  if(dir.exists(ftp_pth)){
    mtrs <- tibble(coverage_lower = as.numeric(purrr::pluck(var$crops_coverage, 1)[1]),
                   coverage_upper = as.numeric(purrr::pluck(var$crops_coverage, 1)[2])) %>% 
      dplyr::mutate(coverage = round((coverage_lower+coverage_upper)/2, 3))
    
    covg_mtrs <- var$country_coverage %>% 
      purrr::pluck(., 1) %>% 
      dplyr::mutate_if(is.numeric,
                       function(i){
                         x <- ifelse(is.nan(i), 0, i)
                         return(x)
                       })
    
    lst <- list(coverage_metrics = mtrs , country_coverage = covg_mtrs)
    
    writexl::write_xlsx(lst, path = paste0( ftp_pth,as.character(var$name), "_coverage",  ".xlsx"))
  }
  
}#end for


######### Pruebas de con mapas

msk <- raster("C:/Users/acmendez/Google Drive/CIAT/hidden_project/Gap_analysis_UI_original/www/masks/mask_world.tif")
gap_rich <- raster("D:/OneDrive - CGIAR/Attachments/Desktop/gap_richness_map.tif")
shp <- raster::shapefile("C:/Users/acmendez/Google Drive/CIAT/hidden_project/Gap_analysis_UI_original/www/world_shape_simplified/all_countries_simplified.shp")


total_area <- raster::area(msk) %>% 
  raster::mask(., mask = msk) %>% 
  exactextractr::exact_extract(., st_as_sf(shp), fun  = 'sum')

gap_area_rst <- raster::area(gap_rich) %>% 
  raster::mask(., mask = gap_rich) %>% 
  exactextractr::exact_extract(., st_as_sf(shp), fun = 'sum')

gap_count <- exactextractr::exact_extract(gap_rich, st_as_sf(shp), fun = "max")

vth <- download_map_data("https://code.highcharts.com/mapdata/custom/world-palestine-highres.js" )
vth <- get_data_from_map(vth)

hcmp_data<- vth %>% 
  dplyr::select(`hc-a2`, name, subregion, `iso-a3` , `iso-a2`)

gap_data <- shp@data %>% 
  dplyr::select(ISO2, ISO3, NAME) %>% 
  dplyr::mutate(area_total = round(total_area, 0), gap_area = round(gap_area_rst, 0), 
                gap_count = round(gap_count,0)) %>% 
  as_tibble


hcmp_data %>% 
  dplyr::left_join(., gap_data, by = c("iso-a2" = "ISO2")) %>% 
  dplyr::mutate(area_total = ifelse(is.na(area_total ), 0, area_total ),
                gap_area   = ifelse(is.na(gap_area),0, gap_area),
                gap_count  = ifelse(is.na(gap_count),0, gap_count)) %>% 
  dplyr::mutate(area_freq = round(gap_area/area_total, 1),
                area_freq = ifelse(is.nan(area_freq), 0, area_freq)) %>% 
  write.csv(., "D:/OneDrive - CGIAR/Attachments/Desktop/gaps_summary_template.csv", row.names = F)




nrow(gap_data)
nrow(hcmp_data)

#### dviersity tree Template


x <- readLines("C:/Users/acmendez/Downloads/Cowpea  genepool.tree.json") %>% fromJSON(txt =., simplifyDataFrame = F)

repos <- as.Node(x)
print(repos, "id", "login")/

plot(repos)

tree_df <- ToDataFrameTypeCol(repos) 

tree_df <- tree_df[, !grepl("children", tree_df)]

names(tree_df) <- paste0("level_", 1:ncol(tree_df))

tree_df <- as_tibble(tree_df)

edgelist <- list()
i <- 1
while(i <= ncol(tree_df)){
  cat(i, "\n")
  
  if(i+1 <= ncol(tree_df)){
  lvl1 <- paste0("level_", i)
  lvl2 <- paste0("level_", i+1)
  
  
  edgelist[[i]]<- tree_df %>% 
    dplyr::group_by(eval(parse(text= lvl1)), eval(parse(text= lvl2))) %>% 
    tally %>% 
    ungroup %>% 
    dplyr::select(start = `eval(parse(text = lvl1))`, end = `eval(parse(text = lvl2))`)
  }
  
  i <- i + 1 
}

edgelist <- edgelist %>% 
  bind_rows()

edgelist %>% 
  dplyr::filter(!is.na(end)) %>% 
  dplyr::add_row(start = NA, end = unlist(edgelist[1,1]), .before = 1 ) %>%
  dplyr::mutate(collected = "") %>% 
  collapsibleTree::collapsibleTreeNetwork(., attribute = "collected", fontSize = 7)




plot(as.igraph(repos, directed = TRUE, direction = "climb"))


##### calculate gap richnes for each crop


root_dir <- "Z:/gap_analysis_landraces/runs/results/"

crops_paths <- data.frame(pth = paste0(list.dirs(path = root_dir, full.names = T, recursive = F), "/lvl_1") ,
                          name = list.dirs(path = root_dir, full.names = F, recursive = F),
                          stringsAsFactors = F)


cov_mtrs <- apply(crops_paths,1, function(i){
  
  
  cat("Calculating Gap richness for crop: ", i[2], "\n")
  mt <- matrix(c(0, 2, 0, 2, 3, 1), 2,3, byrow = T)
  
  x<-tryCatch({
    
    rst <- list.files(path = as.character(i[1]), recursive = T, full.names = T, pattern = "gap_class_final_new.tif$")
    rst <-  rst[!grepl(pattern = "old", rst)]
    
    if(length(rst) == 0){
      stop("No file found")
    }
    #cargar los rasters   
    rst <- rst %>%  
      lapply(., raster)
    
    
    #calcular coverages 
    
    upper <- lapply(rst, function(k){
      rst <- k
      rst[rst[] != 3] <- NA
      
      return(rst)
    }) %>% 
      raster::stack(.) %>%
      sum(., na.rm = T)
    
    upper[upper[] == 0] <- NA
    
    
    lower <- lapply(rst, function(k){
      rst <- k
      rst[rst[] < 2] <- NA
      return(rst)
    }) %>% 
      raster::stack(.) %>%
      sum(., na.rm = T)
    
    lower[lower[] == 0] <- NA
    
    area_tot <- rst %>% 
      raster::stack(.) +1 
    area_tot <- sum(area_tot, na.rm = T)
    area_tot[area_tot == 0] <- NA
    
    covg_upp <- 100 - length(which(!is.na(upper[])))/length(which(!is.na(area_tot[])))*100
    covg_low <- 100 - length(which(!is.na(lower[])))/length(which(!is.na(area_tot[])))*100
    
    rich <- rst %>% 
      raster::stack(.) %>% 
      reclassify(., mt) %>% 
      sum(., na.rm = T) 
    
    rich[rich[] == 0] <- NA
    # writeRaster(rich, paste0("Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/", i[2], ".tif"), overwrite = T, NAflag = 0)
    
    status <- data.frame(lower = covg_low, upper = covg_upp, status = "Done", stringsAsFactors = F)
    
    return(status)
  }, error = function(e){
    
    cat("An error ocurred :(  \n")
    status <- data.frame(lower= NA, upper = NA, status = "ERROR", stringsAsFactors = F)
    return(status)
  })
  
  return(cbind( name = i[2], x))
  
})

bind_rows(cov_mtrs) %>% 
  dplyr::mutate(name = crops_paths$name) %>% 
  write.csv(., "Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/coverage_metrics.csv", row.names = F)



# MAPAS EN FORMATO PNG

options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, lattice, rasterVis, maptools, tidyverse, latticeExtra, sp, RColorBrewer))

rsin <- raster("Z:/gap_analysis_landraces/runs/results/all_crops_gap_richness.tif")
wrld <- raster::shapefile('Z:/gap_analysis_landraces/runs/input_data/shapefiles/shp/all_countries.shp')
ssdn <- raster::shapefile('Z:/gap_analysis_landraces/runs/input_data/shapefiles/Sudan_admin_WGS84/Sudan_admin_0.shp')
msk <- raster("Z:/gap_analysis_landraces/runs/input_data/mask/mask_world.tif")
grat <- sp::gridlines(wrld, easts = seq(-180, 180, by = 20), norths = seq(-90, 90, by = 20))


mt <- matrix(c(1, 3, 1, 
               3, 6, 2,
               6,  15, 3), 3,3, byrow = T)

rsin      <- raster::reclassify(x = rsin, rcl = mt, include.lowest=F)
rsin      <- raster::ratify(rsin)
rat       <- levels(rsin)[[1]]
rat$level <- c("1-3", "4-6", "> 7")
if(nrow(rat) == 1){
  cols <- '#2991c6'
}else{
  cols <- grDevices::colorRampPalette(c('#2991c6', '#fafa65', '#e61614'))(nrow(rat))
}
levels(rsin) <- rat

raster::crs(rsin) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
# Figure details
ht  <- 12
fct <- (rsin@extent@xmin-rsin@extent@xmax)/(rsin@extent@ymin-rsin@extent@ymax)
wt  <- ht*(fct+.1)
# Theme
mThm <- rasterVis::rasterTheme(region = brewer.pal(9, "YlGn"), panel.background = list(col = "white"))
# Map
p <- rasterVis:::levelplot(rsin,
                           att          = 'level',
                           xlab         = 'Longitude',
                           ylab         = 'Latitude',
                           scales       = list(x = list(draw = T), y = list(draw = T)),
                           margin       = F,
                           par.settings = mThm,
                           col.regions  =  cols,
                           maxpixels    = ncell(rsin)) + 
  latticeExtra::layer(sp.lines(grat, lwd = 0.5, lty = 2, col = "black")) +
  latticeExtra::layer(sp.polygons(ssdn, lwd = 0.8, col = "#e9e9e9", fill = "#cccccc"), under = T) +
  latticeExtra::layer(sp.polygons(wrld, lwd = 0.8, col = "#e9e9e9", fill = "#cccccc"), under = T)
hght = 3.5
wdth = 10
png("Z:/gap_analysis_landraces/runs/results/all_crops_gap_richness.png", height = hght, width = wdth, units = "in", res = 300)
print(p)
dev.off()

####### BIPLOT GAP COUNTS AND PERCENTAGE
shp <-shapefile("Z:/gap_analysis_landraces/runs/input_data/shapefiles/new_world_shapefile/ne_50m_admin_0_countries.shp")
shp_no_ant <- shp[shp@data$NAME !=  "Antarctica", ]

rsdm <- raster("Z:/gap_analysis_landraces/runs/results/sdm_all_crops_sum.tif")
rsin_r <- raster("Z:/gap_analysis_landraces/runs/results/all_crops_gap_richness.tif")


rsin <- stars::read_stars("Z:/gap_analysis_landraces/runs/results/all_crops_gap_richness.tif")
sf_rsin <- sf::st_as_sf(rsin)

wrld <- raster::shapefile('Z:/gap_analysis_landraces/runs/input_data/shapefiles/shp/all_countries.shp')
grat <- sp::gridlines(wrld, easts = seq(-180, 180, by = 20), norths = seq(-90, 90, by = 20))


rsdm_msk <- raster::mask(rsdm, rsin_r) 
perct <- (rsin_r/rsdm_msk)*100

tmp_file <-  "D:/perct.tif"
writeRaster(perct, tmp_file, overwrite = T)

perct <- stars::read_stars(tmp_file)
sf_perct <- sf::st_as_sf(perct)

rsf <- cbind(sf_rsin, sf_perct)
names(rsf)[1:2]<- c("gap_rich", "perct")

 data <- rsf %>% 
  dplyr::mutate(gap_rich_cat = case_when(
   gap_rich >= 0 & gap_rich <= 3 ~ 1,
   gap_rich >= 4 & gap_rich <= 7 ~ 2,
   gap_rich >= 8 ~ 3
  ),
  perct_cat = case_when(
    perct >= 0 & perct <= 33 ~ 1,
    perct > 33  & perct <= 66 ~ 2,
    perct > 66 ~ 3
  ),
  bi_class = paste0(gap_rich_cat, "-", perct_cat))

#data <- biscale::bi_class(rsf, x = gap_rich , y = perct, style = "equal", dim = 3)

shapefile_df <- broom::tidy(shp_no_ant)

custom_pal  <- bi_pal_manual(val_1_1  = "#FDF5D0", 
                             val_1_2  = "#9bcfe4",  
                             val_1_3  = "#2bafe5", 
                             val_2_1  = "#fcb189", 
                             val_2_2  = "#ae978c", 
                             val_2_3  = "#467a8d",
                             val_3_1  = "#f57334", 
                             val_3_2  = "#a75e3b",
                             val_3_3  = "#5b473e", 
                             preview = FALSE)

map <- ggplot() +
  geom_polygon(data=shapefile_df, aes(x=long, y=lat, group=group), fill=  "#cccccc", color="#cccccc", size=0.2) +
  geom_sf(data , mapping = aes(fill=bi_class), colour= NA, show.legend = FALSE) +
  geom_polygon(data=shapefile_df, aes(x=long, y=lat, group=group), fill=  NA, color="#e9e9e9", size=0.2)+
  geom_sf(data = st_as_sf(grat), size = 0.2, linetype = "dashed")+
  coord_sf(xlim = c(-170, 170), ylim = c(-58, 83), expand = FALSE) +
  xlab("Longitude")+ ylab("Latitude")+
  bi_scale_fill(pal = custom_pal, #"GrPink", 
                dim = 3) +
  theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA))



legend <- bi_legend(pal = custom_pal, #"GrPink" 
                    dim = 3,
                    xlab = "# Crop with gaps",
                    ylab = "% Crop with gaps",
                    size = 18)

allplot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.02, -0.00009, 0.3, 0.3)

save_plot(paste0("D:/", "bivariate_plot_gap_crichness.png"), base_width=10, base_height=10, allplot,
          base_asp = 1.2)


#### grafico de coverage

metrics <- do.call(rbind,consolidado@crop$crops_coverage) %>% 
  dplyr::mutate(mid_point = (lower+ upper)/2,
                label     = consolidado@crop$name) %>% 
  tidyr::drop_na() 
  
metrics %>% 
  ggplot(aes(x = reorder(label, mid_point), y = mid_point))+
  geom_bar(stat = "identity", colour="black", fill = "gray70")+
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1, position= position_dodge())+
  geom_point(position=position_dodge(width = 1), size=3, shape=21, fill="black")+
  scale_y_continuous(limits = c(0, 101), breaks = seq(0,100, by= 10))+
  ylab("Coverage (%)")+
  xlab("")+
  coord_flip()+
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = element_text(size = 10),
                 axis.title = element_text(size = 20),
                 legend.text = element_text(size = 17),
                 legend.title = element_text(size = 20))

ggplot2::ggsave(filename = "Z:/gap_analysis_landraces/runs/results/coverage_graph.png", device = "png", width = 8, height = 6, units = "in")



############ descargar datos para los mapas de higcharter

shp1 <- geojson_read("https://code.highcharts.com/mapdata/custom/world-palestine-highres.geo.json",  what = "sp")
shp_hc <- shp1@data



require(haven)

data <- haven::read_dta("C:/Users/acmendez/Downloads/base de datos (1).dta")
data$id <- 1:nrow(data)
casos <- data %>% 
  dplyr::select(casocontrol, Edad, Comuna, catedad2, Sexo , Deseacontinuarconlaencuesta, id) %>% 
  filter(casocontrol == 1, Deseacontinuarconlaencuesta == "Sí") %>% 
  filter(Comuna != "", Sexo != "", !is.na(Edad)) %>% 
  group_by(catedad2, Sexo) %>% 
  tally()%>% 
  rename("casos" = n)

controles <- data %>% 
  dplyr::select(casocontrol, Edad, Comuna, catedad2, Sexo, Deseacontinuarconlaencuesta, id ) %>% 
  filter(casocontrol == 0,  Deseacontinuarconlaencuesta == "Sí") %>% 
  filter(Comuna != "", Sexo != "", !is.na(Edad)) %>% 
  group_by(catedad2, Sexo) %>% 
  tally() %>% 
  rename("controles"  =n)


muestra <- c( 26,52, 6, 6)

conteos <- data.frame(casos[-1 ,], controles = controles$controles[-c(1,2) ], muestra)

elegidos <-apply(conteos, 1, function(i){
  cat(i[1], "\n")
  edad <- as.numeric(i[1])
  s <- as.character(i[2])
  n <- as.numeric(i[5])
  to_sample <- which(data$catedad2 == edad & data$Sexo == s & data$Deseacontinuarconlaencuesta == "Sí" & data$casocontrol == 0)
  muestra <- sample(to_sample, size = n, replace = F)
  return(muestra)
})

data$elegidos <- 0

data$elegidos[unlist(elegidos)] <- 1

write.table(data$elegidos, "clipboard", sep = "\t")

data %>% 
  filter(casocontrol == 1 |  elegidos == 1) %>% 
  filter(catedad2 != 0) %>% 
  filter(Deseacontinuarconlaencuesta == "Sí") %>% 
  writexl::write_xlsx(., "C:/Users/acmendez/Google Drive/TRABAJOS UNIVALLE/semestre 3/epi aplicada 1/base_covid_filtrada.xlsx")
  
  
  
data %>% 
  dplyr::select(casocontrol, Edad, Comuna, catedad2, Sexo, Deseacontinuarconlaencuesta, id ) %>% 
  filter(casocontrol == 0,  Deseacontinuarconlaencuesta == "Sí") 



bd <- data %>% 
  dplyr::select(casocontrol, Edad, Comuna, catedad2, Sexo ) %>% 
  dplyr::mutate(Comuna = as.numeric(Comuna), id = 1:nrow(.) ) %>%
  filter(!is.na(Comuna), Sexo != "", !is.na(Edad)) 
 

id <- bd$id

glm1 <- bd %>% 
glm(casocontrol ~ catedad2  + Comuna + Sexo, data = .,  family = binomial(link = "log")) 
 
X <- glm1$fitted.values

rr <- Match(Tr= bd$casocontrol, X= X, M=1)


rr$index.control %>% length()

bd$casocontrol %>% table

which(bd$casocontrol == 1) %in% rr$index.control

bd$elegidos <- "No elegidos"
bd[rr$index.control, "elegidos"] <- "Control"

nrow(data  )

casos %>% 
  dplyr::left_join(., controles, by = c("Comuna" = "Comuna"))

names(data, edad)



data$edad_cat <- as.numeric(as.factor(data$edad))
data$edad <- as.factor(data$edad)
data$hriaca <- as.factor(data$hriaca)


l1 <- glm(casos~ edad  , offset = log(poblacion),data = data, family= poisson(link = "log"))
summary(l1)


l2 <- poissonirr(casos~ edad + hriaca  +edad*hriaca + offset(log(poblacion)) ,data = data)

pr <- predict(l1, data = data[, -1]) %>% exp()

data$inc_edad <- data$casos/ data$poblacion

sum <- data %>%
  dplyr::mutate(pred = pr,
                inc_pred = pred/poblacion) 
  # group_by(edad) %>% 
  # dplyr::summarise(n = sum(casos), 
  #           pob = sum(poblacion), 
  #           n_pred = sum(pred)) %>% 
  # ungroup() %>% 
  # mutate(edad_cat = 1:6, 
  #        inc_pred = n_pred/pob, 
  #        inc_edad = n/pob)

sum %>% 
  dplyr::select(edad_cat, inc_edad, inc_pred, hriaca ) %>%
  dplyr::filter(hriaca ==0 ) %>%
  dplyr::select(-hriaca) %>% 
  pivot_longer(cols = - c(edad_cat)) %>% 
  ggplot()+
  geom_line(aes(x = edad_cat , y = value, colour = name))




