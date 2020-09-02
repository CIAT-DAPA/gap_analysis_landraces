root_dir <- "Z:/gap_analysis_landraces/runs/results/"

crops_paths <- data.frame(pth = paste0(list.dirs(path = root_dir, full.names = T, recursive = F), "/lvl_1") ,
                          name = list.dirs(path = root_dir, full.names = F, recursive = F),
                          stringsAsFactors = F)

save.raster <- TRUE

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
    
    path <- paste0(as.character(i[1]), "/gap_richness_", i[2], ".tif")
    if(save.raster){
      writeRaster(rich, path, overwrite = T, NAflag = 0)
    }
    
    status <- data.frame(lower = covg_low, upper = covg_upp, status = "Done", stringsAsFactors = F)
    
    return(status)
  }, error = function(e){
    
    cat("An error ocurred :(  \n")
    status <- data.frame(lower= NA, upper = NA, status = "ERROR", stringsAsFactors = F)
    return(status)
  })
  
  return(cbind( name = i[c], x))
  
})


bind_rows(cov_mtrs) %>% 
  dplyr::mutate(name = crops_paths$name) %>% 
write.csv(., "Z:/gap_analysis_landraces/runs/LGA_documents/gaps_richness_per_crop/coverage_metrics.csv", row.names = F)



