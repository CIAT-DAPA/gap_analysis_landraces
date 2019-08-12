##### ANDRES CAMILO MENDEZ
##### FUNCTION TO CREATE ALL DELANUAY  TRIANGULATIONS FOR ALL OCCURRENCES


validation_metrics <- function(n.sample = 100, bf_rad = 50, baseDir, area, group, crop, lvl, pnt = NULL, dens.level = "high_density" ,filename ){
  
  
  
  if(is.null(pnt)){stop("You should set a value for pnt. E.j ('pnt1' or 'pnt2'... )")}
  
  
  
  coreDir <-  paste0("/", crop, "/", lvl, "/", group, "/", area)
  validationDir <-  paste0(coreDir, "/gap_validation/buffer_100km/",dens.level,"/",pnt)
  results_dir <- paste0(baseDir, "/results")
  
  #Setting directories path 
  
  sdmDir <- paste0(results_dir, validationDir, "/02_sdm_results/prj_models/",group ,"_prj_median.tif")
  
  outDir <- paste0(results_dir, validationDir,"/03_gap_models")
  
  delaDir <- paste0(results_dir, validationDir,"/03_gap_models")
  
  occDir <- paste0(baseDir,"/input_data/by_crop", coreDir, "/occurrences/Occ")
  
  
  
  cat(">>> Initializing validation process  \n \n")
  # gap score
  
  gp_m <- raster(paste0(outDir, "/", filename)) 
  SDM <- raster(sdmDir)
  coord_sys <- crs(SDM)
  rm(SDM); removeTmpFiles()
  
  occ <- shapefile(paste0(results_dir, validationDir, "/01_selected_points/Occ.shp"))
  buff_omit <- raster(paste0(results_dir, validationDir,"/01_selected_points/buffer_radius_to_omit.tif"))
  
  gp_m2 <- raster::mask(gp_m, buff_omit, maskvalue = 1)
  
  
  if(!file.exists(paste0(results_dir, validationDir, "/01_selected_points/buffer_radius_to_omit_shp.shp") ) ){
    
   
    buff <- raster::crop(buff_omit, extent(occ))
    
    cat("Initializing process of find buffers centroid \n ")
    cat("Converting raster to polygons, this procces will take several minutes ... \n \n")
    
    buff_pol <- rasterToPolygons(buff, dissolve = TRUE)
    
    cent <- getSpPPolygonsLabptSlots(buff_pol)[2,]
    cat("Creating buffer with .shp format \n")
    
    
  }else{
    cat("Loading buffer ... \n")
    buff_prime <- shapefile(paste0(results_dir, validationDir,"/01_selected_points/buffer_radius_to_omit_shp.shp"))
    cent <- getSpPPolygonsLabptSlots(buff_prime)
    cent <- t(cent)
    cat("Buffer loaded :) \n \n")
  }
  # End find buffer centroid
  
  # importing kernel density raster to extrac the point in the higgest densities areas
  cat("Importing kernel density raster to extract points in the higgest densities areas \n \n")
  
  knl <- raster(paste0(outDir, "/kernel.tif")) 
  knl <- raster::mask(knl, buff_omit, maskvalue = 1)
  knl[which(knl[] == 0 )] <- NA
  knl.dens <-  raster::quantile(x = knl[], probs = c(.9, 1), na.rm = T)
  knl[knl[] <=   knl.dens[1] ] <- NA
  
  knl.dens_2 <- raster::quantile(x = knl[], probs = c(.8, .90), na.rm = T)
  
  knl[knl[] <= knl.dens_2[1]] <- NA
  knl[!is.na(knl[])] <- 1
  
  b_occr <- raster::as.data.frame(knl, xy=T)
  
  rm(knl, knl.dens, knl.dens_2); g <-gc(); rm(g)
  
  b_occr <- b_occr[complete.cases(b_occr),]
  cords_dummy <- b_occr[,1:2] 
  
  if(n.sample > nrow(cords_dummy)){cat("The number of n.sample is greater than the total coords in the selected density area \n")
    n.sample <- nrow(cords_dummy) - 10
  }
  
  
  #buffer_prime <- buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  crs(SDM)), width=100000)
  #occur <- shapefile(occDir)
  res_per_buff <- lapply(radius, function(bf_rad){
    
    if(bf_rad >= 101){stop("Buffer radius must be less than 100 km")}
    # creando el buffer de tamano radius para el buffer prime
    width =  bf_rad*1000
    buff_50 <- raster::buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  coord_sys), width = width)
    
    scr <- raster::extract(gp_m, buff_50)
    scr <- unlist(scr)
    scr <- scr[complete.cases(scr)]
    
    #genera las coordeanadas aleatorias en donde generar los buffers
    n_cords <- base::sample(nrow(cords_dummy), n.sample, replace = F  )
    
    cat(paste(">>> Calculating performance metrics for the gap score in:", pnt, "whit a buffer of", bf_rad, "Km \n "))
    
    
    width = bf_rad*1000
    
    #dummy_buff <- buffer(buf_cord, width = bf_rad)
    
    cord <- cords_dummy[n_cords, ]
    presences <- sp::SpatialPoints(cord)
    crs(presences) <- coord_sys
    all_buffs <- raster::buffer(presences, width=width, dissolve = F)
    
    no_gap_ls <- raster::extract(gp_m2, all_buffs)
    
    results <- lapply(no_gap_ls, function(no_gap){
      
      no_gap <- no_gap[!is.na(no_gap)]
      
      if(length(no_gap)!=0){
        
        ng <- data.frame( score = no_gap, observe = rep(0, length(no_gap) ))
        gap <- data.frame(score = scr, observe = rep(1, length(scr)))
        
        bd <- dplyr::bind_rows(ng, gap, .id = NULL)
        
        if(length(levels(factor(bd$observe))) > 1){
          #val <- pROC::roc(response = factor(bd$observe), predictor = bd$score )
          croc <- suppressMessages( pROC::roc(response = bd$observe, predictor = bd$score))
          croc_summ <- data.frame (sensi = croc$sensitivities, speci = croc$specificities, threshold =  croc$thresholds) %>% 
            round(., 3) %>% 
            dplyr::mutate(., max.TSS = sensi + speci - 1) %>% 
            dplyr::mutate(., minROCdist = sqrt((1- sensi)^2 + (speci -1)^2))
          
          max.tss <- croc_summ %>% dplyr::filter(., max.TSS == max(max.TSS)) %>% 
            dplyr::mutate(., method = rep("max(TSS)", nrow(.)))
          
          minRoc <- croc_summ %>% 
            dplyr::filter(., minROCdist == min(minROCdist))%>% 
            dplyr::mutate(., method = rep("minROCdist", nrow(.)))
          
          croc_summ <- rbind(max.tss, minRoc) %>% 
            dplyr::filter(., speci == max(speci))  %>% 
            dplyr::sample_n(., 1) %>%
            dplyr::mutate(auc = round(croc$auc,3)) %>%
            dplyr::select(threshold, auc, sensi, speci, max.TSS)
          
          
        }else{
          croc_summ <- data.frame(threshold = NA, auc = NA, sensi = NA, speci = NA, max.TSS = NA)
          
        }
        
      }else{
        
        croc_summ <- data.frame(threshold = NA, auc = NA, sensi = NA, speci = NA, max.TSS = NA)
        
      }
      
      return(croc_summ)
      
    })
    
    validation_results <- do.call(rbind, results) %>% as.data.frame()
    names(validation_results) <- c("score", "auc", "se", "es", "tss")
    return(validation_results)
  })
  
  names(res_per_buff) <- paste0(radius, "km")

  
   
  
  #cat(paste(">>> Saving results in:",paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"), "\n" ))
  cat(">>>Process finished :) \n")
  #saveRDS(results, file = paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"))
  return(res_per_buff)
}# end function



