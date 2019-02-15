##### ANDRES CAMILO MENDEZ
##### FUNCTION TO CREATE ALL DELANUAY  TRIANGULATIONS FOR ALL OCCURRENCES


validation_metrics <- function(n.sample = 100, bf_rad = 50, baseDir, area, group, crop, lvl, pnt = NULL, ncores = NULL, dens.level = "high_density" ,filename ){
  
  cat(
    "    oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                 `..`               `.....`        `..`         `...              `N       
    N`                 omMh-            -sdddmddd/      `oMNd/       .sNNd.             `N       
    N`                /mooms.         `omh:`   .-.      `sNyhd:     `omsdm-             `N       
    N`               -dy.`sN+`        /mh-              `sNo:dh-   `+mo-dm-             `N       
    N`              `yd-  `hm:        +ms`              `sN+ /mh. `/ms`-dm-             `N       
    N`              sNmddddmMh-       /my-              `sN+ `+Ns`:dy` -dm-             `N       
    N`             /my:-----sNs.      `sNy:`   `-.      `sN+  .sNhdh.  -dm-             `N       
    N`            -dd-      `yN+`      `/yddddddh/      `om+   .yMd-   .dd.             `N       
    N`            `.`        `..          `.....`        `.`    `..     ..              `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++N       
    \n \n")
  
  if(is.null(pnt)){stop("You should set a value for pnt. E.j ('pnt1' or 'pnt2'... )")}
  if(bf_rad >= 101){stop("Buffer radius must be less than 100 km")}
  
  
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
  
  #buffer_prime <- buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  crs(SDM)), width=100000)
  #occur <- shapefile(occDir)
  width =  bf_rad*1000
  buff_50 <- raster::buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  coord_sys), width = width)
  
  scr <- raster::extract(gp_m, buff_50)
  scr <- unlist(scr)
  scr <- scr[complete.cases(scr)]
  
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
  
  n_cords <- base::sample(nrow(cords_dummy), n.sample, replace = F  )
  
  if(!is.null(ncores)){
    
    cl <- makeSOCKcluster(ncores)
    registerDoSNOW(cl)
    
    pb <- tkProgressBar(max=n.sample)
    progress <- function(n) setTkProgressBar(pb, n)
    opts <- list(progress=progress)
    
    cat(paste(">>> Initializing process to calculate the performance parameters for the score in:", pnt, "whit a buffer of", bf_rad, "Km \n \n "))
    results <- foreach( i = 1:n.sample, .combine = "rbind", .packages = c("raster", "pROC", "dplyr", "sdm"), .options.snow=opts)  %dopar% {
      
      #library(dplyr)
      cord <- cords_dummy[n_cords[i], ]
      width = bf_rad*1000
      buf_cord <- raster::buffer(SpatialPoints(cord, proj4string =  coord_sys), width=width)
      #dummy_buff <- buffer(buf_cord, width = bf_rad)
      
      #cat(paste("Extrayendo datos del buffer(it can take a long time): " ,i, "\n \n"))
      
      no_gap <- raster::extract(gp_m2, buf_cord)
      no_gap <- unlist(no_gap)
      no_gap <- no_gap[complete.cases(no_gap)]
      
      if(length(no_gap)!=0 ){
        ng <- data.frame( score = no_gap, observe = rep(0, length(no_gap) ))
        gap <- data.frame(score = scr, observe = rep(1, length(scr)))
        
        #remove cero values
        bd <- dplyr::bind_rows(ng, gap, .id = NULL) %>% dplyr::filter(., score != 0) %>% round(., 3)
        
        rm(ng, gap)
        
        if(length(levels(factor(bd$observe))) > 1){
          #val <- pROC::roc(response = factor(bd$observe), predictor = bd$score )
          sm <- sdm::evaluates(factor(bd$observe), bd$score )
          
          auc <- sm@statistics$AUC
          m <- which(sm@threshold_based$criteria == "minROCdist")
          value <- sm@threshold_based$threshold[m]
          se <- sm@threshold_based$sensitivity[m]
          es <- sm@threshold_based$specificity[m]
          tss <- sm@threshold_based$TSS[m] 
          
        }else{
          value <- NA
          auc <- NA
          se <- NA
          es <- NA
          tss <- NA
        }
       
      }else{
        
        value <- NA
        auc <- NA
        se <- NA
        es <- NA
        tss <- NA
      }
      return(c(value, auc, se, es, tss))
      
      
    }
    stopCluster(cl)
  }else{
    
    cat(paste(">>> Initializing process to calculate the performance parameters for the score in:", pnt, "whit a buffer of", bf_rad, "Km \n \n "))
    
    results <- lapply(1:n.sample, function(i){
      
      cord <- cords_dummy[n_cords[i], ]
      width = bf_rad*1000
      buf_cord <- raster::buffer(SpatialPoints(cord, proj4string =  coord_sys), width=width)
      #dummy_buff <- buffer(buf_cord, width = bf_rad)
      

      no_gap <- raster::extract(gp_m2, buf_cord)
      no_gap <- unlist(no_gap)
      no_gap <- no_gap[complete.cases(no_gap)]
      
      if(length(no_gap)!=0){
        
        ng <- data.frame( score = no_gap, observe = rep(0, length(no_gap) ))
        gap <- data.frame(score = scr, observe = rep(1, length(scr)))
        
        bd <- dplyr::bind_rows(ng, gap, .id = NULL)
        if(length(levels(factor(bd$observe))) > 1){
          #val <- pROC::roc(response = factor(bd$observe), predictor = bd$score )
          sm <- sdm::evaluates(factor(bd$observe), bd$score )
          
          auc <- sm@statistics$AUC
          m <- which(sm@threshold_based$criteria == "minROCdist")
          value <- sm@threshold_based$threshold[m]
          se <- sm@threshold_based$sensitivity[m]
          es <- sm@threshold_based$specificity[m]
          tss <- sm@threshold_based$TSS[m] 
          
        }else{
          value <- NA
          auc <- NA
          se <- NA
          es <- NA
          tss <- NA
          
        }
       
      }else{
        
        value <- NA
        auc <- NA
        se <- NA
        es <- NA
        tss <- NA
      }
      return(c(value, auc, se, es, tss))
      
      
    })
    
    
    results <- do.call(rbind, results)
    
  }#  end if  
  
  results <- as_tibble(results)
  names(results) <- c("score", "auc", "se", "es", "tss")
  
  #cat(paste(">>> Saving results in:",paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"), "\n" ))
  cat(">>>Process finished :) \n")
  #saveRDS(results, file = paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"))
  return(results)
}# end function



