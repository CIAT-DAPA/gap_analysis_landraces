#Create sample (background, occurrences, swd) for SDMs
#Chrystiam Sosa, Julian Ramirez-Villegas
#CIAT, Nov 2017

#function to create background, occurrence, and swd (bg+occ) samples
# pseudo_abs = c(1, 2) # 1: occurrence ecoregions masked by native area, 2: occurrence ecoregions for complete extent of mask
samples_create <- function(occFile, occName, backDir, occDir, swdDir, mask, climDir, clim_spDir, extension_r, var_names_generic, var_names_sp, overwrite = F, correlation = 0, pseudo_abs = 1){
  
  #load raster files
  cat("Loading raster files","\n")
  current_clim_layer_generic <- lapply(paste0(climDir, "/", var_names_generic, extension_r), raster)
  current_clim_layer_sp <- lapply(paste0(baseDir, "/input_data/by_crop/", crop, "/raster/", region, "/", var_names_sp, extension_r), raster)
  current_clim_layer <- stack(c(current_clim_layer_generic, current_clim_layer_sp))
  
  # background samples file name
  outBackName <- paste0(backDir, "/bg_", occName, ".csv")
  outOccName <- paste0(occDir, "/occ_", occName, ".csv")
  outSWDName <- paste0(swdDir, "/swd_", occName, ".csv")
  outSWDComplete_Name <- paste0(swdDir, "/swd_Complete_", occName, ".csv")
  #create background if it doesnt exist
  if (!file.exists(outBackName) | overwrite) {
    cat("Processing:", paste(occName), "\n")
    # spData <- readRDS(occFile)
    spData <- read.csv(occFile, header = T)
    spData[,clsModel] <- tolower(spData[,clsModel])
    spData <- spData[which(spData[,clsModel] == occName),]
    
    #create random points
    cat("Creating random points\n")
    
    # Load mask
    mask <- raster::raster(mask)
    
    if(pseudo_abs == 1){
      # Load native area restricted by ecoregions
      ntva <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/native_area_tuberosum_chilotanum.tif")
      
      climLayers <- raster::crop(current_clim_layer, raster::extent(ntva))
      climLayers <- raster::mask(climLayers, ntva)
      
      climLayers <- climLayers[[1:42]]
      saveRDS(climLayers, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/tuberosum_chilotanum/climLayers_cropped.rds")
      
      climLayers <- readRDS("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/native_area/tuberosum_chilotanum/climLayers_cropped.rds")
      
      unsuit_bg <- mopa::OCSVMprofiling(xy = unique(spData[,c("Longitude","Latitude")]), varstack = climLayers)
      random_bg <- mopa::pseudoAbsences(xy = unique(spData[,c("Longitude","Latitude")]), background = unsuit_bg$absence, exclusion.buffer = 0.083*5, prevalence = 0.05)
      random_bg_df <- as.data.frame(random_bg$species1$PA01)
      spPoints  <- SpatialPoints(coords = random_bg_df[random_bg_df$v == 0, c("x", "y")])
      proj4string(spPoints)<- CRS("+proj=longlat +datum=WGS84")
      
      raster::shapefile(spPoints, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/lvl_1/tuberosum_chilotanum/americas/background/bg_tuberosum_chilotanum.shp")
      
      nSamples <- nrow(random_bg_df[random_bg_df$v == 0, c("x", "y")])
      cat(nSamples, "pseudo-absences generated for n =", nrow(unique(spData[,c("Longitude","Latitude")])), "presences\n")
      
      xranSample <- random_bg_df[random_bg_df$v == 0, c("x", "y")]
      colnames(xranSample) <- c("lon","lat")
    }
    if(pseudo_abs == 2){
      
      climLayers <- raster::crop(current_clim_layer, raster::extent(ntva))
      climLayers <- raster::mask(climLayers, ntva)
      
      unsuit_bg <- mopa::OCSVMprofiling(xy = unique(spData[,c("Longitude","Latitude")]), varstack = climLayers)
      random_bg <- mopa::pseudoAbsences(xy = unique(spData[,c("Longitude","Latitude")]), background = unsuit_bg$absence, exclusion.buffer = 0.083*5, prevalence = 0.05)
      random_bg_df <- as.data.frame(random_bg$species1$PA01)
      spPoints  <- SpatialPoints(coords = random_bg_df[random_bg_df$v == 0, c("x", "y")])
      proj4string(spPoints)<- CRS("+proj=longlat +datum=WGS84")
      
      raster::shapefile(spPoints, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/potato/lvl_1/tuberosum_chilotanum/americas/background/bg_tuberosum_chilotanum.shp")
      
      nSamples <- nrow(random_bg_df[random_bg_df$v == 0, c("x", "y")])
      cat(nSamples, "pseudo-absences generated for n =", nrow(unique(spData[,c("Longitude","Latitude")])), "presences\n")
      
      xranSample <- random_bg_df[random_bg_df$v == 0, c("x", "y")]
      colnames(xranSample) <- c("lon","lat")
    }
    
    # Extract variable data
    ex_raster_env <- as.data.frame(raster::extract(current_clim_layer, xranSample))
    z <- cbind(id = 1:nrow(xranSample), species = occName, status = 0, xranSample, ex_raster_env)
    z <- z[complete.cases(z),]
    cat(nrow(z), "pseudo-absences ready to use\n")
    occ <- z
    #preparing samples
    occSample <- unique(spData[,c("Longitude", "Latitude")])
    names(occSample) <- c("lon", "lat")
    occ_env_data <- as.data.frame(raster::extract(current_clim_layer, occSample))
    occSample <- cbind(id = 1:nrow(occSample), species = occName, status = 1, occSample, occ_env_data)
    occSample <- occSample[complete.cases(occSample),]
    
    #prepare swd
    swdSample_Complete <- rbind(occSample, z)
    swdSample <- swdSample_Complete
    #excluiding correlation
    if(correlation == 0){
      cat("Ommiting the correlation approach\n")
      
      swdSample <- swdSample_Complete
      
    }
    
    #Using choose variables algorithms (Correlation, VIF, or PCA + VIF)
    if(correlation == 1){
      cat("Using Pearson correlation approach\n")
      
      descrCor <- cor(swdSample[,-c(1:5)])
      highlyCorDescr <- caret::findCorrelation(descrCor, cutoff = .75)
      swdSample <- swdSample[,!colnames(swdSample) %in% (colnames(descrCor)[highlyCorDescr])]
      
    }
    
    if(correlation == 2){
      cat("Using VIF approach\n")
      
      descrCor <- vifstep(swdSample[,-c(1:5)], th = 5)
      highlyCorDescr <- descrCor@excluded
      swdSample <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
      
    }
    
    if(correlation==3){
      cat("Using PCA + VIF approach","\n")
      
      z <- FactoMineR::PCA(X = swdSample[,-c(1:5)], ncp = 5, scale.unit = T, graph = F)
      # Selecting a number of components based on the cumulative ratio of variance which has more than 70%
      ncomp <- as.numeric(which(z$eig[,ncol(z$eig)] >= 70)[1])
      vars <- rownames(z$var$cos2)[unlist(lapply(X = 1:nrow(z$var$cos2[,1:ncomp]), FUN = function(r){
        if(length(which(z$var$cos2[r,1:ncomp] >= .15)) > 0){
          res <- T
        } else {
          res <- F
        }
        return(res)
      }))]
      #vars1 <- z$var$cor[,1] > 0.7 | z$var$cor[,1] < -0.7
      #vars2 <- z$var$cor[,2] > 0.7 | z$var$cor[,2] < -0.7
      #vars <- c(vars1, vars2)
      #vars <- names(vars[which(vars == TRUE)])
      
      descrCor <- usdm::vifstep(swdSample[,vars], th = 10)
      highlyCorDescr <- descrCor@excluded
      swdSample <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
    }
    cat("Saving csv files","\n")
    
    write.csv(swdSample_Complete, outSWDComplete_Name, quote = F, row.names = F)
    write.csv(occ, outBackName, quote = F, row.names = F)
    write.csv(occSample, outOccName, quote = F, row.names = F)
    write.csv(swdSample, outSWDName, quote = F, row.names = F)
    
  } else {
    
    cat("already processed\n")
    swdSample <- read.csv(outSWDName, header = T)
    
  }
  return(swdSample)
}
