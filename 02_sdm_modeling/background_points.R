# Create sample (background, occurrences, swd) for SDMs
# Chrystiam Sosa, Julian Ramirez-Villegas, Harold Achicanoy, Andres camilo mendez
# CIAT, Nov 2018

# Creating Native area filtered by EcoRegions raster file
NAFiltered <- function(crop = "potato", occName = "ajanhuiri"){
  
  outFile <- paste0(input_data_dir, "/by_crop/", crop, "/native_area/", occName, "/native_area_", occName, ".tif")
  
  if(!file.exists(outFile)){
    
    # Load occurrence and ecoregions data
    occ <- raster::shapefile(paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/", region, "/occurrences/Occ.shp"))
    elu <- raster::raster(paste0(input_data_dir, "/ecosystems/globalelu/World_ELU_2015_5km.tif"))
    
    # Extract specific ecoregions using occurrence data
    regions <- raster::extract(x = elu, y = occ@coords); regions <- sort(unique(regions)) # print(regions)
    elu[!(elu[] %in% regions)] <- NA # Exclude ecoregions that are not in occurrence data
    
    # Load country level native area
    # Created independently from GRIN
    nac <- raster::shapefile(paste0(input_data_dir, "/by_crop/", crop, "/native_area/", occName, "/native_area_", occName, ".shp"))
    
    # Crop native area by extracted regions
    elu_crpd <- raster::crop(elu, raster::extent(nac))
    elu_crpd <- raster::mask(x = elu_crpd, mask = nac)
    elu_crpd[!is.na(elu_crpd[])] <- 1
    raster::writeRaster(elu_crpd, outFile)
    
  } else {
    
    occ <- raster::shapefile(paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/", region, "/occurrences/Occ.shp"))
    elu_crpd <- raster::raster(outFile)
    
  }
  
  cat(paste0("Number of unique accessions: ", nrow(unique(occ@coords)), "\n"))
  cat(paste0("Number of pixels on native area filtered: ", sum(!is.na(elu_crpd[])), "\n"))
  if(sum(!is.na(elu_crpd[])) > 10*nrow(unique(occ@coords))){cat("Defined Area able to generate enough number of pseudo-absences\n")}
  return(elu_crpd)
}

# Profiling function
OCSVMprofiling2 <- function(xy, varstack, nu = 0.5, rm_bg_region = rm_bg_region){
  
  
  if(!is.null(rm_bg_region)){
    cat("Removing pseudo-absences in countries (ISO_3):", rm_bg_region, "\n" )
    shp_toremove <- raster::shapefile(paste0(input_data_dir,"/shapefiles/new_world_shapefile/ne_50m_admin_0_countries.shp"))
    shp_toremove <- shp_toremove[shp_toremove@data$ISO_A3 %in% toupper(rm_bg_region), ] 
    
    msk <- raster::mask(varstack[[1]], shp_toremove, inverse = TRUE)
    rm(shp_toremove)
  }else{
    msk <- varstack[[1]]
  }

  coo <- raster::as.data.frame( msk, xy = T) %>% 
    drop_na() %>% 
    dplyr::select(1:2)
  
  bioclim    <- varstack
  mat        <- cbind(xy, rep(1, nrow(xy)))
  mat        <- as.data.frame(cbind(1, raster::extract(bioclim, mat[,1:2])))
  
  mod        <- e1071::svm(mat[, -1], y = NULL, type = "one-classification", nu = nu)
  proj       <- as.data.frame(cbind(1, raster::extract(bioclim, cbind(coo))))
  pre        <- predict(mod, proj[, -1])
  absence    <- coo[(which(pre == 0)),]
  presence   <- coo[(which(pre != 0)),]
  
  return(list(Absences = absence, Presences = presence))
}

# Pseudo-absences function
pseudoAbsences2 <- function(xy, background, exclusion.buffer = 0.0166, tms = 10, coord.sys = crs(mask)){
  
  presences <- sp::SpatialPoints(xy)
  crs(presences) <- coord.sys
  spol <- rgeos::gBuffer(presences, width = exclusion.buffer)
  
  
  coords.l  <- background
  coords    <- coords.l
  sp.coords <- sp::SpatialPoints(coords)
  crs(sp.coords) <- coord.sys
  a         <- sp::over(sp.coords, spol)
  abs.bg    <- coords[which(is.na(a)), 1:2]
  set.seed(1234)
  if(nrow(abs.bg) >  tms * nrow(xy)){
   aa <- abs.bg[sample(1:nrow(abs.bg), size = tms * nrow(xy)),]
  }else{
   aa <- abs.bg
  }
  
  colnames(aa) <- c("Longitude", "Latitude")
  aa$Status <- 0
  xy$Status <- 1
  ab <- rbind(xy, aa); rownames(ab) <- 1:nrow(ab)
  
  return(ab)
}

# Pseudo-absences options: pa_method
# "ntv_area_ecoreg": Native Area cropped by EcoRegion
# "ecoreg": EcoRegion
# "all_area": Full extent of mask
pseudoAbsences_generator <- function(file_path, clsModel, overwrite = F, correlation = 0, pa_method = "ntv_area_ecoreg", rm_bg_region = NULL ){
  
 
  
  cat("Loading raster files","\n")
  current_clim_layer_generic <- lapply(list.files(climDir, pattern =  ".tif$", full.names = T), raster)
  current_clim_layer_generic <- lapply(current_clim_layer_generic , function(aa){names(aa) <- gsub( ".tif$", "", basename(aa@file@name)); return(aa)})
  current_clim_layer_sp      <- lapply(list.files(paste0(baseDir, "/input_data/by_crop/", crop, "/raster/", region), pattern = ".tif$", full.names = T), raster)
  current_clim_layer_sp      <- lapply(current_clim_layer_sp , function(aa){names(aa) <- gsub(".tif$", "", basename(aa@file@name)); return(aa)})
  all_clim_layer             <- stack(c(current_clim_layer_generic, current_clim_layer_sp))
  
  # remove undesirable layers
  layers_in <- names(all_clim_layer)[ !tolower(names(all_clim_layer)) %in% c("yield", "production", "harvested.area", "ethnicity", "monthCountByTemp10") ]
  current_clim_layer <- all_clim_layer[[layers_in]]
  
  # Background samples file name
  outBackName         <- paste0(backDir, "/bg_", occName, ".csv")
  outOccName          <- paste0(occDir, "/occ_", occName, ".csv")
  outSWDName          <- paste0(swdDir, "/swd_", occName, ".csv")
  outSWDComplete_Name <- paste0(swdDir, "/swd_Complete_", occName, ".csv")
  
  # Create background if it does not exist
  if (!file.exists(outBackName) | overwrite){
    mask <- raster::raster(mask)
    
    cat("Processing:", paste(occName), "\n")
    db_path <- paste0(file_path, "/", crop, "_lvl_1_bd.csv")
    spData            <- read.csv(db_path, header = T, stringsAsFactors = F)
    spData$ID <- 1:nrow(spData)
    #spData[,clsModel] <- tolower(spData[,clsModel])
    spData            <- spData[which(spData[,clsModel]== occName),]
    
    #remove H accessions
    if("status" %in% names(spData)){
      spData$status <- NULL
    }
    
    #remove repeated coordinates
    #rep <- which(duplicated( raster::extract(mask, spData[, c("Longitude", "Latitude")], cellnumbers = TRUE)  ))
    #spData <- spData[-rep, ]
    
    spData$cellID <-NA
    spData$cellID <-raster::extract(mask,SpatialPoints(cbind(spData$Longitude, spData$Latitude)),cellnumbers=TRUE) 
    spData <-spData[!duplicated(spData$cellID),- which(names(spData) %in% c("cellID"))]
    
    #add column to identifiers bd and fill it based on valid occurrences
    ids_path <- paste0(file_path, "/", crop, "_bd_identifiers.csv")
    if(file.exists(ids_path)){
      ids_db <- read.csv(ids_path, header = T, stringsAsFactors = F)
      
      if(is.null(ids_db$used)){
        ids_db$used <- FALSE
      }
      ids_db$used[spData$ID] <- TRUE
      
      write.csv(ids_db, ids_path, row.names = F)
      rm(ids_db)
    }
    
    spData$ID <- NULL
    
    cat("Creating random Pseudo-absences points using: ", pa_method, "method \n")
    
    if(pa_method == "ntv_area_ecoreg"){
      
      cat("Load native area restricted by ecoregions\n")
      if(!file.exists(paste0(input_data_dir, "/by_crop/", crop, "/native_area/", occName, "/native_area_", occName,".tif"))){
        ntv_area <- NAFiltered(crop = crop, occName = occName)
      } else {
        ntv_area <- raster::raster(paste0(input_data_dir, "/by_crop/", crop, "/native_area/", occName, "/native_area_", occName,".tif"))
      }
      
      climLayers <- raster::crop(current_clim_layer, raster::extent(ntv_area))
      climLayers <- raster::mask(climLayers, ntv_area)
      
     
      
      unsuit_bg <- OCSVMprofiling2(xy = unique(spData[,c("Longitude","Latitude")]), varstack = climLayers, rm_bg_region = rm_bg_region)
      random_bg <- pseudoAbsences2(xy = unique(spData[,c("Longitude","Latitude")]), background = unsuit_bg$Absences, exclusion.buffer = 0.083*5, tms = 10, coord.sys = crs(current_clim_layer))
      
      bg_spPoints  <- SpatialPoints(coords = random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      proj4string(bg_spPoints)<- crs(mask)
      raster::shapefile(bg_spPoints, paste0(input_data_dir, "/by_crop/", crop, "/", level,"/", occName,"/" ,region, "/background/background_", occName, ".shp"), overwrite = TRUE)
      
      nSamples <- nrow(random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      cat(nSamples, "pseudo-absences generated for n =", nrow(unique(spData[,c("Longitude","Latitude")])), "presences\n")
      
      xranSample <- random_bg[random_bg$Status == 0, c("Longitude","Latitude")]
      colnames(xranSample) <- c("lon","lat")
    }
    if(pa_method == "ecoreg"){
      
      elu     <- raster::raster(paste0(input_data_dir, "/ecosystems/globalelu/World_ELU_2015_5km.tif"))
      regions <- raster::extract(x = elu, y = unique(spData[,c("Longitude","Latitude")])); regions <- sort(unique(regions))
      elu[!(elu[] %in% regions)] <- NA # Exclude ecoregions that are not in occurrence data
      elu     <- raster::crop(elu, raster::extent(mask))
      elu     <- raster::mask(elu, mask)
      
      climLayers <- raster::crop(current_clim_layer, elu)
      climLayers <- raster::mask(climLayers, elu)
      
      
      
      unsuit_bg <- OCSVMprofiling2(xy = unique(spData[,c("Longitude","Latitude")]), varstack = climLayers, rm_bg_region = rm_bg_region)
      random_bg <- pseudoAbsences2(xy = unique(spData[,c("Longitude","Latitude")]), background = unsuit_bg$Absences, exclusion.buffer = 0.083*5, tms = 10, coord.sys = crs(current_clim_layer))
      
      bg_spPoints  <- SpatialPoints(coords = random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      proj4string(bg_spPoints)<- crs(mask)
      raster::shapefile(bg_spPoints, paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/",region,"/background/background_", occName, ".shp"), overwrite = TRUE)
      
      nSamples <- nrow(random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      cat(nSamples, "pseudo-absences generated for n =", nrow(unique(spData[,c("Longitude","Latitude")])), "presences\n")
      
      xranSample <- random_bg[random_bg$Status == 0, c("Longitude","Latitude")]
      colnames(xranSample) <- c("lon","lat")
    }
    if(pa_method == "all_area"){
      
      climLayers <- current_clim_layer
     
      
      unsuit_bg <- OCSVMprofiling2(xy = unique(spData[,c("Longitude","Latitude")]), varstack = climLayers, rm_bg_region = rm_bg_region)
      random_bg <- pseudoAbsences2(xy = unique(spData[,c("Longitude","Latitude")]), background = unsuit_bg$Absences, exclusion.buffer = 0.083*5, tms = 10, coord.sys = crs(current_clim_layer))
      
      bg_spPoints  <- SpatialPoints(coords = random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      proj4string(bg_spPoints)<- crs(mask)
      raster::shapefile(bg_spPoints, paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName,"/",region, "/background/background_", occName, ".shp"), overwrite = TRUE)
      
      nSamples <- nrow(random_bg[random_bg$Status == 0, c("Longitude", "Latitude")])
      cat(nSamples, "pseudo-absences generated for n =", nrow(unique(spData[,c("Longitude","Latitude")])), "presences\n")
      
      xranSample <- random_bg[random_bg$Status == 0, c("Longitude","Latitude")]
      colnames(xranSample) <- c("lon","lat")
    }
    
    # Extract variable data
     
    ex_raster_env <- as.data.frame(raster::extract(all_clim_layer, xranSample))
    z             <- cbind(id = 1:nrow(xranSample), species = occName, status = 0, xranSample, ex_raster_env)
    z             <- z[complete.cases(z),]
    cat(nrow(z), "pseudo-absences ready to use\n")
    occ           <- z
    
    # Preparing samples
    occSample        <- unique(spData[,c("Longitude", "Latitude")])
    names(occSample) <- c("lon", "lat")
    occ_env_data     <- as.data.frame(raster::extract(all_clim_layer, occSample))
    occSample        <- cbind(id = 1:nrow(occSample), species = occName, status = 1, occSample, occ_env_data)
    occSample        <- occSample[complete.cases(occSample),]
    
    
    
    # Preparing swd
    swdSample_Complete <- rbind(occSample, z)
    swdSample          <- swdSample_Complete
    
    # Excluding correlated variables
    if(correlation == 0){
      cat("Ommiting the correlation approach\n")
      swdSample <- swdSample_Complete
    }
    
    # Using choose variables algorithms (Correlation, VIF, or PCA + VIF)
    if(correlation == 1){
      cat("Using Pearson correlation approach\n")
      descrCor       <- cor(swdSample[,-c(1:5)])
      highlyCorDescr <- caret::findCorrelation(descrCor, cutoff = .75)
      if("ethnicity" %in% (colnames(descrCor)[highlyCorDescr])){
        swdSample      <- swdSample[,!colnames(swdSample) %in% (colnames(descrCor)[highlyCorDescr])]
      }else{
        plus_eth <- c((colnames(descrCor)[highlyCorDescr]), "ethnicity")
        swdSample      <- swdSample[,!colnames(swdSample) %in% plus_eth ]
      }
     
    }
    
    if(correlation == 2){
      cat("Using VIF approach\n")
      descrCor       <- usdm::vifstep(swdSample[,-c(1:5)], th = 5)
      highlyCorDescr <- descrCor@excluded
      if("ethnicity" %in% highlyCorDescr){
        highlyCorDescr <- highlyCorDescr[-which(highlyCorDescr == "ethnicity")]
      }
      swdSample      <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
    }
    
    if(correlation == 3){
      cat("Using PCA + VIF approach","\n")
      z <- FactoMineR::PCA(X = swdSample[,-c(1:5)], ncp = 5, scale.unit = T, graph = F)
      # Selecting a number of components based on the cumulative ratio of variance which has more than 70%
      ncomp <- as.numeric(which(z$eig[,ncol(z$eig)] >= 70)[1])
      vars  <- rownames(z$var$cos2)[unlist(lapply(X = 1:nrow(z$var$cos2[,1:ncomp]), FUN = function(r){
        if(length(which(z$var$cos2[r,1:ncomp] >= .15)) > 0){ res <- T } else { res <- F }
        return(res)
      }))]
      descrCor       <- usdm::vifstep(swdSample[,vars], th = 10)
      highlyCorDescr <- descrCor@excluded
      if("ethnicity" %in% highlyCorDescr){
        highlyCorDescr <- highlyCorDescr[-which(highlyCorDescr == "ethnicity")]
      }
      swdSample      <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
    }
    
  
    
    
    cat("Saving csv files","\n")
    
    write.csv(swdSample_Complete, outSWDComplete_Name, quote = F, row.names = F)
    write.csv(occ, outBackName, quote = F, row.names = F)
    write.csv(occSample, outOccName, quote = F, row.names = F)
    write.csv(swdSample, outSWDName, quote = F, row.names = F)
    #save swd file in the input data auxiliar folder
    write.csv(swdSample, paste0(input_data_aux_dir, "/pseudo_abs_file_", occName, ".csv" ), quote = F, row.names = F)
    # save variables selected in a csv file 
    var_names <- colnames(swdSample)[!colnames(swdSample) %in% c("id", "species", "status", "lon", "lat")]
    write.csv(x = var_names, file = paste0(sp_Dir, "/sdm_variables_selected.csv"), row.names = F)
  } else {
    
    cat("PseudoAbsence file already created, importing it...\n")
    swdSample <- read.csv(outSWDName, header = T)
    
  }
  return(swdSample[,3:ncol(swdSample)])
}
