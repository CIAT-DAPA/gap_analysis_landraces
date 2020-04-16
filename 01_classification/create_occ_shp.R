#### FUNCTION TO CREATE THE SHAPEFILE OF OCCURRENCES ####
# AUTHOR: ANDRES CAMILO MENDEZ ALZATE
###############################################


create_occ_shp <- function(file_path, file_output, validation ){
  
  cat("Importing data base \n")
  msk <- raster(mask)
  db_path <- paste0(file_path, "/", crop, "_", level, "_bd.csv")

  Occ <- read.csv(db_path, header  = TRUE, stringsAsFactors = F)
  Occ$ID <- 1:nrow(Occ)

  if("status" %in% names(Occ)){
    Occ <- Occ %>% dplyr::filter(., status == "G") %>% dplyr::select(., "Longitude", "Latitude", one_of(c("Y", "ensemble")), ID)  
  } else{
    Occ <- Occ  %>% dplyr::select(., "Longitude", "Latitude", one_of(c("Y", "ensemble")), ID)
  }
  
  
  
  names(Occ) <- c("Longitude", "Latitude", "ensemble", "ID")
  Occ$ensemble <- as.character(Occ$ensemble)
  Occ <- Occ[which(Occ$ensemble == occName),]
  
  cat("Removing coordiantes on the ocean/sea \n")
  Occ <- Occ[which(!is.na(raster::extract(x = msk, y = Occ[,c("Longitude", "Latitude")]))),]
  
  cat("Removing duplicated coordinates \n")
  
  #remove repeated coordinates
  #rep <- which(duplicated( raster::extract(msk, Occ[, c("Longitude", "Latitude")], cellnumbers = TRUE)  ))
  #if(length(rep) != 0){
   # Occ  <- Occ[-rep, ]
  #}
  
  #spData <- spData[-rep, ]
  
  Occ$cellID <-NA
  Occ$cellID <-raster::extract(msk,SpatialPoints(cbind(Occ$Longitude, Occ$Latitude)), cellnumbers=TRUE) 
  Occ <-Occ[!duplicated(Occ$cellID),-which(names(Occ) %in% c("cellID"))]
  
  
  #add column to identifiers bd and fill it based on valid occurrences
  ids_path <- paste0(file_path, "/", crop, "_bd_identifiers.csv")
  if(file.exists(ids_path) & !validation){
    ids_db <- read.csv(ids_path, header = T, stringsAsFactors = F)
    
    if(is.null(ids_db$used)){
      ids_db$used <- FALSE
    }
    ids_db$used[Occ$ID] <- TRUE
    
    write.csv(ids_db, ids_path, row.names = F)
    rm(ids_db)
  } 
  
Occ$ID <- NULL
  
  #save occurrences in csv format
  write.csv(Occ, paste0(occDir, "/Occ.csv"), row.names = FALSE)
  write.csv(Occ, paste0(input_data_aux_dir, "/Occ.csv"), row.names = FALSE)
  
  coordinates(Occ) <- ~Longitude+Latitude
  crs(Occ)  <- crs(msk)
  #Occ@bbox <- matrix(raster::extent(msk), ncol = 2, byrow = T)
  cat("Saving occurrences \n")
  shapefile(Occ, file_output, overwrite = TRUE)
  #save the same file but into the results folder
  if(!validation){
    shapefile(Occ, paste0(input_data_aux_dir, "/Occ.shp"), overwrite = TRUE)
    
  }
  
  
  #writeOGR(Occ, paste0(occDir,"/Occ.shp"), "Occ", driver="ESRI Shapefile", overwrite_layer=TRUE)
  
  cat(">>> Total number of occurrences:", nrow(Occ), " \n")
  

  
}






