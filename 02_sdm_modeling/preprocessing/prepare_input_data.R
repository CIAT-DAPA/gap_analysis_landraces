#### FUNCTION TO PREPARE THE MAIN INPUT FILE ####
# AUTHOR: ANDRES CAMILO MENDEZ ALZATE
###############################################

Occ <- read.csv('Z:/gap_analysis_landraces/runs/input_data/by_crop/rice_african/lvl_1/classification/rice_african_lvl_1_bd.csv')
names(df)[4] <- "Latitude"


prepare_input_data <- function(file_path, file_output ){
  
  cat("Importing data base /n")
  msk <- raster(mask)
  
  Occ <- read.csv(file_path, header  = TRUE)

  Occ <- Occ  %>% dplyr::select(., "Longitude", "Latitude", one_of(c("y", "ensemble")))
  
  cat("Removing duplicated coordinates /n")
  
  #remove repeated coordinates
  rep <- which(duplicated( raster::extract(msk, Occ[, c("Longitude", "Latitude")], cellnumbers = TRUE)  ))
  Occ  <- Occ[-rep, ]
  
  
  names(Occ) <- c("Longitude", "Latitude", "ensemble")
  #Occ$ensemble <- tolower(Occ$ensemble)
  Occ <- Occ[which(Occ$ensemble == occName),]
  
  Occ <- Occ[which(!is.na(raster::extract(x = msk, y = Occ[,c("Longitude", "Latitude")]))),]
  coordinates(Occ) <- ~Longitude+Latitude
  crs(Occ)  <- crs(msk)
  #Occ@bbox <- matrix(raster::extent(msk), ncol = 2, byrow = T)
  cat("Saving occurrences \n")
  shapefile(Occ, file_output, overwrite = TRUE)
  #writeOGR(Occ, paste0(occDir,"/Occ.shp"), "Occ", driver="ESRI Shapefile", overwrite_layer=TRUE)
  
  cat(">>> Total number of occurrences:", nrow(Occ), " \n")
  

  
}






