#ANDRES CAMILO MENDEZ
#SCRIPT TO CROP ALL GENERIC AND SPECIFIC RASTERS


crop_raster <- function(mask, region){
  
  
  mask <- raster(mask)
  #crop rasters from by_crop folders
  
  files <- data.frame(fullnames = list.files(clim_spWorld, pattern = ".tif$", full.names = TRUE),
                names = list.files(clim_spWorld, pattern = ".tif$", full.names = FALSE))
  
  apply(files, 1,function(x){
    
  if(!file.exists(paste0(clim_spReg, "/", x[2]))) { 
    cat("Croping: ", x[2], "\n")
    x[1] %>% raster::raster(.) %>% raster::crop(x = ., y = extent(mask)) %>% raster::mask(x = ., mask = mask) %>%
      writeRaster(., paste0( clim_spReg, "/", x[2]), overwrite = TRUE)
  }
    
  })
   
  #crop rasters from generic_rasters
  clim_world <- paste0(input_data_dir, "/generic_rasters/world")
  files <- data.frame(fullnames = list.files(clim_world, pattern = ".tif$", full.names = TRUE),
  names = list.files(clim_world, pattern = ".tif$", full.names = FALSE))

  apply(files, 1,function(x){
    
    if(!file.exists(paste0(climDir, "/", x[2]))) { 
      cat("Croping: ", x[2], "\n")
      x[1] %>% raster::raster(.) %>% raster::crop(x = ., y = extent(mask)) %>% raster::mask(x = ., mask = mask) %>%
        writeRaster(., paste0( climDir, "/", x[2]), overwrite = TRUE)
    }
    
  })
  
  
}#end function