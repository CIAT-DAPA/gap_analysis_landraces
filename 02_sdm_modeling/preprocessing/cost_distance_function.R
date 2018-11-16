
cost_dist_function <-  function( outDir, friction, classResults, occName, mask, occDir, filename, arcgis = FALSE, code ){
  
 

  if(!file.exists(paste0(outDir, "/cost_dist.tif"))){
   
     msk <- raster(mask)
    
    cat("Creating occurrences shapefile"," \n ")
    Occ <- read.csv(paste0(classResults, "/", filename), header = T)
 #   Occ <- Occ  %>% dplyr::select(., contains("long"), contains("lat"), one_of(c("y", "ensemble")))
    Occ <- Occ  %>% dplyr::select(., "Longitude", "Latitude",one_of(c("y", "ensemble")))
    names(Occ) <- c("Longitude", "Latitude", "ensemble")
    Occ$ensemble <- tolower(Occ$ensemble)
    Occ <- Occ[which(Occ$ensemble == occName),]
    Occ <- Occ[which(!is.na(raster::extract(x = msk, y = Occ[,c("Longitude", "Latitude")]))),]
    coordinates(Occ) <- ~Longitude+Latitude
    crs(Occ)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    Occ@bbox <- matrix(raster::extent(msk), ncol = 2, byrow = T)
    cat("Saving occurrences \n")
    writeOGR(Occ, paste0(occDir,"/Occ.shp"), "Occ", driver="ESRI Shapefile", overwrite_layer=TRUE)
    
    
  if(arcgis){
    
    sink(code)
    cat('import arcpy', fill = T)
    cat('from arcpy import env', fill = T)
    cat('from arcpy.sa import *', fill = T)
    cat(paste0('arcpy.env.mask = ', '"', mask, '"'), fill = T)
    cat(paste0('arcpy.env.extent = ', '"', mask, '"'), fill = T)
    cat(paste0('arcpy.env.snapRaster = ', '"', mask, '"'), fill = T)
    cat(paste0('arcpy.env.cellSize = ', '"', mask, '"'), fill = T)
    cat(paste0('arcpy.env.extent = ', '"', 'MAXOF', '"'), fill = T)
    #cat(paste0('env.workspace = ', '"', outDir, '"'), fill = T)
    cat(paste0('friction = arcpy.Raster(', '"', friction, '"',')'), fill = T)
    cat(paste0('shp = arcpy.FeatureSet(', '"', paste0(occDir, "/Occ.shp"), '"',')'), fill = T)
    cat('arcpy.CheckOutExtension("Spatial")', fill = T)
    cat('outCostDistance = CostDistance(shp, friction)', fill = T)
    cat(paste0('outCostDistance.save(', '"', paste0(outDir, "/cost_dist.tif"), '"',')'), fill = T)
    sink()
    
    shell(code)# system2(paste0('python ', code));# shell.exec(code)
    cost_dist <- raster(paste0(outDir, "/cost_dist.tif"))
    cost_dist <- raster::crop(x = cost_dist, y = raster::extent(msk))
    
  }  else{
  
    cat("Calculating cost distance raster \n")
    #p <- shapefile(paste0(occDir, "/Occ.shp"))
    r <- raster(friction ) %>% raster::crop( x =., y = extent(Occ)  )
    t <- transition(r, function(x) 1/mean(x), 8) 
    t <- geoCorrection(t) 
    
    cost_dist <- accCost(t, Occ) 
    cost_dist[which(cost_dist[] == Inf)] <- NA
    rm(t); gc() 
    
  }
    
    raster::writeRaster(cost_dist, filename = paste0(outDir, "/cost_dist.tif"), overwrite= T)

    
  } else {
    cat("Cost distance already created, importing it ... \n")
    cost_dist <- raster(paste0(outDir, "/cost_dist.tif"))
    
  }
  
  return(cost_dist)
  cat('Done... \n')
}


# 
