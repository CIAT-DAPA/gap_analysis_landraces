
cost_dist_function <-  function(code, outDir, friction, classResults, occName, mask, occDir){
  
  suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
  pacman::p_load(raster, rgdal, sp)
  
  if(!file.exists(paste0(outDir, "/cost_dist.tif"))){
    
    Occ <- read.csv(paste0(classResults, "/genepool_predicted.csv"), header = T)
    Occ <- Occ[,c("Longitude", "Latitude", "ensemble")]
    Occ$ensemble <- tolower(Occ$ensemble)
    Occ <- Occ[which(Occ$ensemble==occName),]
    msk <- raster(mask)
    Occ <- Occ[which(!is.na(raster::extract(x = msk, y = Occ[,c("Longitude", "Latitude")]))),]
    coordinates(Occ) <- ~Longitude+Latitude
    crs(Occ)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    Occ@bbox <- matrix(raster::extent(msk), ncol = 2, byrow = T)
    writeOGR(Occ, paste0(occDir,"/Occ.shp"), "Occ", driver="ESRI Shapefile")
    
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
    raster::writeRaster(cost_dist, filename = paste0(outDir, "/cost_dist.tif"), format = "GTiff", overwrite = T)
    
  } else {
    
    cost_dist <- raster(paste0(outDir, "/cost_dist.tif"))
    
  }
  
  return(cost_dist)
  print('Done...')
  
}
