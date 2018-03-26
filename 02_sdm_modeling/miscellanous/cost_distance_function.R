
cost_dist_function <-  function(code, envDir, friction, outDir, classResults, occName, occDir, mask){
  
  suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
  suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(raster)}else{library(rgdal)})
  suppressMessages(if(!require(sp)){install.packages("sp");library(raster)}else{library(sp)})
  
  if(!file.exists(paste0(envDir,"/cost_dist.tif"))){
    
    mask <- raster(mask)
    
    Occ <- read.csv(paste0(classResults,"/","genepool_predicted.csv"),header=T)
    Occ <- Occ[,c("Longitude","Latitude","ensemble")]
    Occ$ensemble <- tolower(Occ$ensemble)
    Occ <- Occ[which(Occ$ensemble==occName),]
    coordinates(Occ) <- ~Longitude+Latitude
    crs(Occ)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    Occ@bbox <- matrix(raster::extent(mask), ncol = 2, byrow = T)
    writeOGR(Occ, paste0(envDir,"/Occ.shp"), "Occ", driver="ESRI Shapefile")
    
    sink(code)
    cat('import arcpy', fill = T)
    cat('from arcpy import env', fill = T)
    cat('from arcpy.sa import *', fill = T)
    cat('arcpy.env.mask = ', '"',mask, '"', fill = T)
    cat('arcpy.env.extent = ', '"',mask, '"', fill = T)
    # cat(paste0('arcpy.env.extent = ', '"', 'MAXOF', '"'), fill = T)
    cat('arcpy.env.snapRaster = ', '"',mask, '"', fill = T)
    cat('arcpy.env.cellSize = ', '"',mask, '"', fill = T)
    cat(paste0('friction = arcpy.Raster(', '"',friction, '"',')'), fill = T)
    cat(paste0('shp = arcpy.FeatureSet(', '"',paste0(occDir,"/Occ.shp"), '"',')'), fill = T)
    cat('arcpy.CheckOutExtension("Spatial")', fill = T) 
    cat('outCostDistance = CostDistance(shp, friction)', fill = T)
    cat(paste0('outCostDistance.save(', '"',paste0(envDir,"/cost_dist.tif"), '"',')'), fill = T)
    sink()
    
    shell(code)
    
  } else {
    cost_dist <- raster(paste0(envDir,"/cost_dist.tif"))
  }
  return(cost_dist)
  print('Done...')
  
}
