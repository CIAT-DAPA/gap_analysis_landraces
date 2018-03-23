
cost_dist_function <-  function(code, envDir, lyr, outDir,classResults,occName, mask){
  
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
    cat(paste0('arcpy.env.extent = ', '"', 'MAXOF', '"'), fill = T)
    cat(paste0('env.workspace = ', '"', envDir, '"'), fill = T)
    cat(paste0('friction = arcpy.Raster(', '"',friction, '"',')'), fill = T)
    cat(paste0('shp = arcpy.FeatureSet(', '"',paste0(envDir,"/Occ.shp"), '"',')'), fill = T)
    cat('arcpy.CheckOutExtension("Spatial")', fill = T) 
    cat('outCostDistance = CostDistance(shp, friction)', fill = T)
    cat(paste0('outCostDistance.save(', '"',paste0(envDir,"/cost_dist.tif"), '"',')'), fill = T)
    sink()
    
    shell(code)# system2(paste0('python ', code));# shell.exec(code)
    
  } else {
    cost_dist <- raster(paste0(envDir,"/cost_dist.tif"))
    
  }
  return(cost_dist)
  print('Done...')
  
}

# 
cost_dist_function(code = paste0(sp_Dir_input,"/","cost_dist.py"),
                   envDir = paste0(sp_Dir_input,"/","raster"),
                   lyr = friction,
                   outDir = paste0(sp_Dir_input,"/","raster"),
                   classResults = classResults,
                   occName = occName
)