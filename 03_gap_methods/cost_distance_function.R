
cost_dist_function <-  function( outDir, friction, mask, occDir, arcgis = FALSE, code ){
  
 

  if(!file.exists(paste0(outDir, "/cost_dist.tif"))){
   
     msk <- raster(mask)
     Occ <- shapefile(paste0(occDir, "/Occ.shp"))
     
    
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
    r <- raster(friction ) %>% raster::crop( x =., y = extent(msk)  )
    t <- transition(r, function(x) 1/mean(x), 8) 
    t <- geoCorrection(t) 
    
    cost_dist <- accCost(t, Occ) 
    cost_dist[which(cost_dist[] == Inf)] <- NA
    
    #Normalize cost distance rater to be in 0-1 scale
    
    #mask cost distance raster with SDM raster
    sdm <- raster(paste0(model_outDir, "/", occName, "_prj_median.tif"))
    cost_dist <- raster::mask(cost_dist, sdm)
    #idenfy outliers using IQR 
    qls <- raster::quantile(cost_dist, na.rm = T)
    up_limit <- qls[4] + (1.5* (qls[4] - qls[2]))
    cost_dist <- cost_dist/up_limit
    cost_dist[which(cost_dist[] > 1 )] <- 1
    
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
