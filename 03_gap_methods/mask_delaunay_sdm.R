delaunay_Sdm_function <-function(mask,delaunaypolygonsobj,model_outDir,cont_mask){
  require(raster);require(adehabitatHR);require(rgeos)
  
  sdm <- raster(paste0(model_outDir,"/",occName,"_prj_median_thr.tif"))
  sdm_points <- rasterToPoints(sdm,spatial = T)
  sdm_chull <- adehabitatHR::mcp(sdm_points,percent=100,unout ="km2")
  sdm_chull_RP <- rasterToPolygons(x=sdm,n=8,na.rm=T,dissolve=T)
  #cont_mask<-shapefile(cont_mask);gc()
  writeOGR(sdm_chull_RP,paste0(gap_del_outDir,"/","delaunay"),"sp", driver="ESRI Shapefile")
  
  sdm_fit <- gIntersection(delaunaypolygons_obj,sdm_chull_RP,byid = T,drop_lower_td=T)
  p.df <- data.frame( ID=names(sdm_fit));colnames(p.df) <- "ID"
  row.names(p.df) <- p.df$ID
  p <- SpatialPolygonsDataFrame(sdm_fit, p.df)
  p$ID2 <-  1:length(p)
  p$area <- geosphere::areaPolygon(x = p) / 1000000
  
  
  cent <- gCentroid(p,byid = TRUE)
  dist <- distanceFromPoints(sdm,cent); dist <- dist/1000
  dist <- dist*sdm
  
  del_ras <- rasterize(x=p,y=sdm,mask=FALSE,field="ID2",na.rm=T)
  #save files
  
  writeRaster(dist,paste0(gap_del_outDir,"/","dist_centroid_SDM.tif"))
  writeRaster(del_ras,paste0(gap_del_outDir,"/","polygon_SDM.tif"))
  
  
  writeOGR(p,paste0(gap_del_outDir,"/","delaunay"),"fitted_sdm", driver="ESRI Shapefile")
  
  plot(sdm_chull)

  test <- 1-(dist/max(dist[],na.rm=T))
  writeRaster(test,paste0(gap_del_outDir,"/","test_centroid_SDM.tif"))
  
}