#Julian Ramirez-Villegas, Chrystian Sosa
#March 2018

#major changes in revision of 27th March
#mask delaunay polygons to the SDM raster

#test function
#wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
#crop_name <- "common_bean"; level <- "1"; lv_name <- "mesoamerican"; region <- "americas"
msk <- raster::shapefile(paste(wd,"/input_data/shapefiles/GAUL_2014/CONTINENTAL_20km.shp",sep=""))

#dp_obj: delaunay polygons SpatialPolygonsDataFrame
#msk:    raster or shapefile file with which dp_obj will be cut
#wd:     working directory

mask_delaunay <- function(dp_obj, msk, wd){
  #load packages
  require(raster); require(rgeos)
  
  #directories
  res_dir <- paste(wd,"/results/",crop_name,"/lvl_",level,"/",lv_name,"/",region,sep="")
  del_dir <- paste(res_dir,"/gap_models/delaunay",sep="")
  
  #if mask is a raster, then convert it to shapefile and write it
  if (length(grep("Raster",class(msk)[1])) > 0) {
    if (!file.exists(paste(del_dir,"/mask.shp",sep=""))) {
      msk_pols <- rasterToPolygons(x=msk,n=8,na.rm=T,dissolve=T)
      proj4string(msk_pols) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      gwd <- getwd(); setwd(del_dir)
      writeOGR(obj=msk_pols, dsn=".", layer="mask", driver="ESRI Shapefile")
      setwd(gwd)
    } else {
      msk_pols <- raster::shapefile(paste(del_dir,"/mask.shp",sep=""))
    }
  } else {
    msk_pols <- msk #msk is a shapefile
  }
  
  #intersect SDM and delaunay
  dp_msk <- gIntersection(dp_obj, msk_pols, byid = T, drop_lower_td=T)
  p.df <- data.frame( ID=names(dp_msk));colnames(p.df) <- "ID"
  row.names(p.df) <- p.df$ID
  p <- SpatialPolygonsDataFrame(dp_msk, p.df)
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