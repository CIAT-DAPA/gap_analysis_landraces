#Julian Ramirez-Villegas
#March 2018

####
#based on Carson Farmer's Voronoi polygons function (http://carsonfarmer.com/2009/09/voronoi-polygons-with-r/), 
#changed to produce Delaunay triangles instead.
####
#Note: revision on 27th March removed Chrys code to intersect polygons
#      with continents. This is because the intersection is now made with
#      the SDM in the function mask_delaunay_sdm.R
####

#test function
#wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
#crop_name <- "common_bean"; level <- "1"; lv_name <- "mesoamerican"; region <- "americas"
#occ_dir <- paste(wd,"/input_data/by_crop/",crop_name,"/lvl_",level,"/",lv_name,"/",region,"/occurrences",sep="")
#sp_occ <- shapefile(paste0(occ_dir,"/Occ.shp"))
#outdir <- "~/nfs"
#msk <- raster::shapefile("~/nfs/delaunay/CONTINENTAL_20km.shp")
#dp_obj <- delaunaypolygons(sp_occ, outdir="~/nfs")

delaunaypolygons <- function(x, outdir) {
  #load required packages
  #if polygons do not exist then create them
  if (!file.exists(paste0(outdir,"/delaunay/raw_delaunay.shp"))){
    #create spatial polygons from triangulation
    if (.hasSlot(x, 'coords')) {crds <- x@coords} else {crds <- x}
    crds <- unique(crds)
    z <- deldir(crds[,1], crds[,2])
    w <- triang.list(z) #for Delaunay
    polys <- vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
      pcrds <- cbind(w[[i]]$x, w[[i]]$y)
      pcrds <- rbind(pcrds, pcrds[1,])
      polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    SP <- SpatialPolygons(polys)
    
    #create data.frame from SP with coordinates of all corners
    data_df <- data.frame()
    for (i in 1:length(SP)) {
      #i <- 1
      xcoords <- SP@polygons[[i]]@Polygons[[1]]@coords[,1]
      ycoords <- SP@polygons[[i]]@Polygons[[1]]@coords[,2]
      d_i <- data.frame(i,t(as.data.frame(xcoords)),t(as.data.frame(ycoords)))
      names(d_i) <- c("ID",paste("x",1:4,sep=""),paste("y",1:4,sep=""))
      row.names(d_i) <- 1
      data_df <- rbind(data_df,d_i)
    }
    row.names(data_df) <- sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))
    
    #make SpatialPointsDataFrame object with data file, and assign projection
    delaunay <- SpatialPolygonsDataFrame(SP, data=data_df)
    proj4string(delaunay) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    #write files
    gwd <- getwd(); setwd(paste(outdir,"/delaunay",sep=""))
    rgdal::writeOGR(obj=delaunay, dsn=".", layer="raw_delaunay", driver="ESRI Shapefile")
    setwd(gwd)
  } else {
    #load shapefile
    delaunay <- raster::shapefile(paste0(outdir,"/delaunay/raw_delaunay.shp"))
  }
  #return file
  return(delaunay)
}
