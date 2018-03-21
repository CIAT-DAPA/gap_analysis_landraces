# Carson's Voronoi polygons function
require(sp);require(raster)
x <- shapefile()
delaunaypolygons <- function(x) {
  require(deldir)
  require(sp)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2])
  #w <- tile.list(z) #for Voronoi
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
  #ax <- sapply(slot(SP, 'polygons'), function(x) slot(x, 'Polygons'))
  #ax <- sapply(ax, function(x) slot(x, 'coords'))
  #points(ax[[1]]@labpt[1], ax[[1]]@labpt[2],pch=20,col="red")
  #data.frame(x=crds[,1],y=crds[,2], row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID')))
  delaunay <- SpatialPolygonsDataFrame(SP, data=data_df)
  return(delaunay)
}
