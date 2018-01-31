####  IMPORT PACKAGES
require( raster)
require(rgdal)
require(rasterVis)
require(rgeos)
require(deldir)#delanuay and voronoi
require(sp)
require(tidyverse)



cost <- raster("Z:/Input_data/_friction_surface/cost_distances/prueba_cost4OSM.tif")
dgp1 <- raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/dist_to_GP1.tif") 



delanuy <- shapefile("Z:/Results/accession_density/Andean/delaunay_triang.shp")

ocur <- readRDS("Z:/Input_data/_datosAndres/acp/data4modeling.RDS" )
ocur <- ocur %>% dplyr::select(., Longitude, Latitude, Analysis) %>% filter(., Analysis == "Americas") %>% select(., 1:2)
am_shp <- shapefile("Z:/Input_data/_datosAndres/americas_shape.shp")

sp_ocur <- SpatialPoints(ocur, proj4string = crs(wmask))

ov <- sp::over(sp_ocur, am_shp)
am_ocur <- ocur[which(!is.na(ov$NAME)), ]
am_ocur <- SpatialPoints(am_ocur, proj4string = crs(wmask) )

plot(nerda)

delanuy@data




crs(delanuy) <- crs(wmask)
delanuy$area <- area(delanuy)/1000000

summary(delanuy$area)

stk <- raster::stack(cost, delanuy) 

delanuy <- delanuy[delanuy$area >= 0.9,]



cost_sdm <- raster::mask(x = cost, mask = delanuy[delanuy$ID == 1038,] )
dist_sdm <- raster::mask(x = dgp1, mask = delanuy[delanuy$ID == 1038,] )

stk <- raster::stack(lagran, dist)
rvl <- raster::rasterToPoints(x = stk)
rvl

### Script to make a delanuay triangulation
crds <- am_ocur@coords
dup <- duplicated(crds)
crds <- crds[!dup, ]


dela <- deldir(crds[,1], crds[,2] )
w <- triang.list(dela)
polys <- vector(mode='list', length=length(w))
centroids<- matrix(0,nrow = length(w) , ncol = 2)

for (i in seq(along=polys)) {
  pcrds = cbind(w[[i]]$x, w[[i]]$y)
  pcrds = rbind(pcrds, pcrds[1,])
  polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  centroids[i,] <- rbind(gCentroid(SpatialPolygons(list(polys[[i]]), proj4string = crs(wmask)) )@coords)
  
  
}
SP <- SpatialPolygons(polys, proj4string = crs(wmask))

delanuay <- SpatialPolygonsDataFrame(SP, data=data.frame( ID=sapply(slot(SP, 'polygons'), 
                                                                    function(x) slot(x, 'ID')), centroid_x= centroids[,1], 
                                                          centroid_y=centroids[,2] , 'area'  = area(SP)/1000000) )

writeOGR(delanuay,"Z:/Input_data/_datosAndres/delanuay", driver = "ESRI Shapefile", layer="delanuay_andres" )

#End delanuay triangulation

#Check and fix some errors
df <- delanuay@data
plot(delanuay[delanuay$ID == 958,])


### ACP and Clustering proposal



