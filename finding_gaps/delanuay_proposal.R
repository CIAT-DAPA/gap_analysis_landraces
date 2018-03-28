
suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, rlang, fastcluster, sf ) 

wmask <- raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/mask_wb_c_ant.tif")

delanuay <- shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/delanuay/delanuay_andres.shp")
delanuay@bbox <- as.matrix(extent(wmask))


r <- raster()
extent(r) <- extent(wmask)
crs(r) <- crs(wmask)
res(r) <- res(wmask)
r[is.na(r[])]<- 0

del <- st_sf(dealnuay[1])
st_intersection(del, r)

cr <- crop( wmask , delanuay[1563,] )
rr <- mask(cr, delanuay[1563,])

xy <- SpatialPoints(delanuay@data[1563, 2:3])
crs(xy) <- crs(wmask)
dRas <- distanceFromPoints(rr, xy )

f_ras <-  dRas + rr
plot(f_ras)