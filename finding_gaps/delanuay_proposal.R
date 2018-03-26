
suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, rlang, fastcluster, sf ) 

wmask <- raster("\\dapadfs\Workspace_cluster_9\gap_analysis_landraces\Input_data\mask_wb_c_ant.tif")

delanuay <- shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/delanuay/delanuay_andres.shp")
delanuay@bbox <- as.matrix(extent(wmask))


r <- raster()
extent(r) <- extent(wmask)
crs(r) <- crs(wmask)

del <- st_sf(dealnuay[1])
st_intersection(del, r)


cr <- crop( r , delanuay[200,] )
rr <- mask(cr, delanuay[1860,])