library(tidyverse)
delaunaypolygons <- function(x,cont_mask){
  
  # Required libraries
  library(sf)
  library(sfdct)
  library(tidyverse)
  
  # Apply Delaunay triangulation
  xy_data <- sf::st_multipoint(x = x %>% as.matrix)
  xy_triangles <- ct_triangulate(xy_data)
  cat(">> Delaunay triangulation done.\n")
  
  # Transform object classes
  polys <- vector("list", length(xy_triangles))
  for (i in 1:length(xy_triangles)) {
    polys[[i]] <- Polygons(list(Polygon(xy_triangles[[i]][1])), ID=as.character(i))
  }
  xy_triangles <- sf::st_as_sf(SpatialPolygons(polys))
  rm(polys)
  cat(">> Spatial transformations done.\n")
  
  # Load countries edges
  # global_ctries <- sf::st_read(paste(baseDir,"/Input_data/_maps/GAUL_2014/CONTINENTAL.shp",sep=""))
  global_ctries <- sf::st_read(cont_mask)
  
  # Fix coordinates projection
  sf::st_crs(xy_triangles) <- sf::st_crs(global_ctries)
  
  # Intersect Delaunay results with countries edges
  system.time(expr = {delaunay <- sf::st_intersection(x = xy_triangles, y = global_ctries)})
  cat(">> Intersections done.\n")
  gap_del_outDir
  st_write(delaunay,paste0(gap_del_outDir,"/","delaunay.shp"), driver = "ESRI Shapefile")
  return(delaunay)
  
}
# delaupol <- delaunaypolygons(x = xy_data)
# delaupol <- delaupol %>% mutate(PolygonID = 1:nrow(delaupol))
# plot(delaupol[,"STATUS"])
# 
# library(velox)
# library(sf)
# 
# pr_map <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Results/SDM_modelling/Andean/prj_models/Andean_prj_mean.tif")
# vx <- velox(pr_map)
# 
# vx$rasterize(spdf = delaupol, field = 'PolygonID')
# 
# tst <- st_transform(delaupol, crs = crs)
# tst <- as(tst, 'Spatial')
# vx$rasterize(spdf = tst, field = 'PolygonID')
# rly <- vx$as.RasterLayer(band = 1)
# pnt <- vx$getCoordinates()
# pnt <- rename(pnt, lon = V1, lat = V2)
# 
# pnt <- raster::rasterToPoints()
# 
# 
# # Knowing the municipality for each pixel
# pnts <- vx$extract_points(sp = coords)
# pnts <- tbl_df(pnts)
# 
# 
# library(foreach)
# library(doParallel)
# cores <- detectCores()
# cl    <- makeCluster(cores[1]-1); rm(cores)
# registerDoParallel(cl)
# 
# extract_val <- function(i){
#   rst <- tmaptools::crop_shape(x = pr_map, y = delaupol[i,])
#   rst_val <- rst[] %>% na.omit %>% as.numeric
#   return(rst_val)
# }
# test <- foreach(i = 1:nrow(delaupol)) %dopar% {
#   extract_val(i)
# }
# 
# normalizePath("explore_and_classify_occurrence_data.R")
