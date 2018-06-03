
######********FUCNTION TO CALCULATE A GAP SCORE USING DELAUNAYS TRIANGULATION*****************#################
######SCRIPT CREADO POR ANDRES CAMILO MENDEZ
######THIS SCRIPT COMES WHIT ABSOLUTLELY NO WARRANTY


suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, 
               deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, 
               rlang, fastcluster, sf, doParallel, rmapshaper, GISTools, doSNOW, tcltk, doParallel) 

calc_delaunay_score <- function(baseDir, area, group, crop, lvl, ncores = NULL, validation = FALSE, pnt = NULL, dens.level = "high_density"){
  
  cat(
    "        oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                 `..`               `.....`        `..`         `...              `N       
    N`                 omMh-            -sdddmddd/      `oMNd/       .sNNd.             `N       
    N`                /mooms.         `omh:`   .-.      `sNyhd:     `omsdm-             `N       
    N`               -dy.`sN+`        /mh-              `sNo:dh-   `+mo-dm-             `N       
    N`              `yd-  `hm:        +ms`              `sN+ /mh. `/ms`-dm-             `N       
    N`              sNmddddmMh-       /my-              `sN+ `+Ns`:dy` -dm-             `N       
    N`             /my:-----sNs.      `sNy:`   `-.      `sN+  .sNhdh.  -dm-             `N       
    N`            -dd-      `yN+`      `/yddddddh/      `om+   .yMd-   .dd.             `N       
    N`            `.`        `..          `.....`        `.`    `..     ..              `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++N       
    \n \n" )
  
  cat(">>> Generating Delaunays triangulation \n \n")
  
  if(validation == FALSE){
    cat(">>> Initializing process to calculte a gap score using Delaunays Triangulation \n \n ")
  } else {
    cat(">>> Initializing process to Validate a gap score using Delaunays Triangulation through +++ LOGCV +++ \n \n ")
  }
  
  if(validation == TRUE && is.null(pnt)){
    stop("If validation = TRUE then you should set a value for pnt. E.j ('pnt1' or 'pnt2'... )")
  }
  
  coreDir       <- paste0("/", crop, "/", lvl, "/", group, "/", area)
  validationDir <- paste0(coreDir, "/gap_validation/buffer_100km/", dens.level, "/", pnt)
  results_dir   <- paste0(baseDir, "/results")
  
  sdmDir <- switch(as.character(validation),
                   "FALSE" = paste0(results_dir, coreDir, "/prj_models", "/",group ,"_prj_median.tif"),
                   "TRUE"  = paste0(results_dir, validationDir, "/02_sdm_results/prj_models/",group ,"_prj_median.tif"))
  
  outDir <- switch(as.character(validation),
                   "FALSE" = paste0(results_dir,coreDir , "/gap_models" ),
                   "TRUE"  = paste0(results_dir, validationDir,"/03_gap_models"))
  
  delaDir <- switch(as.character(validation),
                    "FALSE" = paste0(results_dir,coreDir , "/gap_models"),
                    "TRUE"  =  paste0(results_dir, validationDir,"/03_gap_models"))
  
  occDir <- switch(as.character(validation),
                   "FALSE" = paste0(baseDir,"/input_data/by_crop", coreDir, "/occurrences/Occ"),
                   "TRUE"  = paste0(results_dir, validationDir,"/01_selected_points/Occ"))
  
  cat(">>> Importing Delaunays triangulation \n \n")
  
  if(file.exists(paste0(delaDir, "/delaunay/raw_delaunay.shp")) == FALSE){
    delaunaypolygons(x = shapefile(occDir) , outdir = delaDir)
    cat("Delaunay was successfully created \n \n")
  } else {
    cat("Delaunay is already created \n \n")
  }
  
  cat("Importing SDM raster \n")  
  SDM <- raster(sdmDir)
  
  SDM[!is.na(SDM[])] <- 1
  
  cat("Coverting SDM raster to Polygons \n \n ")
  sdm_pol <- raster::rasterToPolygons(SDM, n = 4, na.rm = T, dissolve = T)
  
  cat("Simplifying SDM Polygon \n \n ")
  sdm_pol_sf <- rmapshaper::ms_simplify(sdm_pol)
  rm(sdm_pol)
  
  delanuay <- shapefile(paste0(delaDir, "/delaunay/raw_delaunay.shp"))
  # Change path after
  delanuay@bbox <- as.matrix(extent(SDM))
  
  if(length(grep("area", names(delanuay@data))) == 0){
    delanuay$area <- raster::area(delanuay)/1000000
  }
  
  if( length(grep("centroid", names(delanuay@data))) == 0){
    
    centroids <- getSpPPolygonsLabptSlots(delanuay)
    delanuay$centroid.x <- centroids[,1]
    delanuay$centroid.y <- centroids[,2]
    
  }
  
  cat("Converting SDM Polygon to sf \n \n ")
  sf_dela <- st_as_sf(delanuay)
  sf_sdm <- st_as_sf(sdm_pol_sf)
  
  cat("Intersecting SDM and Delanuay Polygons \n \n ")
  
  i <- st_intersection(st_buffer(sf_dela, 0), st_buffer(sf_sdm, 0))
  rm(sf_dela, sf_sdm)
  
  cat("Removing Bad sf geometrys \n \n ")
  
  bad <- unlist(lapply(1:length(i$geometry), function(x){
    if(is(i$geometry[x]) == "sfc_GEOMETRYCOLLECTION"){
      return(x)
    } else {
      return(NA)
    }
  }))
  bad <-  bad[!is.na(bad)]
  if(length(bad) != 0){i$geometry[bad] <- NULL}
  
  cat("writing intercepted polygons to TempDir() \n \n ")
  
  write_sf(i$geometry,
           dsn = paste0(tempdir(), "/intersection.shp"),
           layer = "intercep",
           driver = "ESRI Shapefile",
           delete_layer = TRUE,
           delete_dsn = TRUE)
  rm(i, bad)
  
  cat("Calculating Max area \n \n ")
  i_shp <- shapefile(paste0(tempdir(), "/intersection.shp"))
  
  max.area <- max(raster::area(i_shp)/1000000) # al dividir por 1000000 la unidad es kilometro^2 
  
  cat(paste(">>> Max.area = ", max.area, "\n"))
  rm(i_shp)
  gc(); g <- gc(); rm(g); removeTmpFiles(h=0)
  
  # delanuay  <- delanuay[delanuay$area >= 0.02,]
  
  r <- raster()
  extent(r) <- extent(SDM)
  crs(r) <- crs(SDM)
  res(r) <- res(SDM)
  r[is.na(r[])]<- 0
  
  cat("Calculating distances inside each triangulation \n \n")
  
  if(is.null(ncores)){
    
    delaDist_list <- lapply(1:(length(delanuay)), function(x){
      
      vertex_1 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[1,] 
      vertex_1 <-  SpatialPoints(data.frame( x = vertex_1[1], y = vertex_1[2]), proj4string = crs(SDM))
      
      vertex_2 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[2,]
      vertex_2 <-  SpatialPoints(data.frame( x = vertex_2[1], y = vertex_2[2]), proj4string = crs(SDM))
      
      vertex_3 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[3,]
      vertex_3 <-  SpatialPoints(data.frame( x = vertex_3[1], y = vertex_3[2]), proj4string = crs(SDM))
      
      centroid <- delanuay@data[x, c("centroid.x", "centroid.y")]
      
      cat(paste("Processing feature: " , x, "\n"))
      cr <- raster::crop( x = r , y = extent(delanuay[x, ]) )
      rr <- mask(cr, delanuay[x,])
      
      if(all(is.na(rr[])) == TRUE){rr[is.na(rr[])] <- 0}
      
      xy <- SpatialPoints(centroid, proj4string = crs(SDM))
      
      dRas <- raster::distanceFromPoints(rr, xy)
      
      dVer1 <- raster::distanceFromPoints(rr, vertex_1)  + rr # dist to nearest vertex
      dVer2 <- raster::distanceFromPoints(rr, vertex_2) + rr # dist to nearest vertex
      dVer3 <- raster::distanceFromPoints(rr, vertex_3)+ rr # dist to nearest vertex
      
      dVer <- min(raster::stack(dVer1, dVer2, dVer3))
      dVer <- dVer/max(dVer[], na.rm = T)
      
      cat( paste("max value distances: ",  max(values(dRas),na.rm = T)," \n \n" ))
      
      dRas <-  dRas + rr
      dRas <- dRas/max(dRas[], na.rm = T)
      
      s <- delanuay$area[x]/max.area
      cat(paste("El area relativa para este feature es: ", s , "\n \n "))
      if(s > 1){s <- 1}
      sRas <- rr + s
      if(round(res(sRas)[1],8) !=  0.04166667){
        stop("me cago el la putisisma") # CAMBIAR
      }
      cat(paste("resolution:", res(sRas)[1], "****** \n \n \n "))
      return(list(dist_to_centroid = dRas, dist_near_vertex = dVer, area_score = sRas))
      
    })
    
  } else {
    
    cat(paste("Initializing parallel process with:", ncores, "cores (Be carefull with the number of cores) \n \n"))
    cl <- makeSOCKcluster(ncores)
    registerDoSNOW(cl)
    # on.exit(stopCluster(cl))
    
    ntasks <- length(delanuay)
    pb <- tkProgressBar(max = ntasks)
    progress <-  function(n) setTkProgressBar(pb, n)
    # function(n) cat(sprintf(" Number of task done: %d of %.0f  \n", n, ntasks ))
    
    opts <- list(progress = progress)
    
    delaDist_list <- foreach(x = 1:length(delanuay), .options.snow = opts, .packages = c("raster", "sp", "rgdal")) %dopar% {
      
      vertex_1 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[1,] 
      vertex_1 <- SpatialPoints(data.frame(x = vertex_1[1], y = vertex_1[2]), proj4string = crs(SDM))
      
      vertex_2 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[2,]
      vertex_2 <- SpatialPoints(data.frame(x = vertex_2[1], y = vertex_2[2]), proj4string = crs(SDM))
      
      vertex_3 <- delanuay@polygons[[x]]@Polygons[[1]]@coords[3,]
      vertex_3 <-  SpatialPoints(data.frame(x = vertex_3[1], y = vertex_3[2]), proj4string = crs(SDM))
      
      centroid <- delanuay@data[x, c("centroid.x", "centroid.y")]
      
      cat(paste("Processing feature: " , x, "\n"))
      cr <- raster::crop( x = r , y = extent(delanuay[x, ]))
      rr <- mask(cr, delanuay[x,])
      
      if(all(is.na(rr[])) == TRUE){rr[is.na(rr[])] <- 0}
      
      xy <- SpatialPoints(centroid, proj4string = crs(SDM))
      
      dRas <- raster::distanceFromPoints(rr, xy)
      
      dVer1 <- raster::distanceFromPoints(rr, vertex_1)  + rr # dist to nearest vertex
      dVer2 <- raster::distanceFromPoints(rr, vertex_2) + rr # dist to nearest vertex
      dVer3 <- raster::distanceFromPoints(rr, vertex_3)+ rr # dist to nearest vertex
      
      dVer <- min(raster::stack(dVer1, dVer2, dVer3))
      dVer <- dVer/max(dVer[], na.rm = T)
      
      cat( paste("max value distances:",  max(values(dRas), na.rm = T)," \n \n" ))
      
      dRas <-  dRas + rr
      dRas <- dRas/max(dRas[], na.rm = T)
      
      s <- delanuay$area[x]/max.area
      cat(paste("El area relativa para este feature es: ", s , "\n \n "))
      if(s > 1){s <- 1}
      sRas <- rr + s
      if(round(res(sRas)[1],8) !=  0.04166667){stop("Error: Extent Diferentes")} 
      cat(paste("resolution: ", res(sRas)[1], "****** \n \n \n "))
      return(list(dist_to_centroid = dRas, dist_near_vertex = dVer, area_score = sRas))
      
    }
    
    stopCluster(cl)
    
  }
  
  ### merge everything
  
  cat("+++ Merging raster (This process will take several minutes. Be patient) \n \n")
  
  out_dist_centroid <- delaDist_list[[1]]$dist_to_centroid
  out_dist_vertex <- delaDist_list[[1]]$dist_near_vertex
  out_area_relativa <- delaDist_list[[1]]$area_score
  
  mergeFun <- function(mRast){
    
    out_dist_centroid <<- merge(out_dist_centroid, mRast$dist_to_centroid)
    out_dist_vertex <<- merge(out_dist_vertex, mRast$dist_near_vertex)
    out_area_relativa <<- merge(out_area_relativa, mRast$area_score)             
    
  }
  
  system.time(
    lapply(delaDist_list[2:length(delaDist_list)], function(mRast){ 
      
      out_dist_centroid <<- merge(out_dist_centroid, mRast$dist_to_centroid)
      
      if(round(res(out_dist_centroid)[1],8) !=  0.04166667){stop("Error: Diferentes extents")} 
      
      out_dist_vertex <<- merge(out_dist_vertex, mRast$dist_near_vertex)
      out_area_relativa <<- merge(out_area_relativa, mRast$area_score)             
      
    })
  )
  
  cat("Calculating gap score \n \n ")
  
  x <- out_dist_centroid
  a <- out_dist_vertex
  p <- out_area_relativa * (1 - x) * a
  
  # p2 <- out_area_relativa * mean((1-x),a)
  # p3 <- out_area_relativa * sqrt((1-x),a)
  
  rm(x, a); g <- gc(); rm(g); removeTmpFiles(h = 24)
  
  # writeRaster(p, paste0(outDir, "/delaunay_aux.tif"), format = "GTiff", overwrite= T)
  # writeRaster(p2, paste0(outDir,"/delanuay_probs_mean.tif"), format = "GTiff", overwrite= T)
  # writeRaster(p3, paste0(outDir,"/delanuay_probs_sqrt.tif"), format = "GTiff", overwrite= T)
  
  SDM <- raster(sdmDir)
  # 
  # sdm_crop <- raster::crop(x = SDM, y = extent(p))
  # p_masked <- raster::mask(x = p, mask = sdm_crop)
  
  
  ###### calculating the gap score
  # gap_score <- sdm_crop * p_masked
  # extent(gap_score) <- extent(SDM)
  
  cat("Calculating scores for delaunay's outside points \n \n")
  #### scoring points out side of the delaunay triangulation
  
  dela_bound <-  gUnaryUnion(delanuay, id = NULL)
  
  vertex <- dela_bound@polygons[[1]]@Polygons[[1]]@coords[]
  vertex <- vertex[-1, ]
  vertex <- SpatialPoints(data.frame(x = vertex[,1], y = vertex[,2]), proj4string = crs(SDM))
  
  out <- mask(x = SDM, mask = dela_bound, inverse = T)
  
  out2 <- out
  out2[!is.na(out2[])] <- 1
  
  dist_out <- raster::distanceFromPoints(out2 , vertex)
  dist_out <- dist_out * out2
  #### normalizing distances
  dist_out_norm <- dist_out/max(dist_out[], na.rm = T)
  ### calculating new score
  # dist_out_score <-  dist_out_norm * out
  
  #gap_score<- raster(paste0(gap_outDir,"/delanuay_gap_score.tif"))
  
  gap_score <- merge(dist_out_norm, p)
  
  cat(paste("Writing raster in route:", paste0(outDir, "/delaunay.tif"), "\n \n"))
  writeRaster(gap_score, paste0(outDir, "/delaunay.tif"), format = "GTiff", overwrite = T)
  
}

# Example:
# a <- c("americas", "world")
# g <- c("mesoamerican", "andean")
# c <- "common_bean"
# lvl <- "lvl_1"
# results_dir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs" 
# delaunay_scoring(baseDir = baseDir, area = a[1], group = g[1], crop = c, lvl = "lvl_1", ncores = 10, validation = FALSE , pnt = NULL )
