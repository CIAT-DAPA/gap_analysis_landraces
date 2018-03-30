
suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, 
               deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, 
               rlang, fastcluster, sf, doParallel, rmapshaper ) 


g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

# Base directory
OSys <- Sys.info()[1]
baseDir   <- switch(OSys,
                   "Linux" = "/mnt/workspace_cluster_9/gap_analysis_landraces/runs",
                   "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs",
                   "Darwin" = "~nfs/workspace_cluster_9/gap_analysis_landraces/runs")
rm(OSys)

# Software directory
srcDir <- paste(baseDir, "/scripts", sep = "")
# Analysis region: "americas", "world"
region <- "americas"
# NOT TO RUN (crops directories and subdirectories)
# source(paste0(srcDir,"/preprocessing/pre_config.R"))

# Choose a directory for temporal raster files
raster::rasterOptions(tmpdir = choose.dir(default = "", caption = "Please select the temporary folder")) # "D:/TEMP/CSOSSA"
# for testing: raster::tmpDir()

# Calling species to run
# Configuring crop directories to run
source(paste0(srcDir, "/preprocessing/config_crop.R"))

# Define crop, analysis level and creating needed directories
crop <- "common_bean" # crop
level_1 <- c("andean", "mesoamerican") # level 1: genepool
level_2 <- c("nueva_granada", "peru", "chile", "durango-Jalisco", "mesoamerica","guatemala") # level 2: race
level_3 <- NULL # level 3
# x <- config_crop_dirs(baseDir, crop, level_1, level_2, level_3); rm(x)
##########

# Preparing inputs for each unit of analysis
level <- "lvl_1"
occName <- "mesoamerican" # "andean", "mesoamerican"
source(paste(srcDir, "/preprocessing/config.R", sep = ""))


SDM <- raster(paste0(model_outDir,"/mesoamerican_prj_median.tif"))

SDM[!is.na(SDM[])] <- 1

cat("Coverting SDM raster to Polygons \n \n ")
sdm_pol <- raster::rasterToPolygons(SDM, n = 4, na.rm = T, dissolve = T)

cat("Simplifying SDM  Polygon \n \n ")
sdm_pol_sf <- rmapshaper::ms_simplify(sdm_pol)
rm(sdm_pol)


delanuay <- shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/delanuay/delanuay_andres.shp")#change path after
delanuay@bbox <- as.matrix(extent(SDM))

cat("Converting  SDM  Polygon to sf \n \n ")

sf_dela <- st_as_sf(delanuay)
sf_sdm <- st_as_sf(sdm_pol_sf)

cat("Intersecting  SDM and Delanuay  Polygons \n \n ")

i <- st_intersection( st_buffer(sf_dela,0), st_buffer(sf_sdm, 0))
rm(sf_dela, sf_sdm)

cat("Removing Bad sf geometrys \n \n ")

bad <- unlist(lapply(1:length(i$geometry), function(x){
 if( is(i$geometry[x]) == "sfc_GEOMETRYCOLLECTION"){return(x)} else{return(NA) }
}) )
bad <-  bad[!is.na(bad)] 

if(length(bad) != 0){ i$geometry[bad] <- NULL  }

cat("writing intercepted polyfons to TempDir() \n \n ")

write_sf(  i$geometry , dsn = 
             paste0(tempdir(), "/intersection.shp")
             ,layer = "intercep"
             ,driver = "ESRI Shapefile"
             ,delete_layer=TRUE
          ,delete_dsn=TRUE
          )

rm(i, bad)

cat("Calculating Max area \n \n ")
i_shp <- shapefile(paste0(tempdir(), "/intersection.shp"))
max.area <- max(area(i_shp)/1000000) # al dividir por 1000000 la unidad es megametro^2 
 rm(i_shp)

gc(); g <- gc(); rm(g); removeTmpFiles(h=0)

#delanuay  <- delanuay[delanuay$area <= 873906.95,]
#delanuay  <- delanuay[delanuay$area >= 0.02,]

r <- raster()
extent(r) <- extent(SDM)
crs(r) <- crs(SDM)
res(r) <- res(SDM)
r[is.na(r[])]<- 0




system.time(
 delaDist_list <- lapply(1:(length(delanuay)), function(x){
   
   
   vertex <- delanuay@polygons[[x]]@Polygons[[1]]@coords[1:3,]
   centroid <- delanuay@data[x, 2:3]
   
   
   near_ver <- vertex[which.min(spDistsN1(vertex, as.matrix(centroid), longlat = TRUE) ), ]
   near_ver <- SpatialPoints(data.frame(x = near_ver[1], y = near_ver[2]), proj4string = crs(SDM))
   
   
   cat(paste("Processing feature: " , x, "\n"))
   cr <- raster::crop( x = r , y = extent(delanuay[x, ]) )
   rr <- mask(cr, delanuay[x,])
   
   if( all(is.na(rr[])) == TRUE){ rr[is.na(rr[])]<- 0}
    
   xy <- SpatialPoints(centroid, proj4string = crs(SDM))
   
   dRas <- raster::distanceFromPoints(rr, xy )
   dVer <- raster::distanceFromPoints(rr, near_ver)# dist to nearest vertex
   
   cat( paste("max value distances: ",  max(values(dRas),na.rm = T)," \n \n" ))
   
   dRas <-  dRas + rr
   dVer <- dVer + rr
  
  
   return(list(dist_to_centroid = dRas, dist_near_vertex = dVer))
   
 } )

)


### merge everything

 out_dist_centroid <- delaDist_list[[1]]$dist_to_centroid
 out_dist_vertex <- delaDist_list[[1]]$dist_near_vertex
 
 
mergeFun <- function(mRast){
  
  out_dist_centroid <<- merge(out_dist_centroid, mRast$dist_to_centroid)
  out_dist_vertex <<- merge(out_dist_vertex, mRast$dist_near_vertex)
}

system.time(
lapply(delaDist_list[2:length(delaDist_list)],mergeFun)
       )

writeRaster(outRast, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/delanuay/delanuay_andres.tif", format = "GTiff", overwrite= T)

 outRast2 <- 1 - outRast/max(outRast[], na.rm = T)
 writeRaster(outRast2, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/_datosAndres/delanuay/delanuay_norm_andres.tif", format = "GTiff", overwrite= T)
 


ncores <- detectCores(all.tests = FALSE, logical = TRUE)
cl <- makeCluster(ncores-5)
registerDoParallel(cl)

delaDist_list <- foreach(x = 1:length(delanuay), .packages = "raster") %dopar% {

  
   cr <- raster::crop( x = r , y = extent(delanuay[x, ]) )
  
   rr <- mask(cr, delanuay[x,])
   if( all(is.na(rr[])) == TRUE){ rr[is.na(rr[])]<- 0}
   
   xy <- SpatialPoints(delanuay@data[x, 2:3])
   crs(xy) <- crs(wmask)
   dRas <- raster::distanceFromPoints(rr, xy )

   dRas <-  dRas + rr
   dRas <- 1 - (dRas/max(dRas[], na.rm = T) )
   
   f_ras <- (delanuay$area[x]/max_area) * dRas  
   return(f_ras)
  
}

stopCluster(cl)
 
 

