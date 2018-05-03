#### SCRIPT CREATED BY ANDRES CAMILO MENDEZ
#### THIS FUNCTION ALLOW YOU TO CONVERT THE BUFFER IN RASTER FORMAT TO A SHAPEFILE IF IT'S NECCESARY

suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, 
               deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, 
               rlang, fastcluster, sf, doParallel, rmapshaper, doSNOW, tcltk ) 


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

#############=++++=+++++++=+===+====+++++++






rasterbuffer_To_polygons <- function( baseDir,area, group, crop, lvl, pnt = NULL, dens.level = "high_density", bf_rad = 100  ){
  
  
  cat(
    "        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       
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
  
  if(is.null(pnt)){stop("You should set a value for pnt. E.j ('pnt1' or 'pnt2'... )")}

  
  coreDir <-  paste0("/", crop, "/", lvl, "/", group, "/", area)
  validationDir <-  paste0(coreDir, "/gap_validation/buffer_100km/",dens.level ,"/",pnt)
  results_dir <- paste0(baseDir, "/results")
  sdmDir <- paste0(results_dir, validationDir, "/02_sdm_results/prj_models/",group ,"_prj_median.tif")
  
  if(!file.exists(paste0(results_dir, validationDir,"/01_selected_points/buffer_radius_to_omit.shp") )){
  
  #Setting directories path 
  
  
  cat(">>> Initializing convertion process  \n \n")
  
  
  
  #Load raterized buffer and find the centroid of him
  
  SDM <- raster(sdmDir)
  
  
  buff <- raster(paste0(results_dir, validationDir,"/01_selected_points/buffer_radius_to_omit.tif"))
  
  occ <- shapefile(paste0(results_dir, validationDir, "/01_selected_points/Occ.shp"))
  
  buff <- raster::crop(buff, extent(occ))
  
  cat("Initializing process of find buffers centroid \n ")
  cat("Converting raster to polygons, this procces will take several minutes ... \n \n")
  
  buff_pol <- rasterToPolygons(buff, dissolve = TRUE)
  cent <- getSpPPolygonsLabptSlots(buff_pol)[2,]
  buffer_prime <- buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  crs(SDM)), width=bf_rad*1000)
  buffer_prime <- SpatialPolygonsDataFrame(buffer_prime, data = data.frame(1))
    writeOGR(obj = buffer_prime, dsn = paste0(results_dir, validationDir,"/01_selected_points"), layer = "buffer_radius_to_omit", driver = "ESRI Shapefile",overwrite_layer=TRUE)
  }else{
    cat("File already Exists \n")
    
  }#end if
}#end function

a <- c("americas", "world")
g <- c("mesoamerican", "andean")
c <- "common_bean"
lvl <- "lvl_1"
pnt <- paste0("pnt", 1:5)

cl <- makeSOCKcluster(length(pnt))
registerDoSNOW(cl)

pb <- tkProgressBar(max=length(pnt))
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)


foreach( i = 1:length(pnt), .combine = "rbind", .packages = c("raster", "rgdal", "dplyr", "sdm"), .options.snow=opts)  %dopar% {
  suppressMessages(if(!require(pacman)){install.packages('pacman'); library(pacman)} else {library(pacman)})
  pacman::p_load(dplyr, psych, tm, raster, rgdal, rasterVis, rgeos, 
                 deldir, sp, tidyverse, FactoMineR, factoextra, ggdendro, 
                 rlang, fastcluster, sf, doParallel, rmapshaper, doSNOW, tcltk ) 
  rasterbuffer_To_polygons( baseDir = baseDir, area = a[1], group = g[1], crop = c, lvl = lvl, pnt = pnt[i], dens.level = "high_density" , bf_rad = 100 )
  
}
stopCluster(cl)














