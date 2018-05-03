##### ANDRES CAMILO MENDEZ
##### FUNCTION TO CREATE ALL DELANUAY  TRIANGULATIONS FOR ALL OCCURRENCES

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






validation_metrics <- function(n.sample = 100, bf_rad = 50, knl.dens = 30, baseDir,area, group, crop, lvl, pnt = NULL, ncores = 1, dens.level = "high_density" ,filename = "gap_score_cost_dist.tif" ){

  
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
  
  
  if(is.null(pnt)){stop("You should set a value for pnt. E.j ('pnt1' or 'pnt2'... )")}
  if(bf_rad >= 100){stop("Buffer radius must be less than 100 km")}
  
  
  coreDir <-  paste0("/", crop, "/", lvl, "/", group, "/", area)
  validationDir <-  paste0(coreDir, "/gap_validation/buffer_100km/",dens.level,"/",pnt)
  results_dir <- paste0(baseDir, "/results")
  
#Setting directories path 
  
  sdmDir <- paste0(results_dir, validationDir, "/02_sdm_results/prj_models/",group ,"_prj_median.tif")
  
  outDir <- paste0(results_dir, validationDir,"/03_gap_models")
  
  delaDir <- paste0(results_dir, validationDir,"/03_gap_models")
  
  occDir <- paste0(baseDir,"/input_data/by_crop", coreDir, "/occurrences/Occ")
  
  
  
cat(">>> Initializing validation process  \n \n")
# gap score
gp_m <- raster(paste0(outDir, "/", filename)) 
  

#Load raterized buffer and find the centroid of him

SDM <- raster(sdmDir)

buff <- raster(paste0(results_dir, validationDir,"/01_selected_points/buffer_radius_to_omit.tif"))

occ <- shapefile(paste0(results_dir, validationDir, "/01_selected_points/Occ.shp"))

buff <- raster::crop(buff, extent(occ))

cat("Initializing process of find buffers centroid \n ")
cat("Converting raster to polygons, this procces will take several minutes ... \n \n")

buff_pol <- rasterToPolygons(buff, dissolve = TRUE)

cent <- getSpPPolygonsLabptSlots(buff_pol)[2,]
# End find buffer centroid




#create a buffer whit shp  format
cat("Creating buffer with .shp format \n")

buffer_prime <- buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  crs(SDM)), width=100000)

occur <- shapefile(occDir)


buff_50 <- buffer(SpatialPoints(t(as.data.frame(cent)), proj4string =  crs(SDM)), width = bf_rad*1000 )

scr <- raster::extract(gp_m, buff_50)
scr <- unlist(scr)
scr <- scr[complete.cases(scr)]

# importing kernel density raster to extrac the point in the higgest densities areas
cat("Importing kernel density raster to extract points in the higgest densities areas \n")

knl <- raster(paste0(outDir, "/kernel.tif")) 
knl[knl[] <  knl.dens] <- NA
knl[knl[]>=  knl.dens] <- 1
b_occr <- raster::as.data.frame(knl, xy=T)
b_occr <- b_occr[complete.cases(b_occr),]


cords_dummy <- b_occr[,1:2] 

if(n.sample > nrow(cords_dummy)){stop("The number of n.sample is greater than the total coords in the selected density area")}
n_cords <- base::sample(nrow(cords_dummy), n.sample, replace = F  )


cl <- makeSOCKcluster(ncores)
registerDoSNOW(cl)

pb <- tkProgressBar(max=n.sample)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

cat(paste(">>> Initializing process to calculate the performance parameters for the score in:", pnt, "\n \n "))
results <- foreach( i = 1:n.sample, .combine = "rbind", .packages = c("raster", "pROC", "dplyr", "sdm"), .options.snow=opts)  %dopar% {
 
   cord <- cords_dummy[n_cords[i], ]
   buf_cord <- buffer(SpatialPoints(cord, proj4string =  crs(SDM)), width=bf_rad*1000)
  #dummy_buff <- buffer(buf_cord, width = bf_rad)
  
  #cat(paste("Extrayendo datos del buffer(it can take a long time): " ,i, "\n \n"))
  
  no_gap <- raster::extract(gp_m, buf_cord)
  no_gap <- unlist(no_gap)
  no_gap <- no_gap[complete.cases(no_gap)]
  
  if(length(no_gap)!=0){
    ng <- data.frame( score = no_gap, observe = rep(0, length(no_gap) ))
    gap <- data.frame(score = scr, observe = rep(1, length(scr)))
    
    
    bd <- bind_rows(ng, gap, .id = NULL)
    
    val <- pROC::roc(response = factor(bd$observe), predictor = bd$score )
    sm <- sdm::evaluates(factor(bd$observe), bd$score )
    
    auc <- sm@statistics$AUC
    m <- which(sm@threshold_based$criteria == "minROCdist")
    value <- sm@threshold_based$threshold[m]
    se <- sm@threshold_based$sensitivity[m]
    es <- sm@threshold_based$specificity[m]
    tss <- sm@threshold_based$TSS[m] 
  }else{
    
    value <- NA
    auc <- NA
    se <- NA
    es <- NA
    tss <- NA
  }
  return(c(value, auc, se, es, tss))
  
  
}
stopCluster(cl)



scl <- sort(unique(bd$score), decreasing = T)




results <- as.data.frame(results)
names(results) <- c("score", "auc", "se", "es", "tss")

cat(paste(">>> Saving results in:",paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"), "\n" ))

saveRDS(results, file = paste0(outDir, "/validation_metrics_", substr(filename, 11, 14),".rds"))

}# end function

a <- c("americas", "world")
g <- c("mesoamerican", "andean")
c <- "common_bean"
lvl <- "lvl_1"

validation_metrics(n.sample = 1000, bf_rad = 50, knl.dens = 20, baseDir = baseDir,area = a[1], group = g[1] , crop = c, lvl = "lvl_1", pnt = "pnt1", ncores = 15, dens.level = "high_density" ,filename = "gap_score_delanuay.tif" )

# fitdistr(cost_dist[!is.na(cost_dist[])], densfun =  "beta", start = list(shape1 = 1, shape2 = 1),lower = 0.001) 



