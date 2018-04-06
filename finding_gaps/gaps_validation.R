##### ANDRES CAMILO MENDEZ
##### FUNCTION TO CREATE ALL DELANUAY  TRIANGULATIONS FOR ALL OCCURRENCES

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

#############=++++=+++++++=+===+====+++++++

area <- c("americas", "world")
group <- c("mesoamerican", "andean")
crop <- "common_bean"


for(i in 1:2){
  
  for(j in  1:2){ 
    
    outDir <- paste0(level_result_dir,"/", group[i], "/", area[j], "/gap_models" )
    occDir <- paste0(input_data_dir,"/by_crop/",crop, "/",level , "/", group[i], "/",area[j], "/occurrences/Occ")
    x <- shapefile(occDir)
   delaunaypolygons(x = x , outdir = outDir)
   
    
  }
}


gp_m <- raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km/high_density/pnt1/03_gap_models/gap_score_cost_dist.tif")
buff <- raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km/high_density/pnt1/01_selected_points/buffer_radius_to_omit.tif")

buff
buff_pol <- rasterToPolygons(buff, dissolve = TRUE)
cent <- getSpPPolygonsLabptSlots(buff_pol)

cent<- c(-97.100000  18.866700)

buffer_prime <- buffer(SpatialPoints(cent, proj4string =  crs(SDM)), width=100000)

occDir <- paste0(input_data_dir,"/by_crop/",crop, "/",level , "/", group[1], "/",area[1], "/occurrences/Occ")

occur <- shapefile(occDir)


bf_rad <- 50000
buff_50 <- buffer(SpatialPoints(cent, proj4string =  crs(SDM)), width =bf_rad)

scr <- raster::extract(gp_m, buff_50)
scr <- unlist(scr)
scr <- scr[complete.cases(scr)]

knl <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km/high_density/pnt1/03_gap_models/kernel.tif")
knl[knl[] < 30] <- NA
knl[knl[]>= 30] <- 1
b_occr <- raster::as.data.frame(knl, xy=T)
b_occr <- b_occr[complete.cases(b_occr),]

#b_occr <- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km/high_density/pnt1/01_selected_points/occ_mesoamerican.csv")


cords_dummy <- b_occr[,1:2] 
ifelse(nrow(cords_dummy) >= 1000, n.sample <- 1000, n.sample <- nrow(cords_dummy))
n_cords <- base::sample(nrow(cords_dummy), n.sample, replace = F  )



ncores <- detectCores(all.tests = FALSE, logical = TRUE)
cl <- makeCluster(15)
registerDoParallel(cl)

paralel <- foreach( i = 1:1000, .combine = "rbind", .packages = c("raster", "pROC", "dplyr", "sdm"))  %dopar% {
  cord <- cords_dummy[n_cords[i], ]
  buf_cord <- buffer(SpatialPoints(cord, proj4string =  crs(SDM)), width=bf_rad)
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
paralel <- as.data.frame(paralel)
names(paralel) <- c("score", "auc", "se", "es", "tss")

stopCluster(cl)

 fitdistr(cost_dist[!is.na(cost_dist[])], densfun =  "beta", start = list(shape1 = 1, shape2 = 1),lower = 0.001) 



