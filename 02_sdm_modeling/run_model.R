# Landrace gap analysis: base code
# Chrystian Sosa, Julian Ramirez-Villegas, Harold Achicanoy
# CIAT, March 2018

# R options
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
x <- config_crop_dirs(baseDir, crop, level_1, level_2, level_3); rm(x)
##########

# Preparing inputs for each unit of analysis
level <- "lvl_1"
occName <- "mesoamerican" # andean
source(paste(srcDir, "/preprocessing/config.R", sep = ""))

# Creating the cost distance function according with the level of analysis
cost_dist_function(code = paste0(sp_Dir_input, "/cost_dist.py"),
                   envDir = paste0("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/common_bean/raster"),
                   lyr = friction,
                   outDir = paste0(sp_Dir_input, "/raster"),
                   classResults = classResults,
                   occName = occName,
                   mask = mask
                   envDir, friction, outDir, classResults, occName, occDir, mask
)



# Model driver function for preparing which variables will be selected to run SDMs and creation of SWD files
extension_r <- ".tif"
clsModel <- "ensemble"
correlation <- 3 #1 Correlation, 2 VIF, 3 PCA +VIF
var_names <- model_driver(sp_Dir, mask, occName, extension_r, all = F, overwrite = T, clsModel, correlation = correlation)


# Loading SWD file and occurrence data
swdFile <- paste0(swdDir, "/swd_", occName, ".csv")
spData <- read.csv(swdFile)
spData <- spData[,c(3:ncol(spData))]
names(spData)[1] <- occName

# Loading raster files
clim_layer <- lapply(paste0(climDir, "/", paste0(var_names,".tif")), raster)
clim_layer <- stack(clim_layer)

# Calibration step
cat("Performing calibration step","\n")
if(file.exists(paste0(sp_Dir, "/calibration.csv"))){
  calibration <- read.csv(paste0(sp_Dir, "/calibration.csv"))
  feat <- CreateMXArgs(calibration)
  beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
  feat <- feat[(!grepl("betamultiplier=", feat))]
  rm(calibration)
} else {
  feat <- Calibration_function(spData = spData, save = T, sp_Dir = sp_Dir, ommit = F)
  beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
  feat <- feat[(!grepl("betamultiplier=", feat))]
}

# Running SDMs
cat("Running modeling approach","\n")
m2 <- sdm_approach_function(occName = occName, spData = spData, model_outDir = sp_Dir, var_names = var_names, nCores = 5, nFolds = 5, beta, feat)

# Model evaluation per replicates (nReplicates x 5)
cat("Evaluating models performance\n")
m2_eval <- evaluation_function(m2, eval_sp_Dir)

# Model projecting
cat("Projecting models\n")
# Detaching caret to avoid packages conflict
detach("package:caret", unload = TRUE) # MANDATORY!
model <- projecting_function(m2, m2_eval, model_outDir, nCores = 5, obj.size = 3)

# Final evaluation table
cat("Validating model","\n")
if(!file.exists(paste0(eval_sp_Dir,"/Final_evaluation.csv"))){
  m2_eval_final <- final_evaluation(m2_eval, occName)
} else {
  m2_eval_final <-  read.csv(paste0(eval_sp_Dir, "/Final_evaluation.csv"), header = T)
}

# Run buffer approach
cat("Creating buffer around 50 Km\n")
if(!file.exists(paste0(gap_outDir, "/buffer.tif"))){
  buffer <- create_buffers(xy = spData[which(spData[,1] == 1), c("lon", "lat")], msk = mask, buff_dist = 0.5, format = "GTiff", filename = paste0(gap_outDir, "/buffer.tif"))
  buffer[which(buffer[] == 0)] <- NA; buffer[which(buffer[] != 0)] <- 10
} else {
  buffer <- raster(paste0(gap_outDir, "/buffer.tif"))
}


# Gap maps
cat("Making gap map\n")

if(!file.exists(paste0(gap_outDir, "/gap_map.tif"))){
  gap_map <- sum(model, buffer, na.rm = T)
  gap_map[which(gap_map[] == 10)] <- NA 
  gap_map[which(gap_map[] == 11)] <- NA 
  gap_map[which(gap_map[] == 0)] <- NA 
  writeRaster(gap_map, paste0(gap_outDir, "/gap_map.tif"))
} else {
  gap_map <- raster(paste0(gap_outDir, "/gap_map.tif"))
}

#Kernel function #1 spatstat, #2 Adehabitat, #3 Kernsmooth
if(!file.exists(paste0(gap_outDir,"/","kernel.tif"))){
  occurrences=spData[spData[,1]==1,]
  kernel <- raster_kernel(mask = mask, occurrences = spData[spData[,1] == 1,], out_dir = gap_outDir, kernel_method = 3, scale = T)
} else {
  kernel <- raster(paste0(gap_outDir, "/kernel.tif")) 
}

# Put here environmental similarity index!!!

# Gathering Kernel gap indicator
if(!file.exists(paste0(gap_outDir, "/Kernel_indicator.tif"))){
  kernel_indicator <- kernel_indicator(kernel, friction, model_outDir, gap_outDir, reverse = T)
} else {
  kernel_indicator <- raster(paste0(gap_outDir,"/Kernel_indicator.tif"))   
}
