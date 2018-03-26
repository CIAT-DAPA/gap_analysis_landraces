# Validation process: landrace gap analysis
# Harold Achicanoy
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
#srcDir <- "D:/ToBackup/repositories/cwr-repo/gap_analysis_landraces/02_sdm_modeling"
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


# Selecting randomly an accession coordinate
# Creating a buffer of 50 km (100 km, ...) around the selected point
# Identify and exclude of the analysis the points within the buffer radius
# Run all the analysis:
# variable selection? (esto deberia quedar escrito solo para llamarlo)
# maxent calibration?, maxent run, cost distance, kernel density
# 5, 10, 20, 50, 100, 150

# Temporal files
# Crop
# Level
# LevelName
#

validation_process <- function(occName = "mesoamerican", sp_Dir,
                               buffer_radius = 0.5) # Radius of 50 km for excluding occurrences
{
  
  valDir <- paste0(sp_Dir, "/gap_validation")
  
  cat(">>> Loading occurrence data ... \n")
  swdFile <- paste0(swdDir, "/swd_", occName, ".csv")
  spData <- read.csv(swdFile)
  spData <- spData[,c(3:ncol(spData))]
  names(spData)[1] <- occName
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/Kernel.tif"))
  
  cat(">>> Select a point using kernel density as weights or pick one according to a criteria ... \n")
  set.seed(1234)
  pnt <- base::sample(x = 1:nrow(spData[which(spData[,1] == 1),]), size = 1, replace = F, prob = raster::extract(x = kernel, y = spData[which(spData[,1] == 1), c("lon", "lat")]))
  
  cat(">>> Create a buffer around the point ... \n")
  radius <- create_buffers(xy = spData[pnt, c("lon", "lat")], msk = mask, buff_dist = buffer_radius, format = "GTiff", filename = paste0(valDir, "/buffer_radius_to_omit.tif"))
  
  cat(">>> Identify and exclude of the analysis points within the buffer radius ... \n")
  id_pnts <- raster::extract(x = radius, y = spData[which(spData[,1] == 1), c("lon", "lat")])
  pnt_excl <- spData[which(spData[,1] == 1)[which(id_pnts == 1)],]
  write.csv(x = pnt_excl, file = paste0(valDir, "/coordinates_to_exclude.csv"), row.names = F)
  spData_upt <- spData[base::setdiff(1:nrow(spData), which(spData[,1] == 1)[which(id_pnts == 1)]),]; rm(id_pnts)
  write.csv(x = spData_upt, file = paste0(valDir, "/occ_", occName, "_updated.csv"), row.names = F)
  
  cat(">>> Load same variables for SDM ... \n")
  vars <- read.csv(paste0(sp_Dir, "/sdm_variables_selected.csv"))
  vars <- as.character(vars$x)
  
  cat(">>> Load calibrated feature parameters for SDM ... \n")
  calibration <- read.csv(paste0(sp_Dir, "/calibration.csv"))
  feat <- CreateMXArgs(calibration)
  beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
  feat <- feat[(!grepl("betamultiplier=", feat))]
  rm(calibration)
  
  # Run maxent model again
  m <- sdm_approach_function(occName = occName, spData = spData_upt,
                             model_outDir = valDir, var_names = vars,
                             nCores = 5, nFolds = 2, beta = beta, feat = feat)
  
  # Evaluate maxent again
  m2_eval <- evaluation_function(m, eval_sp_Dir = valDir)
  # Projecting the model again
  model <- projecting_function(m, m2_eval, model_outDir = valDir, nCores = 5, obj.size = 3)
  # Creating a new cost distance
  cost_dist_function(code = paste0(valDir, "/cost_dist.py"),
                     envDir = paste0("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/by_crop/common_bean/raster"),
                     lyr = friction,
                     outDir = paste0(valDir, "/raster"),
                     classResults = classResults,
                     occName = occName,
                     mask = mask,
                     occDir = occDir)
  # Creating a new kernel density
  raster_kernel()
  # And finaly calculate the indicator of gap once more
  gap_indicator(..., type = "sdm_costD") # "sdm_kDensity", "sdm_costD_kDensity"
  # Evaluating adding the excluded points
}