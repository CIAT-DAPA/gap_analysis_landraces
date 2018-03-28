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

srcDir <- paste(baseDir, "/scripts", sep = "")
region <- "americas"
raster::rasterOptions(tmpdir = choose.dir(default = "", caption = "Please select the temporary folder")) # "D:/TEMP/CSOSSA"
source(paste0(srcDir, "/preprocessing/config_crop.R"))

crop <- "common_bean" # crop
level_1 <- c("andean", "mesoamerican") # level 1: genepool
level_2 <- c("nueva_granada", "peru", "chile", "durango-Jalisco", "mesoamerica","guatemala") # level 2: race
level_3 <- NULL # level 3
#x <- config_crop_dirs(baseDir, crop, level_1, level_2, level_3); rm(x)
level <- "lvl_1"
occName <- "mesoamerican" # andean
source(paste(srcDir, "/preprocessing/config.R", sep = ""))

# Validation process
# maxent calibration?, maxent run, cost distance, kernel density
# 5, 10, 20, 50, 100, 150

gap_valDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km"

validation_process <- function(occName = occName,
                               gap_valDir = gap_valDir,
                               buffer_radius = 1) # Radius of 50 km for excluding occurrences
{
  
  cat(">>> Loading occurrence data ... \n")
  swdFile <- paste0(swdDir, "/swd_", occName, ".csv")
  spData <- read.csv(swdFile)
  spData <- spData[,c(3:ncol(spData))]
  names(spData)[1] <- occName
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/kernel.tif"))
  
  cat(">>> Select a point using kernel density as weights or pick one according to a criteria ... \n")
  occ <- spData[which(spData[,1] == 1),]
  set.seed(1234)
  pnt <- base::sample(x = 1:nrow(occ), size = 1, replace = F, prob = raster::extract(x = kernel, y = occ[,c("lon", "lat")]))
  
  cat(">>> Create a buffer around the point ... \n")
  radius <- create_buffers(xy = occ[pnt, c("lon", "lat")], msk = mask, buff_dist = buffer_radius, format = "GTiff", filename = paste0(gap_valDir, "/01_selected_points/buffer_radius_to_omit.tif"))
  
  cat(">>> Identify and exclude of the analysis points within the buffer radius ... \n")
  id_pnts <- raster::extract(x = radius, y = occ[,c("lon", "lat")])
  pnt_excl <- occ[which(id_pnts == 1),]
  write.csv(x = pnt_excl, file = paste0(gap_valDir, "/01_selected_points/coordinates_to_exclude.csv"), row.names = F)
  spData_upt <- occ[base::setdiff(1:nrow(occ), which(id_pnts == 1)),]; rm(id_pnts)
  write.csv(x = spData_upt, file = paste0(gap_valDir, "/01_selected_points/occ_", occName, "_updated.csv"), row.names = F)
  
  spData_upt2 <- spData_upt[,c("lon", "lat")]
  names(spData_upt2) <- c("Longitude", "Latitude")
  spData_upt2$ensemble <- occName
  write.csv(x = spData_upt2, file = paste0(gap_valDir, "/01_selected_points/genepool_predicted.csv"), row.names = F)
  rm(spData_upt2)

  cat(">>> Creating cost distance raster ... \n")
  cost_dist_function(code = paste0(gap_valDir, "/01_selected_points/cost_dist.py"),
                     outDir = paste0(gap_valDir, "/03_gap_models"),
                     friction = friction,
                     classResults = paste0(gap_valDir, "/01_selected_points"),
                     occName = occName,
                     mask = mask,
                     occDir = paste0(gap_valDir, "/01_selected_points")
                     )

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
  cat("Running modeling approach","\n")
  m2 <- sdm_approach_function(occName = occName,
                              spData = rbind(spData_upt, spData[which(spData[,1] == 0),]),
                              model_outDir = paste0(gap_valDir, "/02_sdm_results"),
                              var_names = vars,
                              nCores = 5,
                              nFolds = 5,
                              beta = beta,
                              feat = feat)
  
  # Evaluate maxent again
  m2_eval <- evaluation_function(m2, eval_sp_Dir = paste0(gap_valDir, "/02_sdm_results/evaluation"))
  # Projecting the model again
  model <- projecting_function(m2, m2_eval, model_outDir = paste0(gap_valDir, "/02_sdm_results/evaluation"), nCores = 5, obj.size = 3)
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
  kernel <- raster_kernel(mask = mask,
                          occurrences = spData_upt[spData_upt[,1] == 1,],
                          out_dir = paste0(gap_valDir, "/03_gap_models"),
                          kernel_method = 3,
                          scale = T)
  # And finaly calculate the indicator of gap once more
  gap_indicator(..., type = "sdm_costD") # "sdm_kDensity", "sdm_costD_kDensity"
  # Evaluating adding the excluded points
}