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

cat(">>> Applying validation process for gap metrics <<<\n")

gap_valDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_validation/buffer_100km"
validation_process <- function(occName = occName,
                               gap_valDir = gap_valDir,
                               buffer_radius = 1, # Radius of 100 km for excluding occurrences
                               density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                               geo_score = "cost_dist") # Can be: "cost_dist", "kernel", "delaunay"
{
  
  cat(">>> Loading occurrence data ... \n")
  swdFile <- paste0(swdDir, "/swd_", occName, ".csv")
  spData <- read.csv(swdFile)
  spData <- spData[,c(3:ncol(spData))]
  names(spData)[1] <- occName
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/kernel.tif"))
  kernel_class <- raster(paste0(sp_Dir, "/gap_models/kernel_classes.tif"))
  
  densities <- c("low", "medium", "high")
  
  kernel_class[kernel_class[] != density_pattern] <- NA
  kernel_upt <- raster::mask(x = kernel, mask = kernel_class)
  
  occ <- spData[which(spData[,1] == 1),]
  
  cat(">>> Selecting 5 points randomly using kernel density level as weights ... \n")
  set.seed(1234)
  seedList <- round(runif(n = 5, min = 1, max = 10000))
  for(i in 1:length(seedList)){
    
    cat(">>> Processing point", i, "... \n")
    set.seed(seedList[i])
    probList <- raster::extract(x = kernel_upt, y = occ[,c("lon", "lat")])
    probList[is.na(probList)] <- 0
    pnt <- base::sample(x = 1:nrow(occ), size = 1, replace = F, prob = probList)
    
    cat(">>> Create a buffer around the point", i, "... \n")
    radius <- create_buffers(xy = occ[pnt, c("lon", "lat")],
                             msk = mask,
                             buff_dist = buffer_radius,
                             format = "GTiff",
                             filename = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/buffer_radius_to_omit.tif"))
    
    cat(">>> Identify and exclude of the analysis points within the buffer radius ... \n")
    id_pnts <- raster::extract(x = radius, y = occ[,c("lon", "lat")])
    pnt_excl <- occ[which(id_pnts == 1),]
    write.csv(x = pnt_excl,
              file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/coordinates_to_exclude.csv"),
              row.names = F)
    spData_upt <- occ[base::setdiff(1:nrow(occ), which(id_pnts == 1)),]; rm(id_pnts)
    write.csv(x = spData_upt,
              file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/occ_", occName, "_updated.csv"),
              row.names = F)
    
    spData_upt2 <- spData_upt[,c("lon", "lat")]
    names(spData_upt2) <- c("Longitude", "Latitude")
    spData_upt2$ensemble <- occName
    write.csv(x = spData_upt2,
              file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/genepool_predicted.csv"),
              row.names = F)
    rm(spData_upt2)
    
    cat(">>> Creating cost distance raster ... \n")
    cost_dist_function(code = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/cost_dist.py"),
                       outDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                       friction = friction,
                       classResults = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                       occName = occName,
                       mask = mask,
                       occDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points")
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
    
    cat(">>> Running maxent model approach, evaluate and project ...\n")
    m2 <- sdm_approach_function(occName = occName,
                                spData = rbind(spData_upt, spData[which(spData[,1] == 0),]),
                                model_outDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                                var_names = vars,
                                nCores = 5,
                                nFolds = 5,
                                beta = beta,
                                feat = feat)
    m2_eval <- evaluation_function(m2,
                                   eval_sp_Dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/evaluation"),
                                   spData = rbind(spData_upt, spData[which(spData[,1] == 0),]))
    model_outDir_rep <- paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/replicates")
    clim_layer <- lapply(paste0(climDir, "/", vars, ".tif"), raster)
    clim_layer <- raster::stack(clim_layer)
    clim_table <- raster::as.data.frame(clim_layer, xy = T)
    clim_table <- clim_table[complete.cases(clim_table),]
    model <- projecting_function(m2,
                                 m2_eval,
                                 clim_table,
                                 mask,
                                 model_outDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                 nCores = 5,
                                 obj.size = 3)
    
    cat(">>> Creating a new kernel density ...\n")
    kernel <- raster_kernel(mask = mask,
                            occurrences = spData_upt[spData_upt[,1] == 1,],
                            out_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                            kernel_method = 3,
                            scale = T)
    
  }
  
  cat(">>> Creating a new environmental distance ...\n")
  calc_env_score(lv_name = occName,
                 clus_method = "hclust_mahalanobis",
                 sdm_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                 gap_dir = gap_outDir,
                 occ_dir = occDir,
                 env_dir = climDir,
                 out_dir = gap_outDir)
  
  cat(">>> Calculating gap indicator ...\n")
  calc_gap_score(lv_name = occName,
                 clus_method = "hclust_mahalanobis",
                 gap_method = "cost_dist", # Can be: "cost_dist", "kernel", "delaunay"
                 sdm_dir = model_outDir,
                 gap_dir = gap_outDir,
                 out_dir = gap_outDir)
  
  cat(">>> Evaluate with exclude points ...\n")
  
}