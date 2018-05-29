# Load libraries
cat("Loading R packages\n")
suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(sdm, raster, rgdal, dismo, rJava, maptools, deldir, rgeos)

# Source functions
cat("Loading R scripts\n")
source(paste0(srcDir, "/preprocessing/cost_distance_function.R"))
source(paste0(srcDir, "/sdm/sample_files.R"))
source(paste0(srcDir, "/sdm/null_model.R"))
source(paste0(srcDir, "/sdm/calibration_function.R"))
source(paste0(srcDir, "/sdm/evaluation_function.R"))
source(paste0(srcDir, "/sdm/model_driver.R"))
source(paste0(srcDir, "/sdm/create_buffers.R"))
source(paste0(srcDir, "/sdm/sdm_approach_function.R"))
source(paste0(srcDir, "/sdm/do_projections.R"))
source(paste0(srcDir, "/sdm/projecting_function.R"))
source(paste0(srcDir, "/miscellanous/kernel_function.R"))
source(paste0(srcDir, "/miscellanous/kernel_indicator.R"))
source(paste0(srcDir, "/gap_methods/ecogeo_cluster.R"))
source(paste0(srcDir, "/gap_methods/env_distance.R"))
source(paste0(srcDir, "/gap_methods/combine_score.R"))
source(paste0(srcDir, "/gap_methods/delaunay.R"))

# Working directories
cat("Loading working directories\n")
input_data_dir <- paste0(baseDir, "/input_data")
results_dir    <- paste0(baseDir, "/results")
aux_dir  <- paste0(input_data_dir, "/auxiliar_rasters")
climDir  <- paste0(input_data_dir, "/generic_rasters/", region)
shp_dir  <- paste0(input_data_dir, "/shapefiles")
mask_dir <- paste0(input_data_dir, "/mask")

# Defining species directory
crop_result_dir  <- paste0(results_dir, "/", crop)
level_result_dir <- paste0(crop_result_dir, "/", level)
classResults     <- paste0(level_result_dir, "/classification")

# Species model output directories
cat("Loading species model output directories\n")
cat(" \n")

cat("Loading species model input directories\n")
sp_Dir_input <- paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/", region)
occDir       <- paste0(sp_Dir_input, "/occurrences"); if (!file.exists(occDir)) {dir.create(occDir)}
backDir      <- paste0(sp_Dir_input, "/background"); if (!file.exists(backDir)) {dir.create(backDir)}
swdDir       <- paste0(sp_Dir_input, "/swd"); if (!file.exists(swdDir)) {dir.create(swdDir)}
clim_spDir   <- paste0(input_data_dir, "/by_crop/", crop, "/raster"); if (!file.exists(clim_spDir)) {dir.create(clim_spDir)}

cat("Loading species model output directories","\n")
sp_Dir           <- paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region); if (!file.exists(sp_Dir)) {dir.create(sp_Dir)}
eval_sp_Dir      <- paste0(sp_Dir, "/evaluation"); if(!file.exists(eval_sp_Dir)){dir.create(eval_sp_Dir)}
eval_sp_Dir_rep  <- paste0(eval_sp_Dir, "/replicates");if(!file.exists(eval_sp_Dir_rep)){dir.create(eval_sp_Dir_rep)}
model_outDir     <- paste0(sp_Dir, "/prj_models"); if(!file.exists(model_outDir)){dir.create(model_outDir)}
model_outDir_rep <- paste0(model_outDir, "/replicates");if(!file.exists(model_outDir_rep)){dir.create(model_outDir_rep)}
gap_outDir       <- paste0(sp_Dir, "/gap_models"); if(!file.exists(gap_outDir)){dir.create(gap_outDir)}
gap_valDir       <- paste0(sp_Dir, "/gap_validation"); if(!file.exists(gap_valDir)){dir.create(gap_valDir)}
gap_del_outDir   <- paste0(gap_outDir, "/delaunay"); if(!file.exists(gap_del_outDir)){dir.create(gap_del_outDir)}

# Loading files to perform Delaunay analysis
cont_mask <- paste0(shp_dir, "/GAUL_2014/CONTINENTAL_20km.shp")
friction  <- paste0(aux_dir, "/friction_surface.tif")
mask      <- paste0(mask_dir, "/mask_", region, ".tif")
cat(" \n")
