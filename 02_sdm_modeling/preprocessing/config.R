# Load libraries
cat("Loading R packages","\n")
suppressMessages(if(!require(sdm)){install.packages("sdm");library(sdm)}else{library(sdm)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(dismo)){install.packages("dismo");library(dismo)}else{library(dismo)})
suppressMessages(if(!require(rJava)){install.packages("rJava");library(rJava)}else{library(rJava)})
suppressMessages(if(!require(maptools)){install.packages("maptools");library(maptools)}else{library(maptools)})
suppressMessages(if(!require(deldir)){install.packages("deldir");library(deldir)}else{library(deldir)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos");library(rgeos)}else{library(rgeos)})

# Source functions
cat("Loading R scripts","\n")
source(paste(srcDir,"/preprocessing/cost_distance_function.R",sep=""))
source(paste(srcDir,"/sdm/sample_files.R",sep=""))
source(paste(srcDir,"/sdm/null_model.R",sep=""))
# source(paste(srcDir,"/sdm/delaunay.R",sep=""))
source(paste(srcDir,"/sdm/calibration_function.R",sep=""))
source(paste(srcDir,"/sdm/evaluation_function.R",sep=""))
source(paste(srcDir,"/sdm/model_driver.R",sep=""))
source(paste(srcDir,"/sdm/create_buffers.R",sep=""))
source(paste(srcDir,"/sdm/sdm_approach_function.R",sep=""))
source(paste(srcDir,"/sdm/do_projections.R",sep=""))
source(paste(srcDir,"/sdm/projecting_function.R",sep=""))
source(paste(srcDir,"/miscellanous/kernel_function.R",sep=""))
source(paste(srcDir,"/miscellanous/kernel_indicator.R",sep=""))

# Working directories
cat("Loading working directories","\n")
#Calling input dir and results dir
input_data_dir <-  paste0(baseDir, "/input_data")
results_dir <-  paste0(baseDir, "/results")

aux_dir <- paste(input_data_dir, "/auxiliar_rasters", sep = "")
climDir <- paste(input_data_dir, "/generic_rasters/", region, sep = "")
shp_dir <- paste(input_data_dir, "/shapefiles", sep = "")
mask_dir <- paste(input_data_dir, "/mask", sep = "")
#Defining species directory
crop_result_dir <- paste(results_dir, "/", crop, sep="")
level_result_dir <- paste(crop_result_dir, "/", level, sep = "")
classResults <- paste0(level_result_dir, "/classification")

#species model output directories
cat("Loading species model output directories\n")
cat(" ","\n")

cat("Loading species model input directories\n")
sp_Dir_input <- paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/", region)
occDir <- paste(sp_Dir_input, "/occurrences", sep = ""); if (!file.exists(occDir)) {dir.create(occDir)}
backDir <- paste(sp_Dir_input, "/background", sep = ""); if (!file.exists(backDir)) {dir.create(backDir)}
swdDir <- paste(sp_Dir_input, "/swd", sep = ""); if (!file.exists(swdDir)) {dir.create(swdDir)}
clim_spDir <- paste(input_data_dir, "/by_crop/", crop, "/raster", sep = ""); if (!file.exists(clim_spDir)) {dir.create(clim_spDir)}


cat("Loading species model output directories","\n")
sp_Dir <- paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region); if (!file.exists(sp_Dir)) {dir.create(sp_Dir)}
eval_sp_Dir <- paste0(sp_Dir, "/evaluation"); if(!file.exists(eval_sp_Dir)){dir.create(eval_sp_Dir)}
eval_sp_Dir_rep <- paste0(eval_sp_Dir, "/replicates");if(!file.exists(eval_sp_Dir_rep)){dir.create(eval_sp_Dir_rep)}
model_outDir <- paste0(sp_Dir, "/prj_models"); if(!file.exists(model_outDir)){dir.create(model_outDir)}
model_outDir_rep <- paste0(model_outDir, "/replicates");if(!file.exists(model_outDir_rep)){dir.create(model_outDir_rep)}
gap_outDir <- paste0(sp_Dir, "/gap_models"); if(!file.exists(gap_outDir)){dir.create(gap_outDir)}
gap_valDir <- paste0(sp_Dir, "/gap_validation"); if(!file.exists(gap_valDir)){dir.create(gap_valDir)}
gap_del_outDir <- paste0(gap_outDir, "/delaunay"); if(!file.exists(gap_del_outDir)){dir.create(gap_del_outDir)}

# modDir <- paste(results_dir,"/Results/SDM_modelling",sep=""); if (!file.exists(modDir)) {dir.create(modDir)}
# densDir <- paste(baseDir,"/Results/accession_density",sep=""); if (!file.exists(densDir)) {dir.create(densDir)}
# current_clim_layer_Dir <- paste(baseDir,"/Input_data/raster_sdm/2_5m",sep="")

#Loading files to perform Delaunay analysis

#cont_mask <- paste(baseDir,"/Input_data/_maps/GAUL_2014/G2014_2013_0_NO_ANT.shp",sep="")
#cont_mask <- paste(shp_dir,"/GAUL_2014/CONTINENTAL.shp",sep="")
cont_mask <- paste(shp_dir, "/GAUL_2014/CONTINENTAL_20km.shp", sep = "")

friction <- paste(aux_dir, "/friction_surface.tif", sep = "")
mask <- paste0(mask_dir, "/mask_", region, ".tif")
cat(" ","\n")
