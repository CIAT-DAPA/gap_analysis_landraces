
# Source functions
cat("Loading R scripts\n")
source(paste0(srcDir, "/01_classification/classification_function.R"))
source(paste0(srcDir, "/01_classification/crop_raster.R"))
source(paste0(srcDir, "/01_classification/create_occ_shp.R"))
source(paste0(srcDir, "/01_classification/prepare_input_data.R"))
source(paste0(srcDir, "/02_sdm_modeling/background_points.R"))
#source(paste0(srcDir, "/02_sdm_modeling/model_driver.R"))
source(paste0(srcDir, "/02_sdm_modeling/null_model.R"))
source(paste0(srcDir, "/02_sdm_modeling/calibration_function.R"))
source(paste0(srcDir, "/02_sdm_modeling/tuning_maxNet.R"))
#source(paste0(srcDir, "/02_sdm_modeling/evaluation_function.R"))
#source(paste0(srcDir, "/02_sdm_modeling/sdm_approach_function.R"))
source(paste0(srcDir, "/02_sdm_modeling/sdm_maxnet_approach_function.R"))
#source(paste0(srcDir, "/02_sdm_modeling/do_projections.R"))
#source(paste0(srcDir, "/02_sdm_modeling/projecting_function.R"))
#source(paste0(srcDir, "/02_sdm_modeling/miscellanous/kernel_indicator.R")) --- removed
source(paste0(srcDir, "/03_gap_methods/create_buffers.R"))
source(paste0(srcDir, "/03_gap_methods/kernel_function.R"))
source(paste0(srcDir, "/03_gap_methods/cost_distance_function.R"))
source(paste0(srcDir, "/03_gap_methods/ecogeo_cluster.R"))
source(paste0(srcDir, "/03_gap_methods/env_distance.R"))
source(paste0(srcDir, "/03_gap_methods/combine_score.R"))
source(paste0(srcDir, "/03_gap_methods/delaunay.R"))
source(paste0(srcDir, "/03_gap_methods/delaunay_geo_score.R"))
source(paste0(srcDir, "/03_gap_methods/gaps_validation.R"))
source(paste0(srcDir, "/03_gap_methods/summary_function.R"))
source(paste0(srcDir, "/03_gap_methods/validation_function.R"))
source(paste0(srcDir, "/03_gap_methods/create_png_maps.R"))

# Working directories
cat("Loading working directories\n")
input_data_dir <- paste0(baseDir, "/input_data")
results_dir    <- paste0(baseDir, "/results")
worldDir       <- paste0(input_data_dir, "/generic_rasters/world");if(!file.exists(worldDir)){dir.create(worldDir, recursive = TRUE)}
climDir        <- paste0(input_data_dir, "/generic_rasters/", region);if(!file.exists(climDir)){dir.create(climDir, recursive = TRUE)}
shp_dir        <- paste0(input_data_dir, "/shapefiles")
mask_dir       <- paste0(input_data_dir, "/mask")
classResults   <- paste0(input_data_dir, "/by_crop/", crop, "/", level, "/classification");if(!file.exists(classResults)){dir.create(classResults, recursive = TRUE)}
aux_dir        <- paste(input_data_dir,"/auxiliar_rasters",sep=""); if(!file.exists(aux_dir)){dir.create(aux_dir)}
# Defining species directory
crop_result_dir  <- paste0(results_dir, "/", crop)
level_result_dir <- paste0(crop_result_dir, "/", level)



cat("Loading species model input directories\n")
sp_Dir_input <- paste0(input_data_dir, "/by_crop/", crop, "/", level, "/", occName, "/", region); if(!file.exists(sp_Dir_input )) {dir.create(sp_Dir_input, recursive = TRUE)}
occDir       <- paste0(sp_Dir_input, "/occurrences"); if (!file.exists(occDir)) {dir.create(occDir, recursive = TRUE)}
backDir      <- paste0(sp_Dir_input, "/background"); if (!file.exists(backDir)) {dir.create(backDir, recursive = TRUE)}
swdDir       <- paste0(sp_Dir_input, "/swd"); if (!file.exists(swdDir)) {dir.create(swdDir, recursive = TRUE)}
clim_spDir   <- paste0(input_data_dir, "/by_crop/", crop, "/raster"); if (!file.exists(clim_spDir)) {dir.create(clim_spDir, recursive = TRUE)}
clim_spReg   <- paste0(clim_spDir, "/", region);if (!file.exists(clim_spReg)) {dir.create(clim_spReg, recursive = TRUE)}
clim_spWorld <- paste0(clim_spDir, "/world");if (!file.exists(clim_spWorld)) {dir.create(clim_spWorld, recursive = TRUE)}
native_area_dir <- paste0(input_data_dir, "/by_crop/", crop, "/native_area") ;if (!file.exists(native_area_dir)) {dir.create(native_area_dir, recursive = TRUE)}
# Species model output directories

cat("Loading species model output directories","\n")
sp_Dir           <- paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region); if (!file.exists(sp_Dir)) {dir.create(sp_Dir, recursive = TRUE)}
eval_sp_Dir      <- paste0(sp_Dir, "/evaluation"); if(!file.exists(eval_sp_Dir)){dir.create(eval_sp_Dir, recursive = TRUE)}
eval_sp_Dir_rep  <- paste0(eval_sp_Dir, "/replicates");if(!file.exists(eval_sp_Dir_rep)){dir.create(eval_sp_Dir_rep, recursive = TRUE)}
model_outDir     <- paste0(sp_Dir, "/prj_models"); if(!file.exists(model_outDir)){dir.create(model_outDir, recursive = TRUE)}
model_outDir_rep <- paste0(model_outDir, "/replicates");if(!file.exists(model_outDir_rep)){dir.create(model_outDir_rep, recursive = TRUE)}
gap_outDir       <- paste0(sp_Dir, "/gap_models"); if(!file.exists(gap_outDir)){dir.create(gap_outDir, recursive = TRUE)}
gap_valDir       <- paste0(sp_Dir, "/gap_validation"); if(!file.exists(gap_valDir)){dir.create(gap_valDir, recursive = TRUE)}
gap_del_outDir   <- paste0(gap_outDir, "/delaunay"); if(!file.exists(gap_del_outDir)){dir.create(gap_del_outDir, recursive = TRUE)}
grph_dir         <- paste0(results_dir, "/", crop, "/lvl_1/", occName, "/", region, "/graphics");if(!dir.exists(grph_dir)){ dir.create(grph_dir, recursive = T) }
input_data_aux_dir <- paste0(results_dir, "/", crop, "/lvl_1/", occName, "/", region, "/input_data");if(!dir.exists(input_data_aux_dir)){ dir.create(input_data_aux_dir, recursive = T) }
  
cat("Loading dirs for validation process")

if(!file.exists(paste0(gap_valDir, "/buffer_100km")) ){dir.create(paste0(gap_valDir, "/buffer_100km"))}
if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density"))}
lapply(1:5, function(x){
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/01_selected_points")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/01_selected_points"), recursive = TRUE)}
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/02_sdm_results/evaluation/replicates")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/02_sdm_results/evaluation/replicates"), recursive = TRUE)}
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/02_sdm_results/prj_models/replicates")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/02_sdm_results/prj_models/replicates"), recursive = TRUE)}
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/03_gap_models/delaunay")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/03_gap_models/delaunay"), recursive = TRUE)}
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/04_gap_index")) ){dir.create(paste0(gap_valDir, "/buffer_100km/high_density/pnt",x,"/04_gap_index"), recursive = TRUE)}

})



# Loading files to perform Delaunay analysis
cont_mask <- paste0(shp_dir, "/GAUL_2014/CONTINENTAL_20km.shp")
friction  <- paste0(aux_dir, "/friction_surface.tif")
mask      <- paste0(mask_dir, "/mask_", region, ".tif")
cat(" \n")
