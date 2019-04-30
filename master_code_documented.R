# Landrace Gap Analysis: master code
# Chrystian Sosa, Julian Ramirez, Harold Achicanoy, Andres Mendez, Maria Diaz, Colin Khoury
# CIAT, 2019

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

# Define base directory, according with your operative system
OSys <- Sys.info()[1]
baseDir   <- switch(OSys,
                   "Linux"   = "/mnt/workspace_cluster_9/gap_analysis_landraces/runs",
                   "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs",
                   "Darwin"  = "~nfs/workspace_cluster_9/gap_analysis_landraces/runs")
rm(OSys)

# Define code's folder
srcDir <- paste(baseDir, "/scripts", sep = "")
# Define region of study
region <- "africa"

# Configuring crop directories
source(paste0(srcDir, "/02_sdm_modeling/preprocessing/config_crop.R"))

# Define crop
crop <- "african_maize"
# Define level of analysis
level_1 <-  c("2", "3", "4")
level   <- "lvl_1"
# Define occurrence name: it is necessary to specify the group, e.g. Group = "3"
occName <- level_1[1]

# Load all packages and functions needed to develop the analysis
source(paste(srcDir, "/02_sdm_modeling/preprocessing/config.R", sep = ""))
# Loading the library to prepare the input data for the SDM
library(usdm)

# Function to crop all rasters using a region mask extent (Just need to be run when you start the first group analysis)
# crop_raster(mask   = mask, region = region)

# Function to prepare passport data to run all the code
# Please verify the column position where the groups are defined, e.g. Column: 3
# A window will popup to select the .csv input file
# Input file: a .csv file with at least: longitude, latitude, and the grouping
prepare_input_data(data_path = choose.files( caption = "Select a valid .csv file"), 
                   col_number         = NULL,  # Column position where the groups are
                   do.ensemble.models = FALSE, # Run classification models to measure the environmental separation of the classes
                   add.latitude       = FALSE, # Just for classification models: if you want to add latitude as a predictor
                   add.longitude      = FALSE, # Just for classification models: if you want to add longitude as a predictor
                   do.predictions     = FALSE, # Just for classification models: if you want to predict some accessions that do not have the classes or groups
                   sampling_mthd      = "none", # Just for classification models: for balancing the classes
                   mask               = mask)  # Mask according with the region of analysis
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/input_data/african_maize_lvl_1_bd.csv

# Function to prepare the input file and convert it to a Spatial valid format
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/input_data/african_maize_lvl_1_bd.csv
create_occ_shp(file_path   = paste0(classResults, "/", crop, "_lvl_1_bd.csv"),
               file_output = paste0(occDir,"/Occ.shp"),
               validation  = FALSE)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/input_data/Occ.csv
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/input_data/Occ.shp
# Output file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.csv
# Output file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.shp

# Function to estimate the cost distance according with the level of analysis
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.shp
# Input file: e.g. ./input_data/auxiliar_rasters/friction_surface.tif
cost_dist_function(outDir   = gap_outDir,
                   friction = friction,
                   mask     = mask,
                   occDir   = occDir,
                   arcgis   = FALSE,
                   code     = paste0(sp_Dir_input, "/cost_dist.py"))
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/cost_dist.tif

# Function for preparing which variables will be selected to run SDMs and creation of SWD files
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.csv
# Input file: e.g. ./input_data/mask/mask_africa.tif
# Input file: e.g. ./input_data/generic_rasters/africa (Generic rasters)
# Input file: e.g. ./input_data/by_crop/african_maize/raster/africa (Specific rasters by crop)
var_names   <- model_driver(sp_Dir      = sp_Dir,
                            mask        = mask,
                            occName     = occName,
                            extension_r = ".tif",
                            all         = F,
                            overwrite   = T,
                            clsModel    = "ensemble",
                            correlation = 3 # 1. Correlation, 2. VIF, 3. PCA + VIF
                            )
# Output file: ./results/african_maize/lvl_1/3/africa/sdm_variables_selected.csv (Selected variables to do SDM)
# Output file: ./results/african_maize/lvl_1/3/africa/input_data/pseudo_abs_file_3.csv (Pseudo-absences created)
# Output file: ./input_data/by_crop/african_maize/lvl_1/3/africa/background/background_3.shp (Pseudo-absences created)
# Output file: ./input_data/by_crop/african_maize/lvl_1/3/africa/background/bg_3.shp (Pseudo-absences created)
# Output file: ./input_data/by_crop/african_maize/lvl_1/3/africa/swd/swd_3.csv (Samples with data)
# Output file: ./input_data/by_crop/african_maize/lvl_1/3/africa/swd/swd_Complete_3.csv (Samples with data)

# Loading SWD file and occurrence data
swdFile          <- paste0(swdDir, "/swd_", occName, ".csv")
spData           <- read.csv(swdFile)
spData           <- spData[,c(3:ncol(spData))]
names(spData)[1] <- occName
# Define use of maxnet
use.maxnet <- TRUE

# Define default parameters for maxnet model
beta <- 1
feat <- "lpqh"

# Loading environmental raster files
clim_vars     <- paste0(var_names, ".tif") %in% list.files(climDir, pattern = ".tif$") 
generic_vars  <- paste0(var_names, ".tif") %in% list.files(clim_spReg, pattern = ".tif$")
clim_layer    <- lapply(paste0(climDir, "/", var_names[clim_vars], ".tif"), raster)
generic_layer <- lapply(paste0(clim_spReg,"/", var_names[generic_vars],".tif"), raster)
if(is.null(generic_layer)){
  clim_layer  <- raster::stack(clim_layer)
} else {
  clim_layer  <- raster::stack(c(clim_layer, generic_layer))
}

# Function to develop the Spatial Distribution Modelling (SDM)
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/swd/swd_3.csv (Samples with data file)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/sdm_variables_selected.csv (Selected variables to do SDM)
# Input file: clim_layer (environmental information)
if(use.maxnet){
cat("Running sdm modelling approach using Maxent \n")
sdm_maxnet_approach_function(occName      = occName,
                             spData       = spData,
                             var_names    = var_names,
                             model_outDir = model_outDir,
                             sp_Dir        = sp_Dir,
                             clim_layer   = clim_layer,
                             nFolds       = 5,
                             beta         = beta,
                             feat         = feat,
                             doSDraster   = TRUE,
                             varImp       = TRUE,
                             validation   = FALSE)
} else {
  # Running SDMs
  cat("Running sdm modelling approach \n")
  m2 <- sdm_approach_function(occName      = occName,
                              spData       = spData,
                              model_outDir = sp_Dir,
                              var_names    = var_names,
                              nCores       = 4,
                              nFolds       = 5,
                              beta         = beta,
                              feat         = feat)
  
  # Model evaluation per replicates (nReplicates x 5)
  cat("Evaluating models performance\n")
  m2_eval <- evaluation_function(m2, eval_sp_Dir, spData)
  
  # Model projecting
  cat("Projecting models\n")
  svPth <- paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region, "/prj_models/replicates")
  
  models <- lapply(X = 1:5, FUN = function(i){
    cat("Projectin model", i, " to a raster object \n")
    p <- raster::predict(m2@models[[1]]$maxent[[i]]@object, clim_layer, type = "cloglog", progress='text')
    p_tst <- p
    p_tst[p_tst[] <= m2_eval$threshold[i]] <- NA
    writeRaster(p, paste0(svPth, "/", occName, "_prj_rep-", i, ".tif"))
    writeRaster(p_tst, paste0(svPth, "/", occName, "_prj_th_rep-", i, ".tif"))
    
  })
  
  prj_stk <- raster::stack(models)
  
  cat("Calculating mean, median and sd for replicates \n")
  mean(prj_stk, na.rm = TRUE) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_mean.tif" ), overwrite = TRUE)
  cat("Mean raster calculated \n")
  raster::calc(prj_stk, fun = function(x) {median(x, na.rm = T)}) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_median.tif" ), overwrite = TRUE)
  cat("Median raster calculated \n")
  raster::calc(prj_stk, fun = function(x) {sd(x, na.rm = T)}) %>% writeRaster(., paste0(model_outDir,"/", occName, "_prj_std.tif" ), overwrite = TRUE)
  cat("Sd raster calculated \n")
  
  # Final evaluation table
  cat("Validating model\n")
  if(!file.exists(paste0(eval_sp_Dir, "/Final_evaluation.csv"))){
    m2_eval_final <- final_evaluation(m2_eval, occName)
  } else {
    m2_eval_final <- read.csv(paste0(eval_sp_Dir, "/Final_evaluation.csv"), header = T)
  }
  
}
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/3_prj_mean.tif
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/3_prj_median.tif
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/3_prj_std.tif
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/replicates/3_prj_rep-[1:5].tif (Five model repetitions complete distribution)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/replicates/3_prj_th_rep-[1:5].tif (Five model repetitions thresholded distribution)

# Function to create the kernel density map for the acccession
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.shp
cat("Calculating kernel Density \n")
if(!file.exists(paste0(gap_outDir, "/kernel.tif"))){
  spData <- readOGR(dsn = occDir, layer = "Occ")
  spData <- unique(as.data.frame(spData))
  rownames(spData) <- 1:nrow(spData)
  names(spData)[2:3] <- c("lon", "lat")
  spData[,1] <- 1
  kernel <- raster_kernel(mask          = mask,
                          occurrences   = spData[spData[,1] == 1,],
                          out_dir       = gap_outDir,
                          kernel_method = 2,
                          scale         = T)
} else {
  kernel <- raster(paste0(gap_outDir, "/kernel.tif")) 
}
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/kernel.tif

# Function to create density categories
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/kernel.tif
if(!file.exists(paste0(gap_outDir, "/kernel_classes.tif"))){
  kernel <- raster(paste0(gap_outDir, "/kernel.tif"))
  kernel[kernel[] == 0] <- NA
  kernel <- kernel * 10000
  qVal_1 <- raster::quantile(x = kernel[], probs = c(.9, 1), na.rm = T)
  knl_temp <- kernel
  knl_temp[which(knl_temp[] <= qVal_1[1])] <- NA
  qVal_2 <- raster::quantile(x = knl_temp, probs = c(.6, .95), na.rm = TRUE)
  kernel_class <- raster::reclassify(kernel, c(-Inf,qVal_1[1],1, qVal_1[1],qVal_2[2],2, qVal_2[2],Inf,3))
  
  writeRaster(kernel_class, paste0(gap_outDir, "/kernel_classes.tif"), format = "GTiff")
  rm(kernel, kernel_class, knl_temp, qVal_1, qVal_2); gc()
}
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/kernel_classes.tif

# Function to calculate environmental distance and environmental score
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/swd/swd_3.csv (Samples with data file)
# Input file: e.g. ./input_data/generic_rasters/africa (Generic rasters)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/3_prj_median.tif (Estimated SDM)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/sdm_variables_selected.csv (Selected variables)
calc_env_score(lv_name     = occName,
               clus_method = "hclust_mahalanobis",
               sdm_dir     = sp_Dir,
               gap_dir     = gap_outDir,
               occ_dir     = occDir,
               env_dir     = climDir,
               out_dir     = gap_outDir,
               var_names   = var_names)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/env_score_hclust_mahalanobis.tif (Environmental score)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/ecogeo_hclust_mahalanobis.tif (Created clusters)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/euclidean_dist_hclust_mahalanobis.tif (Euclidean distance)

# Function to calculate the Delaunay triangulation and the geographical score
# Input file: e.g. ./input_data/by_crop/african_maize/lvl_1/3/africa/occurrences/Occ.shp
calc_delaunay_score(baseDir    = baseDir,
                    area       = region,
                    group      = occName,
                    crop       = crop,
                    lvl        = level,
                    ncores     = NULL,
                    validation = FALSE,
                    dens.level = "high_density",
                    pnt        = NULL)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/delaunay/raw_delaunay.shp (Delaunay polygons)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/delaunay.tif (Geographical score by Delaunay)

# Function for calculating the gap score by cost distance and Delaunay
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/prj_models/3_prj_median.tif (SDM)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/env_score_hclust_mahalanobis.tif (Environmental score)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/cost_dist.tif (Geo score: cost distance)
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/delaunay.tif (Geo score: Delaunay)
lapply(c("delaunay", "cost_dist"), function(x){

  calc_gap_score(lv_name     = occName,
                 clus_method = "hclust_mahalanobis",
                 gap_method  = x, # Can be: "cost_dist", "kernel", "delaunay"
                 sdm_dir     = model_outDir,
                 gap_dir     = gap_outDir,
                 out_dir     = gap_outDir)
  
})
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_score_cost_dist.tif
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_score_delaunay.tif

# Function to do validation process creating 5 artificial gaps
# It creates by itself the occurrence data removing some points in high density area randomly
# and calculates all the metrics in order to estimate those artificial gaps as a actual gap
validation_process(occName         = occName,
                   gap_valDir      = gap_valDir,
                   buffer_radius   = 1, # Radius of 100 km for excluding occurrences
                   density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                   geo_score       = c("cost_dist", "delaunay"),
                   use.Arcgis      = FALSE,
                   n.points        = 5,
                   doPar           = FALSE, # Whether or not parallelize
                   use.maxnet      = use.maxnet)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_validation/buffer_100km (all the results are stored there)

# Function to summarize all validation results in order to estimate the proper threshold for
# the gap score cost distance and the gap score by Delaunay
summary_function(area       = region,
                 group      = occName,
                 crop       = crop,
                 lvl        = "lvl_1",
                 pnt        = paste0("pnt", 1:5),
                 filename   = c("gap_score_cost_dist.tif"   ,"gap_score_delaunay.tif"),
                 radius     = seq(55,85, 5), #number of radius size to evaluate
                 baseDir    = baseDir,
                 dens.level = "high_density",
                 ncores     = 3)
# Ouput file: e.g. ./results/african_maize/lvl_1/2/africa/gap_validation/buffer_100km/validation_results.xlsx (thresholds and metrics)


# Function to create png graphs for all rasters and also calculate the thresholded maps and the final gap map
# Input file: e.g. ./results/african_maize/lvl_1/2/africa/gap_validation/buffer_100km/validation_results.xlsx
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_score_cost_dist.tif
# Input file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_score_delaunay.tif
create_png_maps( summ_filepath= paste0(gap_valDir, "/buffer_100km/validation_results.xlsx"), 
                 rast_dirPath = paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region, "/gap_models"),
                 grph_dir     = paste0(results_dir, "/", crop, "/lvl_1/", occName, "/", region, "/graphics"),
                 occName      = occName,
                 sdm_filepath = paste0(model_outDir, "/", occName, "_prj_median.tif"),
                 occ_filepath = paste0(occDir, "/Occ.shp"), 
                 colors       = list(two_cols =  c('grey70', 'red2') , three_cols = c('grey70', 'goldenrod3', 'red2')), 
                 new_ext        = NULL)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/graphics
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_class_cost_dist.tif (Thresholded gap map cost distance approach)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_class_delaunay.tif (Thresholded gap map Delaunay approach)
# Output file: e.g. ./results/african_maize/lvl_1/3/africa/gap_models/gap_class_final.tif (Final outcome)
