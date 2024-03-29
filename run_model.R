# Landrace gap analysis: base code
# Chrystian Sosa, Julian Ramirez-Villegas, Harold Achicanoy, Andres camilo mendez, Maria Victoria, colin khuory
# CIAT, March 2018

# R options

g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

#####################################
##### LOADING PACKAGES ##############
#####################################

suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
pacman::p_load(tcltk, adehabitatHR,   raster, rgdal, doSNOW, sdm, dismo,  rgeos, distances,   sp, 
               tidyverse, rlang, sf, gdistance, caret, earth, fastcluster, xlsx,  FactoMineR, deldir,
               parallelDist, bindrcpp, foreach, doParallel,  pROC, maxnet)


# Base directory
OSys <- Sys.info()[1]
baseDir   <- switch(OSys,
                   "Linux"   = "/mnt/workspace_cluster_9/gap_analysis_landraces/runs",
                   "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs",
                   "Darwin"  = "~nfs/workspace_cluster_9/gap_analysis_landraces/runs")
rm(OSys)

srcDir <- paste(baseDir, "/scripts", sep = "") # Software directory
region <- "rice_custom"                         # Region: "americas", "world"

source(paste0(srcDir, "/00_config/config_crop.R")) # Configuring crop directories

# Define crop, analysis level and creating needed directories
crop <- "rice_asia"
level_1 <-  c("japonica", "indica", "aus", "aromatic")#c("ceusa", "cobra", "hiand", "himex", "lomam", "wemex") # level 1: genepool
level   <- "lvl_1"
occName <- level_1[1] # Level 1: "andean", "mesoamerican"
source(paste(srcDir, "00_config/config.R", sep = ""))
# config_crop_dirs(baseDir, crop, level_1, level_2, level_3)

#crop all raster using region mask extent (OPTIONAL)
crop_raster(mask   = mask,
            region = region )

#prepare input data
prepare_input_data(data_path = choose.files( caption = "Select a valid .csv file"), 
                   col_number = NULL, 
                   do.ensemble.models = TRUE,
                   add.latitude = FALSE,
                   add.longitude = FALSE,
                   do.predictions = TRUE,
                   sampling_mthd = "none",
                   mask = mask)

#prepare the input file and convert it to a Spatial valid format
create_occ_shp(file_path   = paste0(classResults, "/", crop, "_lvl_1_bd.csv"),
               file_output = paste0(occDir,"/Occ.shp"),
               validation  = FALSE)

# Cost distance process according with the level of analysis
cost_dist_function(
                   outDir       = gap_outDir,
                   friction     = friction,
                   mask         = mask,
                   occDir       = occDir,
                   arcgis       = TRUE,
                   code         = paste0(sp_Dir_input, "/cost_dist.py")
                   )

# Model driver function for preparing which variables will be selected to run SDMs and creation of SWD files
var_names   <- model_driver(sp_Dir      = sp_Dir,
                            mask        = mask,
                            occName     = occName,
                            extension_r = ".tif",
                            all         = F,
                            overwrite   = T,
                            clsModel    = "ensemble",
                            correlation = 3# 1. Correlation, 2. VIF, 3. PCA + VIF
                            )

# Loading SWD file and occurrence data
swdFile          <- paste0(swdDir, "/swd_", occName, ".csv")
spData           <- read.csv(swdFile)
spData           <- spData[,c(3:ncol(spData))]
names(spData)[1] <- occName
use.maxnet <- TRUE

# Calibrate parameter for MAXENT
cat("Performing calibration step","\n")
if(file.exists(paste0(sp_Dir, "/calibration.csv"))){
  calibration <- read.csv(paste0(sp_Dir, "/calibration.csv"))
  feat <- CreateMXArgs(calibration)
  beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
  feat <- feat[(!grepl("betamultiplier=", feat))]
  rm(calibration)
} else {
  feat <- Calibration_function(spData = spData, save = T, sp_Dir = sp_Dir, ommit = F, use.maxnet = use.maxnet )
  beta <- feat[(grepl("betamultiplier=", feat))]
  beta <- as.numeric(gsub("betamultiplier=", "", beta))
  feat <- feat[(!grepl("betamultiplier=", feat))]
}

# Loading raster files
clim_vars <-  paste0(var_names, ".tif")  %in% list.files(climDir, pattern = ".tif$") 
generic_vars <- paste0(var_names, ".tif") %in% list.files(clim_spReg, pattern = ".tif$")
clim_layer <- lapply(paste0(climDir, "/", var_names[clim_vars], ".tif"), raster)
generic_layer <- lapply(paste0(clim_spReg,"/", var_names[generic_vars],".tif"), raster)
clim_layer <- raster::stack(c(clim_layer, generic_layer))

## Spatial Distribution Modelling
if(use.maxnet){
#Use MAXNET to run sdm 
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

}else{
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

}#end if

# Kernel function #1 spatstat, #2 Adehabitat, #3 Kernsmooth
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
  } #else {
#   kernel_class <- raster(paste0(gap_outDir, "/kernel_classes.tif")) 
# }

# Calculating environmental distance
calc_env_score(lv_name     = occName,
               clus_method = "hclust_mahalanobis",
               sdm_dir     = sp_Dir,
               gap_dir     = gap_outDir,
               occ_dir     = occDir,
               env_dir     = climDir,
               out_dir     = gap_outDir,
               var_names   = var_names)

# Calculating Delaunay triangulation
calc_delaunay_score(baseDir    = baseDir,
                    area       = region,
                    group      = occName,
                    crop       = crop,
                    lvl        = level,
                    ncores     = NULL,
                    validation = FALSE,
                    dens.level = "high_density",
                    pnt        = NULL)

# Calculating gap score
lapply(c("delaunay", "cost_dist"), function(x){

  calc_gap_score(lv_name     = occName,
                 clus_method = "hclust_mahalanobis",
                 gap_method  = x, # Can be: "cost_dist", "kernel", "delaunay"
                 sdm_dir     = model_outDir,
                 gap_dir     = gap_outDir,
                 out_dir     = gap_outDir)
  
})

#create all neccessary files to carry out the validation process

validation_process(occName = occName,
                   gap_valDir = gap_valDir,
                   buffer_radius = 1, # Radius of 100 km for excluding occurrences
                   density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                   geo_score = c("cost_dist", "delaunay"),
                   use.Arcgis = FALSE,
                   n.points = 5,
                   doPar    = TRUE,# whether or not parallelize
                   use.maxnet = use.maxnet)

#summarize all validation results
summary_function(area =region,
                 group = occName,
                 crop = crop,
                 lvl = "lvl_1",
                 pnt = paste0("pnt", 1:5),
                 filename = c("gap_score_cost_dist.tif"   ,"gap_score_delaunay.tif"),
                 radius = seq(55,85, 1), #number of radius size to evaluate
                 baseDir = baseDir,
                 dens.level = "high_density",
                 ncores = 15 #(detectCores()-8)
)


#create png graphs for all rasters

create_png_maps( summ_filepath= paste0(gap_valDir, "/buffer_100km/validation_results.xlsx"), 
                 rast_dirPath = paste0(results_dir, "/", crop, "/", level, "/", occName, "/", region, "/gap_models"),
                 grph_dir     = paste0(results_dir, "/", crop, "/lvl_1/", occName, "/", region, "/graphics"),
                 occName      = occName,
                 sdm_filepath = paste0(model_outDir, "/", occName, "_prj_median.tif"),
                 occ_filepath = paste0(occDir, "/Occ.shp"), 
                 colors       = list(two_cols =  c('grey70', 'red2') , three_cols = c('grey70', 'goldenrod3', 'red2')), 
                 new_ext        = NULL)
