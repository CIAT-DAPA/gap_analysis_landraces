# Landrace gap analysis: base code
# Chrystian Sosa, Julian Ramirez-Villegas, Harold Achicanoy, Andres camilo mendez, Maria Victoria, colin khuory
# CIAT, March 2018

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

# Base directory
OSys <- Sys.info()[1]
baseDir   <- switch(OSys,
                   "Linux"   = "/mnt/workspace_cluster_9/gap_analysis_landraces/runs",
                   "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs",
                   "Darwin"  = "~nfs/workspace_cluster_9/gap_analysis_landraces/runs")
rm(OSys)

srcDir <- paste(baseDir, "/scripts", sep = "") # Software directory
region <- "sgh_custom"                           # Region: "americas", "world"

source(paste0(srcDir, "/02_sdm_modeling/preprocessing/config_crop.R")) # Configuring crop directories

# Define crop, analysis level and creating needed directories
crop <- "sorghum"
level_1 <-  c("bicolor", "guinea", "durra", "kafir", "caudatum") # level 1: genepool
level_2 <- NULL # level 2: race
level_3 <- NULL # level 3
level   <- "lvl_1"
occName <- "durra" # Level 1: "andean", "mesoamerican"
source(paste(srcDir, "/02_sdm_modeling/preprocessing/config.R", sep = ""))
# config_crop_dirs(baseDir, crop, level_1, level_2, level_3)
raster::rasterOptions(tmpdir = choose.dir(default = "",
                                          caption = "Please select the temporary folder")) # Temporary files directory

#crop all raster using region mask extent (OPTIONAL)
crop_raster(mask   = mask,
            region = region )

# Cost distance process according with the level of analysis
cost_dist_function(
                   outDir       = gap_outDir,
                   friction     = friction,
                   classResults = classResults,
                   occName      = occName,
                   mask         = mask,
                   occDir       = occDir,
                   filename     = paste0(crop, "_", level, "_bd.csv"),
                   arcgis       = FALSE,
                   code         = paste0(sp_Dir_input, "/cost_dist.py")
                   )

# Model driver function for preparing which variables will be selected to run SDMs and creation of SWD files
extension_r <- ".tif"
clsModel    <- "ensemble"
correlation <- 3 # 1. Correlation, 2. VIF, 3. PCA + VIF
var_names   <- model_driver(sp_Dir      = sp_Dir,
                            mask        = mask,
                            occName     = occName,
                            extension_r = extension_r,
                            all         = F,
                            overwrite   = T,
                            clsModel    = clsModel,
                            correlation = correlation)

# Loading SWD file and occurrence data
swdFile          <- paste0(swdDir, "/swd_", occName, ".csv")
spData           <- read.csv(swdFile)
spData           <- spData[,c(3:ncol(spData))]
names(spData)[1] <- occName


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

# Loading raster files
clim_layer <- lapply(paste0(climDir, "/", var_names, ".tif"), raster)
clim_layer <- raster::stack(clim_layer)

use.maxnet <- TRUE
if(use.maxnet){
#Use MAXNET to run sdm 
cat("Running sdm modelling approach using Maxent \n")
sdm_maxnet_approach_function(occName      = occName,
                             spData       = spData,
                             var_names    = var_names,
                             model_outDir = model_outDir,
                             sp_Dir        = spDir,
                             clim_layer   = clim_layer,
                             nFolds       = 5,
                             beta         = beta,
                             feat         = feat,
                             varImp       = TRUE)

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
clim_table <- raster::as.data.frame(clim_layer, xy = T)
rm(clim_layer); g <- gc(); rm(g)
clim_table <- clim_table[complete.cases(clim_table),]
model      <- projecting_function(m2, m2_eval, clim_table, mask, model_outDir, nCores = 5, obj.size = 3, s.dev = TRUE)

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
  spData <- unique(as.data.frame(spData)); rownames(spData) <- 1:nrow(spData)
  names(spData)[2:3] <- c("lon", "lat")
  spData[,1] <- 1
  kernel <- raster_kernel(mask          = mask,
                          occurrences   = spData[spData[,1] == 1,],
                          out_dir       = gap_outDir,
                          kernel_method = 3,
                          scale         = T)
} else {
  kernel <- raster(paste0(gap_outDir, "/kernel.tif")) 
}
if(!file.exists(paste0(gap_outDir, "/kernel_classes.tif"))){
  kernel <- raster(paste0(gap_outDir, "/kernel.tif"))
  kernel[kernel[] == 0] <- NA
  kernel <- kernel * 10000
  qVals <- raster::quantile(x = kernel[], probs = c(.60, .90), na.rm = T)
  kernel_class <- raster::reclassify(kernel, c(-Inf,qVals[1],1, qVals[1],qVals[2],2, qVals[2],Inf,3))
  writeRaster(kernel_class, paste0(gap_outDir, "/kernel_classes.tif"), format = "GTiff")
  rm(kernel, kernel_class); gc()
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
                    ncores     = 4,
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
                   use.maxnet = TRUE)

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
                 ncores = 2 #(detectCores()-8)
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
