# Landrace gap analysis: base code
# Chrystian Sosa, Julian Ramirez-Villegas, Harold Achicanoy
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
region <- "americas"                           # Region: "americas", "world"
raster::rasterOptions(tmpdir = choose.dir(default = "",
                                          caption = "Please select the temporary folder")) # Temporary files directory

source(paste0(srcDir, "/02_sdm_modeling/preprocessing/config_crop.R")) # Configuring crop directories

# Define crop, analysis level and creating needed directories
crop <- "common_bean"
level_1 <- c("andean", "mesoamerican") # level 1: genepool
level_2 <- c("nueva_granada", "peru", "chile", "durango-Jalisco", "mesoamerica","guatemala") # level 2: race
level_3 <- NULL # level 3
level   <- "lvl_1"
occName <- "mesoamerican" # Level 1: "andean", "mesoamerican"
source(paste(srcDir, "/02_sdm_modeling/preprocessing/config.R", sep = ""))
# config_crop_dirs(baseDir, crop, level_1, level_2, level_3)

# Pre-process classified data
if(!file.exists(paste0(classResults, "/genepool_predicted_original.csv"))){
  accessions_predicted <- read.csv(paste0(classResults, "/genepool_predicted.csv"))
  accessions_predicted$Genepool.interpreted.ACID <- as.character(accessions_predicted$Genepool.interpreted.ACID)
  accessions_predicted$ensemble[!is.na(accessions_predicted$Genepool.interpreted.ACID)] <- accessions_predicted$Genepool.interpreted.ACID[!is.na(accessions_predicted$Genepool.interpreted.ACID)]
  accessions_predicted$X <- accessions_predicted$Row.names <- NULL
  accessions_predicted$ensemble <- as.character(accessions_predicted$ensemble)
  accessions_predicted$ensemble[which(accessions_predicted$Country == "Chile")] <- "Andean"
  file.rename(from = paste0(classResults, "/genepool_predicted.csv"), to = paste0(classResults, "/genepool_predicted_original.csv"))
  write.csv(accessions_predicted, paste0(classResults, "/genepool_predicted.csv"), row.names = F)
  rm(accessions_predicted)
}
if(file.exists(paste0(classResults, "/genepool_predicted_original.csv")) &
   file.exists(paste0(classResults, "/genepool_predicted.csv"))){
  
  if(!file.exists(paste0(classResults, "/genepool_predicted_all.csv"))){
    
    accessions_predicted <- read.csv(paste0(classResults, "/genepool_predicted.csv"))
    file.rename(from = paste0(classResults, "/genepool_predicted.csv"), to = paste0(classResults, "/genepool_predicted_all.csv"))
    accessions_predicted <- accessions_predicted[accessions_predicted$status == "G",]
    rownames(accessions_predicted) <- 1:nrow(accessions_predicted)
    write.csv(accessions_predicted, paste0(classResults, "/genepool_predicted.csv"), row.names = F)
    rm(accessions_predicted)
    
  }
  
}

# Cost distance process according with the level of analysis
cost_dist_function(code         = paste0(sp_Dir_input, "/cost_dist.py"),
                   outDir       = gap_outDir,
                   friction     = friction,
                   classResults = classResults,
                   occName      = occName,
                   mask         = mask,
                   occDir       = occDir)

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

# Loading raster files
clim_layer <- lapply(paste0(climDir, "/", var_names, ".tif"), raster)
clim_layer <- raster::stack(clim_layer)

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
cat("Running modeling approach\n")
m2 <- sdm_approach_function(occName      = occName,
                            spData       = spData,
                            model_outDir = sp_Dir,
                            var_names    = var_names,
                            nCores       = 5,
                            nFolds       = 5,
                            beta         = beta,
                            feat         = feat)

# Model evaluation per replicates (nReplicates x 5)
cat("Evaluating models performance\n")
m2_eval <- evaluation_function(m2, eval_sp_Dir, spData)

# Model projecting
cat("Projecting models\n")
clim_table <- raster::as.data.frame(clim_layer, xy = T)
clim_table <- clim_table[complete.cases(clim_table),]
model      <- projecting_function(m2, m2_eval, clim_table, mask, model_outDir, nCores = 5, obj.size = 3)

# Final evaluation table
cat("Validating model\n")
if(!file.exists(paste0(eval_sp_Dir, "/Final_evaluation.csv"))){
  m2_eval_final <- final_evaluation(m2_eval, occName)
} else {
  m2_eval_final <- read.csv(paste0(eval_sp_Dir, "/Final_evaluation.csv"), header = T)
}

# Kernel function #1 spatstat, #2 Adehabitat, #3 Kernsmooth
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
  rm(kernel, kernel_class, qVals); gc()
} else {
  kernel_class <- raster(paste0(gap_outDir, "/kernel_classes.tif")) 
}

# Calculating environmental distance
calc_env_score(lv_name     = occName,
               clus_method = "hclust_mahalanobis",
               sdm_dir     = model_outDir,
               gap_dir     = gap_outDir,
               occ_dir     = occDir,
               env_dir     = climDir,
               out_dir     = gap_outDir)

# Calculating Delaunay triangulation
calc_delaunay_score(baseDir    = baseDir,
                    area       = region,
                    group      = occName,
                    crop       = crop,
                    lvl        = level,
                    ncores     = 10,
                    validation = FALSE,
                    pnt        = NULL)

# Calculating gap metrics
calc_gap_score(lv_name     = occName,
               clus_method = "hclust_mahalanobis",
               gap_method  = "delaunay", # Can be: "cost_dist", "kernel", "delaunay"
               sdm_dir     = model_outDir,
               gap_dir     = gap_outDir,
               out_dir     = gap_outDir)
