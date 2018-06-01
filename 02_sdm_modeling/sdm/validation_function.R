# Validation process: landrace gap analysis
# Harold Achicanoy
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
occName <- "andean" # Level 1: "andean", "mesoamerican"
source(paste(srcDir, "/02_sdm_modeling/preprocessing/config.R", sep = ""))


cat(">>> Applying validation process for gap metrics <<<\n")
gap_valDir <- paste0(baseDir, "/results/", crop, "/", level, "/", occName, "/", region,"/gap_validation/buffer_100km")

allglobal <- function() {
  lss <- ls(envir = parent.frame())
  for (i in lss) {
    assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
  }
}
allglobal()

occName = occName
gap_valDir = gap_valDir
buffer_radius = 1
density_pattern = 3
geo_score = "cost_dist"

validation_process <- function(occName = occName,
                               gap_valDir = gap_valDir,
                               buffer_radius = 1, # Radius of 100 km for excluding occurrences
                               density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                               geo_score = "cost_dist") # Can be: "cost_dist", "kernel", "delaunay"
{
  
  cat(">>> Loading occurrence data ... \n")
  spData <- rgdal::readOGR(dsn = occDir, layer = "Occ")
  spData <- unique(as.data.frame(spData)); rownames(spData) <- 1:nrow(spData)
  names(spData)[2:3] <- c("lon", "lat")
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/kernel.tif"))
  kernel_class <- raster(paste0(sp_Dir, "/gap_models/kernel_classes.tif"))
  
  densities <- c("low", "medium", "high")
  
  kernel_class[kernel_class[] != density_pattern] <- NA
  kernel_upt <- raster::mask(x = kernel, mask = kernel_class)
  
  occ <- spData
  
  cat(">>> Selecting 5 points randomly using kernel density level as weights ... \n")
  set.seed(1234)
  seedList <- round(runif(n = 5, min = 1, max = 10000))
  
  # ----------------------------------------------------------------------------------- #
  allglobal()
  
  run_function <- function(i){
    
    cat(">>> Processing point", i, "... \n")
    set.seed(seedList[i])
    probList <- raster::extract(x = kernel_upt, y = occ[,c("lon", "lat")])
    probList[is.na(probList)] <- 0
    pnt <- base::sample(x = 1:nrow(occ), size = 1, replace = F, prob = probList)
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/genepool_predicted.csv"))){
      cat("Point has not been created, processing ... \n")
      cat(">>> Create a buffer around the point", i, "... \n")
      radius <- .GlobalEnv$create_buffers(xy        = occ[pnt, c("lon", "lat")],
                                          msk       = mask,
                                          buff_dist = buffer_radius,
                                          format    = "GTiff",
                                          filename  = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"))
      
      cat(">>> Identify and exclude of the analysis points within the buffer radius ... \n")
      id_pnts <- raster::extract(x = radius, y = occ[,c("lon", "lat")])
      pnt_excl <- occ[which(id_pnts == 1),]
      write.csv(x = pnt_excl,
                file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/coordinates_to_exclude.csv"),
                row.names = F)
      spData_upt <- occ[base::setdiff(1:nrow(occ), which(id_pnts == 1)),]; rm(id_pnts)
      write.csv(x = spData_upt,
                file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/occ_", occName, ".csv"),
                row.names = F)
      
      spData_upt2 <- spData_upt[,c("lon", "lat")]
      names(spData_upt2) <- c("Longitude", "Latitude")
      spData_upt2$ensemble <- occName
      write.csv(x = spData_upt2,
                file = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/genepool_predicted.csv"),
                row.names = F)
      rm(spData_upt2)
    } else {
      radius <- raster::raster(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/buffer_radius_to_omit.tif"))
      cat("Point has been created, loading data ... \n")
      pnt_excl <- read.csv(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/coordinates_to_exclude.csv"))
      spData_upt <- read.csv(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/occ_", occName, ".csv"))
    }
    
    cat(">>> Creating cost distance raster ... \n")
    .GlobalEnv$cost_dist_function(code         = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/cost_dist.py"),
                                  outDir       = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                  friction     = friction,
                                  classResults = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                                  occName      = occName,
                                  mask         = mask,
                                  occDir       = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points")
    )
    
    cat(">>> Load same variables for SDM ... \n")
    vars <- read.csv(paste0(sp_Dir, "/sdm_variables_selected.csv"))
    vars <- as.character(vars$x)
    
    cat(">>> Load calibrated feature parameters for SDM ... \n")
    calibration <- read.csv(paste0(sp_Dir, "/calibration.csv"))
    feat <- .GlobalEnv$CreateMXArgs(calibration)
    beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
    feat <- feat[(!grepl("betamultiplier=", feat))]
    rm(calibration)
    
    occSDM <- read.csv(paste0(sp_Dir_input, "/swd/swd_", occName, ".csv"))
    occSDM <- occSDM[,3:ncol(occSDM)]
    names(occSDM)[1] <- occName
    id_sdm   <- raster::extract(x = radius, y = occSDM[occSDM[,1] == 1, c("lon", "lat")])
    sdm_excl <- occSDM[which(id_sdm == 1),]
    occSDM_upt <- occSDM[base::setdiff(1:nrow(occSDM), which(id_sdm == 1)),]; rm(id_sdm, sdm_excl)
    
    cat(">>> Running maxent model approach, evaluate and project ...\n")
    m2 <- .GlobalEnv$sdm_approach_function(occName = occName,
                                           spData = occSDM_upt,
                                           model_outDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                                           var_names = vars,
                                           nCores = 5,
                                           nFolds = 5,
                                           beta = beta,
                                           feat = feat)
    m2_eval <- .GlobalEnv$evaluation_function(m2,
                                              eval_sp_Dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/evaluation"),
                                              spData = occSDM_upt)
    model_outDir_rep <- paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/replicates")
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/", occName, "_prj_median.tif"))){
      clim_layer <- lapply(paste0(climDir, "/", vars, ".tif"), raster)
      clim_layer <- raster::stack(clim_layer)
      clim_table <- raster::as.data.frame(clim_layer, xy = T)
      clim_table <- clim_table[complete.cases(clim_table),]
      model <- .GlobalEnv$projecting_function(m2,
                                              m2_eval,
                                              clim_table,
                                              mask,
                                              model_outDir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                              nCores = 5,
                                              obj.size = 3)
    } else {
      cat("SDM model has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/kernel.tif"))){
      cat(">>> Creating a new kernel density ...\n")
      kernel <- .GlobalEnv$raster_kernel(mask = mask,
                                         occurrences = spData_upt, # spData_upt[spData_upt[,1] == 1,],
                                         out_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                         kernel_method = 3,
                                         scale = T)
    } else {
      cat("Kernel raster has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/env_score_hclust_mahalanobis.tif"))){
      cat(">>> Creating a new environmental distance ...\n")
      .GlobalEnv$calc_env_score(lv_name = occName,
                                clus_method = "hclust_mahalanobis",
                                sdm_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                gap_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                occ_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                                env_dir = climDir,
                                out_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"))
    } else {
      cat("Environmental score has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/env_score_hclust_mahalanobis.tif"))){
      cat(">>> Creating a new Delanuay triangulation ...\n")
      calc_delaunay_score(baseDir    = baseDir,
                          area       = region,
                          group      = occName,
                          crop       = crop,
                          lvl        = level,
                          ncores     = 10,
                          validation = TRUE,
                          pnt        = paste0("pnt", i))
    } else {
      cat("Delanuay triangulation score has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", geo_score, ".tif"))){
      cat(">>> Calculating gap indicator ...\n")
      .GlobalEnv$calc_gap_score(lv_name = occName,
                                clus_method = "hclust_mahalanobis",
                                gap_method = geo_score, # Can be: "cost_dist", "kernel", "delaunay"
                                sdm_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                gap_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                out_dir = paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"))
      gap_score <- raster(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", geo_score, ".tif"))
    } else {
      gap_score <- raster(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", geo_score, ".tif"))
    }
    
    # cat(">>> Calculating percentage of correctly classified occurrences ...\n")
    # library(tidyverse)
    # gap_values <- raster::extract(x = gap_score, y = pnt_excl[,c("lon", "lat")])
    # percentile <- seq(from = 0, to = 1, by = 0.001)
    # metrics <- tibble(Coordinates = list(pnt_excl[,c("lon","lat")]),
    #                   Gap_score = list(gap_values),
    #                   CC_points = unlist(purrr::map(.x = percentile, .f = function(z){sum(gap_values[complete.cases(gap_values)] > z)})),
    #                   PCC_points = unlist(purrr::map(.x = percentile, .f = function(z){sum(gap_values[complete.cases(gap_values)] > z)/length(gap_values[complete.cases(gap_values)])})),
    #                   Percentile = percentile)
    # saveRDS(metrics, paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/validation_metrics.RDS"))
    # 
    # return(cat("Done!\n"))
    
    return("Process done\n")
    
  }
  
  for(k in 1:5){
    cat(paste0(">>> Processing point: ", k, " <<<\n"))
    run_function(i = k)
  }
  
}

validation_process(occName = occName,
                   gap_valDir = gap_valDir,
                   buffer_radius = 1, # Radius of 100 km for excluding occurrences
                   density_pattern = 2, # Density pattern (1: low density, 2: medium density, 3: high density)
                   geo_score = "cost_dist")
