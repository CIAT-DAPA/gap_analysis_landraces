# Validation process: landrace gap analysis
# Harold Achicanoy
# CIAT, March 2018


####################################################################################################################################
######################## FUNCTION TO CREATE ALL NECCESSARY FILES TO VALIDATE THE GAP SCORES ########################################
####################################################################################################################################


# cat(">>> Applying validation process for gap metrics <<<\n")
# gap_valDir <- paste0(baseDir, "/results/", crop, "/", level, "/", occName, "/", region,"/gap_validation")



allglobal <- function() {
  lss <- ls(envir = parent.frame())
  for (i in lss) {
    assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
  }
}
allglobal()

# occName = occName
# gap_valDir = gap_valDir
# buffer_radius = 1
# density_pattern = 3
# geo_score = c("cost_dist", "delaunay")

validation_process <- function(occName = occName,
                               gap_valDir = gap_valDir,
                               buffer_radius = 1, # Radius of 100 km for excluding occurrences
                               density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                               geo_score = c("cost_dist", "delaunay"),# Can be: "cost_dist", "kernel", "delaunay"
                               use.Arcgis = TRUE) 
{
  
  cat(">>> Loading occurrence data ... \n")
  spData <- raster::shapefile(paste0(occDir,"/Occ.shp"))
  spData <- unique(as.data.frame(spData)); rownames(spData) <- 1:nrow(spData)
  names(spData)[2:3] <- c("lon", "lat")
  
  cat(">>> Loading SDM raster... \n")
  SDM <- raster(paste0(model_outDir,"/", occName,"_prj_median.tif"))
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/kernel.tif"))
  kernel_class <- raster(paste0(sp_Dir, "/gap_models/kernel_classes.tif"))
  densities <- c("low", "medium", "high")
  
  kernel_class[kernel_class[] != density_pattern] <- NA
  kernel_upt <- raster::mask(x = kernel_class, mask = SDM)
  
  points_to_sample <- as.data.frame(kernel_upt, xy = TRUE, na.rm = TRUE)
  names(points_to_sample) <- c("lon", "lat", "value")
  
  
  rm(kernel_class, SDM); g <- gc(); rm(g)
  
  occ <- spData
  
  cat(">>> Selecting 5 points randomly using kernel density level as weights ... \n")
  set.seed(1234)
  seedList <- round(runif(n = 5, min = 1, max = 10000))
  
  # ----------------------------------------------------------------------------------- #
  allglobal()
  
  run_function <- function(i){
    
    cat(">>> Processing point", i, "... \n")
    set.seed(seedList[i])
    #probList <- raster::extract(x = kernel_upt, y = occ[,c("lon", "lat")])
    
    g <- gc(); rm(g)
    
    #probList[is.na(probList)] <- 0
    pnt <- base::sample(x = 1:nrow(points_to_sample), size = 1, replace = F)
    
    if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/buffer_radius_to_omit_shp.shp"))){
      cat("Point has not been created, processing ... \n")
      cat(">>> Create a buffer around the point", i, "... \n")
      radius <- .GlobalEnv$create_buffers(xy        = points_to_sample[pnt, c("lon", "lat")],
                                          msk       = mask,
                                          buff_dist = buffer_radius,
                                          format    = "GTiff",
                                          filename  = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"))
      
      cat(">>> Identify and exclude of the analysis points within the buffer radius ... \n")
      id_pnts <- raster::extract(x = radius, y = occ[,c("lon", "lat")])
      pnt_excl <- occ[which(id_pnts == 1),]
      write.csv(x = pnt_excl,
                file = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/coordinates_to_exclude.csv"),
                row.names = F)
      spData_upt <- occ[base::setdiff(1:nrow(occ), which(id_pnts == 1)),]; rm(id_pnts)
      write.csv(x = spData_upt,
                file = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/occ_", occName, ".csv"),
                row.names = F)
      
      spData_upt2 <- spData_upt[,c("lon", "lat")]
      names(spData_upt2) <- c("Longitude", "Latitude")
      spData_upt2$ensemble <- occName
      write.csv(x = spData_upt2,
                file = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/", crop, "_", level, "_bd.csv"),
                row.names = F)
      rm(spData_upt2)
    } else {
      radius <- raster::raster(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/buffer_radius_to_omit.tif"))
      cat("Point has been created, loading data ... \n")
      pnt_excl <- read.csv(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/coordinates_to_exclude.csv"))
      spData_upt <- read.csv(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/occ_", occName, ".csv"))
    }
    
    cat(">>> Creating cost distance raster ... \n")
    .GlobalEnv$cost_dist_function(
                                  outDir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                  friction     = friction,
                                  classResults = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                                  occName      = occName,
                                  mask         = mask,
                                  occDir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                                  filename     = paste0(crop, "_", level, "_bd.csv"),
                                  arcgis       = use.Arcgis,
                                  code         = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/cost_dist.py")
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
    cat("      This process can take several minutes...... Be patient \n \n ")
    m2 <- .GlobalEnv$sdm_approach_function(occName = occName,
                                           spData = occSDM_upt,
                                           model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                                           var_names = vars,
                                           nCores = 5,
                                           nFolds = 5,
                                           beta = beta,
                                           feat = feat)
    
    m2_eval <- .GlobalEnv$evaluation_function(m2,
                                              eval_sp_Dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/evaluation"),
                                              spData = occSDM_upt)
    model_outDir_rep <- paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/replicates")
    
    if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/", occName, "_prj_median.tif"))){
      clim_layer <- lapply(paste0(climDir, "/", vars, ".tif"), raster)
      clim_layer <- raster::stack(clim_layer)
      clim_table <- raster::as.data.frame(clim_layer, xy = T)
      clim_table <- clim_table[complete.cases(clim_table),]
      model <- .GlobalEnv$projecting_function(m2,
                                              m2_eval,
                                              clim_table,
                                              mask,
                                              model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                              nCores = 5,
                                              obj.size = 3,
                                              s.dev = FALSE)
      rm(clim_table); g <- gc(); rm(g)
    } else {
      cat("SDM model has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/kernel.tif"))){
      cat(">>> Creating a new kernel density ...\n")
      kernel <- .GlobalEnv$raster_kernel(mask = mask,
                                         occurrences = spData_upt, # spData_upt[spData_upt[,1] == 1,],
                                         out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                         kernel_method = 3,
                                         scale = T)
    } else {
      cat("Kernel raster has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/env_score_hclust_mahalanobis.tif"))){
      cat(">>> Creating a new environmental distance ...\n")
      .GlobalEnv$calc_env_score(lv_name = occName,
                                clus_method = "hclust_mahalanobis",
                                sdm_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                                gap_dir = gap_outDir,
                                occ_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                                env_dir = climDir,
                                out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"))
    } else {
      cat("Environmental score has already been created ... \n")
    }
    
    if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/delaunay.tif"))){
      cat(">>> Creating a new Delanuay triangulation ...\n")
      calc_delaunay_score(baseDir    = baseDir,
                          area       = region,
                          group      = occName,
                          crop       = crop,
                          lvl        = level,
                          ncores     = NULL,
                          validation = TRUE,
                          pnt        = paste0("pnt", i))
    } else {
      cat("Delanuay triangulation score has already been created ... \n")
    }
    
    lapply(geo_score, function(x){
     
       if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))){
        cat(">>> Calculating gap indicator ...\n")
        .GlobalEnv$calc_gap_score(lv_name = occName,
                                  clus_method = "hclust_mahalanobis",
                                  gap_method = x, # Can be: "cost_dist", "kernel", "delaunay"
                                  sdm_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                  gap_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                  out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"))
        gap_score <- raster(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))
      } else {
        gap_score <- raster(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))
      }
      
      
    })
    
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

# validation_process(occName = occName,
#                    gap_valDir = gap_valDir,
#                    buffer_radius = 1, # Radius of 100 km for excluding occurrences
#                    density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
#                    geo_score = c("cost_dist", "delaunay"))
