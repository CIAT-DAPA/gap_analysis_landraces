#### all functions to create neccessary files to validation


cat(">>> Processing point", i, "... \n")
set.seed(seedList[i])
#probList <- raster::extract(x = kernel_upt, y = occ[,c("lon", "lat")])

g <- gc(); rm(g)

#probList[is.na(probList)] <- 0
pnt <- base::sample(x = 1:nrow(points_to_sample), size = 1, replace = F)

if(!file.exists(paste0(gap_valDir, "/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/buffer_radius_to_omit_shp.shp"))){
  cat("Point has not been created, processing ... \n")
  cat(">>> Create a buffer around the point", i, "... \n")
  radius <- create_buffers(xy        = points_to_sample[pnt, c("lon", "lat")],
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

cat(">>> Creating occurrences shapefile... \n")

create_occ_shp(file_path   = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points", "/", crop, "_lvl_1_bd.csv"),
                          file_output = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points","/Occ.shp"),
                          validation  = TRUE)

if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/cost_dist.tif"))){
cat(">>> Creating cost distance raster ... \n")
cost_dist_function(
  outDir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
  friction     = friction,
  mask         = mask,
  occDir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
  arcgis       = use.Arcgis,
  code         = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points/cost_dist.py")
)
}

cat(">>> Load same variables for SDM ... \n")
vars <- read.csv(paste0(sp_Dir, "/sdm_variables_selected.csv"))
vars <- as.character(vars$x)

cat(">>> Load calibrated feature parameters for SDM ... \n")
calibration <- read.csv(paste0(sp_Dir, "/calibration.csv"))
feat <- CreateMXArgs(calibration,  use.maxnet = TRUE)
beta <- feat[(grepl("betamultiplier=", feat))]; beta <- as.numeric(gsub("betamultiplier=", "", beta))
feat <- feat[(!grepl("betamultiplier=", feat))]
rm(calibration)

occSDM <- read.csv(paste0(sp_Dir_input, "/swd/swd_", occName, ".csv"))
occSDM <- occSDM[,3:ncol(occSDM)]
names(occSDM)[1] <- occName
id_sdm   <- raster::extract(x = radius, y = occSDM[occSDM[,1] == 1, c("lon", "lat")])
sdm_excl <- occSDM[which(id_sdm == 1),]
occSDM_upt <- occSDM[base::setdiff(1:nrow(occSDM), which(id_sdm == 1)),]; rm(id_sdm, sdm_excl)

clim_vars <-  paste0(var_names, ".tif")  %in% list.files(climDir) 
generic_vars <- paste0(var_names, ".tif") %in% list.files(clim_spReg)
clim_layer <- lapply(paste0(climDir, "/", var_names[clim_vars], ".tif"), raster)
generic_layer <- lapply(paste0(clim_spReg,"/", var_names[generic_vars],".tif"), raster)
clim_layer <- raster::stack(c(clim_layer, generic_layer))

if(use.maxnet){
  
  cat("Running sdm modelling approach using Maxnet \n")
  cat("      This process can take several minutes...... Be patient \n \n ")
  if(!file.exists(paste0(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/",occName,"_prj_median.tif")))){
    sdm_maxnet_approach_function(occName      = occName,
                                            spData       = occSDM_upt,
                                            var_names    = vars,
                                            model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                            sp_Dir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                            clim_layer   = clim_layer,
                                            nFolds       = 5,
                                            beta         = beta,
                                            feat         = feat,
                                            doSDraster   = FALSE,
                                            varImp       = FALSE,
                                            validation   = FALSE)
    
  }
  
}else{
  
  cat(">>> Running maxent model approach, evaluate and project ...\n")
  m2 <- sdm_approach_function(occName = occName,
                                         spData = occSDM_upt,
                                         model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                                         var_names = vars,
                                         nCores = 5,
                                         nFolds = 5,
                                         beta = beta,
                                         feat = feat)
  
  m2_eval <- evaluation_function(m2,
                                            eval_sp_Dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/evaluation"),
                                            spData = occSDM_upt)
  model_outDir_rep <- paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/replicates")
  
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/", occName, "_prj_median.tif"))){
    
    # clim_table <- raster::as.data.frame(clim_layer, xy = T)
    # clim_table <- clim_table[complete.cases(clim_table),]
    # model <- .GlobalEnv$projecting_function(m2,
    #                                         m2_eval,
    #                                         clim_table,
    #                                         mask,
    #                                         model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
    #                                         nCores = 5,
    #                                         obj.size = 3,
    #                                         s.dev = FALSE)
    # rm(clim_table); g <- gc(); rm(g)
    # 
    # 
    
    svPth <- paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models")
    
    models <- lapply(X = 1:5, FUN = function(i){
      cat("Projectin model", i, " to a raster object \n")
      p <- raster::predict(m2@models[[1]]$maxent[[i]]@object, clim_layer, type = "cloglog", progress='text')
      p_tst <- p
      p_tst[p_tst[] <= m2_eval$threshold[i]] <- NA
      writeRaster(p, paste0(svPth, "/replicates/", occName, "_prj_rep-", i, ".tif"))
      writeRaster(p_tst, paste0(svPth, "/replicates/", occName, "_prj_th_rep-", i, ".tif"))
      
    })
    
    prj_stk <- raster::stack(models)
    
    cat("Calculating mean, median and sd for replicates \n")
    mean(prj_stk, na.rm = TRUE) %>% writeRaster(., paste0(svPth,"/", occName, "_prj_mean.tif" ), overwrite = TRUE)
    cat("Mean raster calculated \n")
    raster::calc(prj_stk, fun = function(x) {median(x, na.rm = T)}) %>% writeRaster(., paste0(svPth,"/", occName, "_prj_median.tif" ), overwrite = TRUE)
    cat("Median raster calculated \n")
    raster::calc(prj_stk, fun = function(x) {sd(x, na.rm = T)}) %>% writeRaster(., paste0(svPth,"/", occName, "_prj_std.tif" ), overwrite = TRUE)
    cat("Sd raster calculated \n")
    
  } else {
    cat("SDM model has already been created ... \n")
  }
  
  
  
}


if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/kernel.tif"))){
  cat(">>> Creating a new kernel density ...\n")
  kernel <- raster_kernel(mask = mask,
                                     occurrences = spData_upt, # spData_upt[spData_upt[,1] == 1,],
                                     out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                                     kernel_method = 2,
                                     scale = T)
} else {
  cat("Kernel raster has already been created ... \n")
}

if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/env_score_hclust_mahalanobis.tif"))){
  cat(">>> Creating a new environmental distance ...\n")
  calc_env_score(lv_name = occName,
                            clus_method = "hclust_mahalanobis",
                            sdm_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results"),
                            gap_dir = gap_outDir,
                            occ_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"),
                            env_dir = climDir,
                            out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                            var_names = var_names)
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
                      pnt        = paste0("pnt", i),
                      dens.level = "high_density")
} else {
  cat("Delanuay triangulation score has already been created ... \n")
}

lapply(geo_score, function(x){
  
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))){
    cat(">>> Calculating gap indicator ...\n")
    calc_gap_score(lv_name = occName,
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