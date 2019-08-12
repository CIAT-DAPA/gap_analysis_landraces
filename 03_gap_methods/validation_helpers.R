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


cat(">>> Load same variables for SDM ... \n")
vars <- read.csv(paste0(sp_Dir, "/sdm_variables_selected.csv"))
vars <- as.character(vars$x)

cat(">>> Load calibrated feature parameters for SDM ... \n")

params_tunned <- Calibration_function(spData = spData,
                                      sp_Dir = sp_Dir, 
                                      ommit = F, 
                                      use.maxnet = TRUE)


occSDM <- read.csv(paste0(sp_Dir_input, "/swd/swd_", occName, ".csv"))
occSDM <- occSDM[,3:ncol(occSDM)]
names(occSDM)[1] <- occName
id_sdm   <- raster::extract(x = radius, y = occSDM[occSDM[,1] == 1, c("lon", "lat")])
sdm_excl <- occSDM[which(id_sdm == 1),]
occSDM_upt <- occSDM[base::setdiff(1:nrow(occSDM), which(id_sdm == 1)),]; rm(id_sdm, sdm_excl)


  cat("Running sdm modelling approach using Maxnet \n")
  cat("      This process can take several minutes...... Be patient \n \n ")
  if(!file.exists(paste0(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models/",occName,"_prj_median.tif")))){
    sdm_maxnet_approach_function(occName      = occName,
                                 spData       = occSDM_upt,
                                 var_names    = vars,
                                 model_outDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                 sp_Dir       = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                                 nFolds       = 5,
                                 beta         = params_tunned$beta,
                                 feat         = params_tunned$features,
                                 doSDraster   = FALSE,
                                 varImp       = FALSE,
                                 validation   = FALSE)
    
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
  

lapply(list("delaunay", "cost_dist", "environ"), function(x){
  
  if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))){
    cat(">>> Calculating gap indicator ...\n")
    calc_gap_score(lv_name = occName,
                   clus_method = "hclust_mahalanobis",
                   gap_method = x, # Can be: "cost_dist", "kernel", "delaunay"
                   sdm_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/02_sdm_results/prj_models"),
                   gap_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                   out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"))
    #gap_score <- raster(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/gap_score_", x, ".tif"))
  } 

})

if(!file.exists(paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models/kernel.tif"))){
  cat(">>> Creating a new kernel density ...\n")
  kernel <- raster_kernel(mask = mask,
                          occDir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/01_selected_points"), # spData_upt[spData_upt[,1] == 1,],
                          out_dir = paste0(gap_valDir, "/buffer_100km/", densities[density_pattern], "_density/pnt", i, "/03_gap_models"),
                          kernel_method = 2,
                          scale = T)
} else {
  cat("Kernel raster has already been created ... \n")
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