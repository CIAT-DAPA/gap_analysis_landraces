# Maria Victoria, Harold Achicanoy
# CIAT, April 2019

crop     <- "wheat_durum"
classes  <- c("g1", "g2")
level    <- "runs_2019_07_environmental_groups"
region   <- "wheat_custom"
mask_dir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask"

calculate_gap_areas_per_country <- function(crop, classes, level, region, mask_dir){
  
  # Results directory
  results_dir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results"
  
  # Load packages
  library(pacman)
  pacman::p_load(raster, tidyverse, sp)
  
  # Load mask
  msk <- raster::raster(paste0(mask_dir,"/mask_",region,".tif" ))
  
  # # Load SDM models
  # sdm_models_1 <- lapply(1:length(classes), function(i){
  #   sdm_models<- raster(paste0(results_dir, "/", crop, "/", level, "/", classes[i], "/", region, "/prj_models/", classes[i],"_prj_median.tif"))
  #   sdm_models[which(sdm_models[] != 0)] <- 1
  #   sdm_models[is.na(sdm_models[])] <- 0
  #   return(sdm_models)
  # })
  # sdm_models <- raster::stack(sdm_models_1)
  # sdm_models <- sum(sdm_models)
  # sdm_models <- raster::crop(sdm_models, msk)
  # sdm_models <- raster::mask(sdm_models, msk)
  # sdm_models[which(sdm_models[] == 0)] <- NA
  
  # Load final gap maps
  final_gap_rast <- lapply(1:length(classes), function(i){
    final_gap_rast1 <- raster(paste0(results_dir, "/", crop, "/", level, "/", classes[i], "/", region, "/gap_models/gap_class_final.tif"))
    final_gap_rast1[which(final_gap_rast1[] != 2)] <- 0
    final_gap_rast1[which(final_gap_rast1[] == 2)] <- 1
    final_gap_rast1[which(is.na(final_gap_rast1[]))] <- 0
    return(final_gap_rast1)
  })
  final_gap_rast_1 <- raster::stack(final_gap_rast)
  final_gap_rast_1 <- sum(final_gap_rast_1, na.rm = T)
  
  out1 <- paste0(results_dir,"/",crop,"/", level, "/model_final_", crop, ".tif")
  if(!file.exists(out1)){
    raster::writeRaster(sdm_models, out1, overwrite = TRUE)
  }
  out2 <- paste0(results_dir,"/",crop,"/", level, "/gap_final_", crop,".tif")
  if(!file.exists(out2)){
    raster::writeRaster(final_gap_rast_1, out2, overwrite = TRUE)
  }; rm(out1, out2)
  
  gc()
  
  # Load Global shapefile
  shp <- raster::shapefile("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/shapefiles/GAUL_2014/G2014_2013_0_NO_ANT.shp"); global<-as.data.frame(shp)
  
  # Iterate by country
  # shp$ADM0_NAME <- shp$ADM0_NAME %>% iconv(., "UTF-8", "latin1")
  # countries <- shp$ADM0_NAME %>% iconv(., "UTF-8", "latin1") %>% as.character() %>% sort()
  countries <- shp$ADM0_NAME %>% as.character() %>% sort()
  
  gap_area_per_country <- lapply(1:length(countries), function(i){
    
    cat("Extracting values from: ", countries[i], " \n")
    
    # Selecting country from shp file
    shp_new <- shp[which(shp@data$ADM0_NAME == countries[i]),]
    
    # Creating raster file for country
    wmsk <- raster::raster("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask/mask_world.tif")
    country <- wmsk %>% raster::crop(shp_new) %>% raster::mask(mask = shp_new)
    
    # Obtaining area per pixel for country raster file
    c_area <- country %>% raster::area() %>% raster::mask(mask = country)
    
    crop_gaps <- final_gap_rast_1
    crop_gaps[which(crop_gaps[] == 0)] <- NA
    crop_gaps[which(!is.na(crop_gaps[]))] <- 1
    
    crop_gaps <- raster::projectRaster(from = crop_gaps, to = country)
    crop_gaps <- crop_gaps %>% raster::crop(country) %>% raster::mask(mask = country)
    cntr_gaps <- c_area * crop_gaps
    df <- data.frame(Country = countries[i], Area = sum(cntr_gaps[], na.rm = T))
    
    return(df)
    
  })
  gap_area_per_country <- do.call(rbind, gap_area_per_country)
  
  out <- paste0(results_dir,"/",crop,"/", level, "/", crop,"_gap_area_per_country.csv")
  if(!file.exists(out)){
    write.csv(gap_area_per_country, out, row.names = F)
  }
  return(cat("Done\n"))
  
}

calculate_gap_areas_per_country(crop     = "wheat_durum",
                                classes  = c("g1", "g2"),
                                level    = "runs_2019_07_environmental_groups",
                                region   = "wheat_custom",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
calculate_gap_areas_per_country(crop     = "wheat_bread",
                                classes  = c("g1", "g2","g3"),
                                level    = "runs_2019_07_environmental_groups",
                                region   = "wheat_custom",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
calculate_gap_areas_per_country(crop     = "maize",
                                classes  = c("ceusa2", "cobra","hiand","himex","lomam","losam","nousa","wemex"),
                                level    = "lvl_1",
                                region   = "americas",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")

# Banano
calculate_gap_areas_per_country(crop     = "banana",
                                classes  = c("all"),
                                level    = "lvl_1",
                                region   = "banana_custom",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# Sorghum
calculate_gap_areas_per_country(crop     = "sorghum",
                                classes  = c("Bicolor","Caudatum","Durra","Guinea"),
                                level    = "lvl_1",
                                region   = "sgh_custom",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# Potato
calculate_gap_areas_per_country(crop     = "potato",
                                classes  = c("tuberosum_andigenum","tuberosum_chilotanum"),
                                level    = "lvl_1",
                                region   = "americas",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# Barley
calculate_gap_areas_per_country(crop     = "barley",
                                classes  = c("spring","winter"),
                                level    = "lvl_1",
                                region   = "world",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# African maize
calculate_gap_areas_per_country(crop     = "african_maize",
                                classes  = c("2","3","4"),
                                level    = "lvl_1",
                                region   = "africa",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# African rice
calculate_gap_areas_per_country(crop     = "rice_african",
                                classes  = c("K2","K4","K5"),
                                level    = "lvl_1",
                                region   = "africa",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")
# Common bean
calculate_gap_areas_per_country(crop     = "common_bean",
                                classes  = c("andean","mesoamerican"),
                                level    = "lvl_1",
                                region   = "americas",
                                mask_dir = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/mask")