##################
# Functin to reclassify gap score rasters
# @author: Andres Camilo Mendez
#2019


gap_class_maps <- function(gap_valDir,
                           gap_outDir){
  
  summ_table <- lapply(list.files(paste0(gap_valDir, "/buffer_100km"), pattern = ".csv$", full.names = T), read.csv  )
  
  names(summ_table) <- substr(list.files(paste0(gap_valDir, "/buffer_100km"), pattern = ".csv$", full.names = F),1,4)
 
  thresh_cost_dist <- summ_table$cost[nrow(summ_table$cost), 7]
  thresh_delaunay  <- summ_table$dela[nrow(summ_table$dela), 7]
  thresh_envi      <- summ_table$env[nrow(summ_table$env), 7]
  
  auc_cost <- summ_table$cost[nrow(summ_table$cost), 2]
  auc_dela <- summ_table$dela[nrow(summ_table$cost), 2]
  auc_envi <- summ_table$env[nrow(summ_table$cost), 2]
  #import gap score rasters
  cost_dist_rast <- raster(paste0(gap_outDir,"/gap_score_cost_dist_new.tif"))
  dela_rast      <- raster(paste0(gap_outDir,"/gap_score_delaunay_new.tif"))
  env_rast       <- raster(paste0(gap_outDir,"/gap_score_environ_new.tif"))
  #reclassify and save both rasters using the thressholds if they doesn't exists
  cat("Reclassifying cost distance score raster \n")
  cost_dist_rast[which(cost_dist_rast[] >= thresh_cost_dist )] <- 1
  cost_dist_rast[which(cost_dist_rast[] != 1)] <- 0
  names(cost_dist_rast) <- "gap_class_cost_dist"
  writeRaster(cost_dist_rast, paste0(gap_outDir, "/gap_class_cost_dist_new.tif"), overwrite = TRUE)
  
  cat("Reclassifying delaunay score raster \n")
  dela_rast[which(dela_rast[] >= thresh_delaunay)] <- 1
  dela_rast[which(dela_rast[] != 1)] <- 0
  names(dela_rast)<- "gap_class_delaunay"
  writeRaster(dela_rast, paste0(gap_outDir, "/gap_class_delaunay_new.tif"), overwrite = TRUE)
  
  cat("Reclassifying environmental score raster \n")
  env_rast[which(env_rast[] >= thresh_envi)] <- 1
  env_rast[which(env_rast[] != 1)] <- 0
  names(env_rast)<- "gap_class_envi"
  writeRaster(env_rast, paste0(gap_outDir, "/gap_class_environ_new.tif"), overwrite = TRUE)
  
  
  
  final_gap_rast <- dela_rast + cost_dist_rast + env_rast
  names(final_gap_rast) <- "gap_class_final"
  writeRaster(final_gap_rast,paste0(gap_outDir, "/gap_class_final_new.tif"), overwrite = TRUE)
  
  rs_gaps <- final_gap_rast #raster("Z:/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_models/gap_class_final_new.tif")
  covg_ul <- 100 - length(which(rs_gaps[]==3))/length(which(!is.na(rs_gaps[])))*100
  covg_ll <- 100 - length(which(rs_gaps[]>=2))/length(which(!is.na(rs_gaps[])))*100
  cat("coverage of",occName,"is UL=",round(covg_ul,1),"% and LL=",round(covg_ll,1),"% \n")
  auc_avg <- (auc_cost  + auc_dela + auc_envi)/3 
  cat("AUC_avg:", auc_avg, "\n")
  
}
