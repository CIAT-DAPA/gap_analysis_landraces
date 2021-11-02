OSys <- Sys.info()[1]
baseDir   <- switch(OSys,
                    "Linux"   = "/mnt/workspace_cluster_9/gap_analysis_landraces/runs",
                    "Windows" = "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs",
                    "Darwin"  = "~nfs/workspace_cluster_9/gap_analysis_landraces/runs")
rm(OSys)

gap_class_maps <- function(crop, occname, region){
  
  results_dir    <- paste0(baseDir, "/results")
  sp_Dir           <- paste0(results_dir, "/", crop, "/lvl_1/", occname, "/", region)
  gap_outDir       <- paste0(sp_Dir, "/gap_models")
  gap_valDir       <- paste0(sp_Dir, "/gap_validation")
  
  
  summ_table <- lapply(list.files(paste0(gap_valDir, "/buffer_100km"), pattern = ".csv$", full.names = T), read.csv  )
  
  names(summ_table) <- substr(list.files(paste0(gap_valDir, "/buffer_100km"), pattern = ".csv$", full.names = F),1,4)
  
  thresh_cost_dist <- summ_table$cost[nrow(summ_table$cost), 7]
  thresh_delaunay  <- summ_table$dela[nrow(summ_table$dela), 7]
  thresh_envi      <- summ_table$env[nrow(summ_table$env), 7]
  
  auc_cost <- summ_table$cost[nrow(summ_table$cost), 2]
  auc_dela <- summ_table$dela[nrow(summ_table$cost), 2]
  auc_envi <- summ_table$env[nrow(summ_table$cost), 2]

  final_gap_rast<- raster(paste0(gap_outDir, "/gap_class_final_new.tif"), overwrite = TRUE)
  
  rs_gaps <- final_gap_rast #raster("Z:/gap_analysis_landraces/runs/results/common_bean/lvl_1/mesoamerican/americas/gap_models/gap_class_final_new.tif")
  covg_ul <- 100 - length(which(rs_gaps[]==3))/length(which(!is.na(rs_gaps[])))*100
  covg_ll <- 100 - length(which(rs_gaps[]>=2))/length(which(!is.na(rs_gaps[])))*100
  auc_avg <- (auc_cost  + auc_dela + auc_envi)/3 

  
  sdm_models <- readRDS(paste0(sp_Dir,"/sdm.rds"))
  
  auc_sdm <- mean(unlist(sdm_models$AUC))
  
  nauc_sdm <- mean(unlist(sdm_models$nAUC))
  
  cauc_sdm <- mean(unlist(sdm_models$cAUC))
  
  metrics_sdm <- do.call(rbind,sdm_models$evaluation_test) %>% apply(., 2,mean)  %>% t() %>% data.frame()
  


  x<-data.frame(crop = crop, group = occname, auc_cost = auc_cost, thresh_cost_dist = thresh_cost_dist, 
                auc_dela = auc_dela, thresh_delaunay = thresh_delaunay,
                auc_envi = auc_envi, thresh_envi = thresh_envi, coverage_interval = paste0("[", covg_ll, ",", covg_ul, "]"),
                auc_gap = auc_avg, auc_sdm = auc_sdm, nauc_sdm = nauc_sdm, cauc_sdm = cauc_sdm, thres_sdm = metrics_sdm$threshold,
                sdm_sensi = metrics_sdm$sensi, sdm_speci = metrics_sdm$speci, matthews_cor = metrics_sdm$matthews.cor, kappa_index = metrics_sdm$kappa_index )
  
    
 return(x) 
  
  
}



crops_info <- read.csv("C:/Users/MVDIAZ/Desktop/richness_maps/richness_maps_per_crop/CROPS_PATHS_richness.csv")



a<- lapply(1:nrow(crops_info), function(i){
  
  cat("Processing...", crops_info$crop_name[i], ": ", crops_info$race_name[i], "\n")
  
  a<- gap_class_maps(crop = crops_info$crop_name[i], occname = crops_info$race_name[i], region = crops_info$region[i])
  
  
  return(a)
  
})