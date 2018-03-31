
library(tidyverse)
library(pROC)
library(sdm)

occName = "mesoamerican"; geo_score = "cost_dist"; pattern = 3; point = 1

get_statistics <- function(occName = "mesoamerican", geo_score = "cost_dist", pattern = 3, point = 1){
  
  valDir <- paste0("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/common_bean/lvl_1/", occName, "/americas/gap_validation/buffer_100km")
  densList <- c("low_density", "medium_density", "high_density")
  
  cat("Loading occurrences points ...\n")
  ePnts <- read.csv(paste0(valDir, "/", densList[pattern], "/pnt", point, "/01_selected_points/coordinates_to_exclude.csv"))
  iPnts <- read.csv(paste0(valDir, "/", densList[pattern], "/pnt", point, "/01_selected_points/occ_", occName, ".csv"))
  
  cat("Loading gap score ...\n")
  gap_score <- raster::raster(paste0(valDir, "/", densList[pattern], "/pnt", point, "/03_gap_models/gap_score_", geo_score, ".tif"))
  
  cat("Obtaining gap score values for occurrences points ...\n")
  gap     <- cbind(ePnts[,c("lon","lat")], raster::extract(x = gap_score, y = ePnts[,c("lon","lat")]))
  names(gap)[3] <- "Gap_score"
  gap$Observed <- 1
  non_gap <- cbind(iPnts[,c("lon","lat")], raster::extract(x = gap_score, y = iPnts[,c("lon","lat")]))
  names(non_gap)[3] <- "Gap_score"
  non_gap$Observed <- 0
  
  cat("Calculing AUC for all points ...\n")
  all <- rbind(gap, non_gap)
  all_complete <- all[complete.cases(all),]
  roc_all <- pROC::roc(response = all_complete$Observed %>% as.character, predictor = all_complete$Gap_score, auc = T)
  auc_all <- roc_all$auc
  
  cat("Calculing AUC for resample of points ...\n")
  set.seed(1)
  nTimes <- round(sum(all_complete$Observed == 0)/sum(all_complete$Observed == 1))
  folds <- modelr::crossv_kfold(all_complete %>% dplyr::filter(Observed == 0), k = nTimes)
  folds <- folds %>%
    dplyr::mutate(data4Val = purrr::map(.x = test,
                                        .f = function(x){
                                          y <- x$data[x$idx,]
                                          z <- rbind(all_complete %>% dplyr::filter(Observed == 1), y)
                                          return(z)
                                        }))
  folds <- folds %>% dplyr::mutate(AUC = purrr::map(.x = data4Val,
                                                    .f = function(x){
                                                      roc <- pROC::roc(response = all_complete$Observed %>% as.character, predictor = all_complete$Gap_score, auc = T)
                                                      auc <- roc$auc %>% as.numeric
                                                      return(auc)
                                                    }))
  
  df <- tibble::tibble(Genepool = occName,
                       Geo_score = geo_score,
                       Density_pattern = densList[pattern],
                       Point = point,
                       Occ_total = nrow(ePnts) + nrow(iPnts),
                       Occ_included = nrow(iPnts),
                       Occ_excluded = nrow(ePnts),
                       Occ_included_data = sum(all_complete$Observed == 0),
                       Occ_excluded_data = sum(all_complete$Observed == 1),
                       AUC_all = auc_all %>% as.numeric,
                       AUC_avg = algo,
                       AUC_sd = algo,
                       Threshold_metrics = list(...))
  
  return(df)
  
}
