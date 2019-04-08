#######
# Implemented by: M.Victoria Díaz
# CIAT, 2018

#########################  Summary_table function ##############################################
## Function to make a summary of the main metrics of Gap model for each crop. 
## Therefore, it calculates the percentage of area that is potential gap by: 1) Both approaches (delaunay AND cost distance); 2) One of them (delaunay OR cost distance)
# @param (string) crop: Crop name(s) 
# @param (string) occName: Name of one genepool, or species of the crop(s)
# @param (string) level: Level you are analizing
# @param (string) region: Region of analysis of the crop
# @param (string) mask_dir: Path where the region mask is located.
# @return (data.frame): This function returns a data frame with a summary.




coverage_metrics<-function(crop, occName , level ,  region , mask_dir = mask){
  
  
  summm <- lapply(1:length(crop), function(i){
      
    cat(i, "\n")
    wmask <- raster::raster(paste0(mask_dir, "/mask_",region[i], ".tif" ))
      
    sdm_model<- raster(paste0(results_dir, "/", crop[i], "/", level, "/", occName[i], "/", region[i], "/prj_models/", occName[i],"_prj_median.tif"))
    
    final_gap_rast <- raster(paste0(results_dir, "/", crop[i], "/", level, "/", occName[i], "/", region[i], "/gap_models/gap_class_final.tif"))
    
    
    rast_area <- raster::area(wmask) * wmask
    sdm_mask <- raster::mask(wmask, sdm_model)
    
    sdm_area <- sdm_mask * rast_area
    total_sdm_area <- sum(sdm_area[], na.rm = TRUE)
    
    
    cat(paste0("Calculating High Confidence coverage area for ", crop[i], " ", occName[i]), "\n")
    
    high_conf <- final_gap_rast
    high_conf[which(high_conf[] != 2)] <- 1
    high_conf[which(high_conf[] == 2)] <- NA
    high_conf <- high_conf * rast_area
    total_hg_conf <- sum(high_conf[], na.rm = TRUE)
    
    high_conf_percent <-  (total_hg_conf/total_sdm_area)*100
    
    rm(high_conf);g <- gc(); rm(g)
    
    
    cat(paste0("Calculating Low Confidence coverage area for ", crop[i], " ", occName[i]), "\n")
    
    low_conf <- final_gap_rast
   # low_conf[which(low_conf[] == 0)] <- 1
  #  low_conf[!is.na(low_conf[])] <- NA
    
     low_conf[which(low_conf[] != 0)] <- NA
     low_conf[which(low_conf[] == 0)] <- 1
    
    
    low_conf <- low_conf * rast_area
    total_lw_conf <- sum(low_conf[], na.rm = TRUE)
    
    low_conf_percent <-  (total_lw_conf/total_sdm_area)*100
    
    rm( rast_area, sdm_mask );g <- gc(); rm(g)
    
    
    cat(paste0("Calculating Gap model performance for ", crop[i], " ", occName[i], "..."), "\n")
    
    validations_file <- xlsx::read.xlsx(paste0(results_dir, "/", crop[i], "/", level, "/", occName[i], "/", region[i], "/gap_validation/buffer_100km/validation_results.xlsx"), sheetIndex =  1, header = TRUE)
    
    
    cat("...Cost distance approach", "\n")
    
 #   average_cost <- validations_file[which(validations_file$approach == "Cost_distance"), c(1:4)] 
 #   average_cost <- average_cost %>% subset(., average_cost$pnt %in% "Average" ) 
  #  average_cost <- average_cost[, -which(names(average_cost) %in% "pnt")]
    
     cost <- validations_file[c(2:7), -1]; cost<-as.data.frame(cost) 
     cost[,1]<-cost[,1] %>% as.character(); cost[,1]<-cost[,1] %>% as.numeric()
     average_cost <- cost[6,1]
    
    
    
    cat("...Delaunay approach", "\n")
    
  #  average_del <- validations_file[which(validations_file$approach == "Delaunay"), c(1:4)]
  #  average_del <- average_del %>% subset(., average_del$pnt %in% "Average" ) 
  #  average_del <- average_del[, -which(names(average_del) %in% "pnt")]
    
     
    
    del <- validations_file[c(10:15), -1]; del<-as.data.frame(del) 
    del[,1]<-del[,1] %>% as.character(); del[,1]<-del[,1] %>% as.numeric()
    average_del <- del[6,1]
    
    GAP_AUC = (average_cost*0.4) + (average_del*0.6)
    
    
    
    cat("Resuming metrics ", "\n")
    summary <- data.frame(Crop = crop[i], Level = "Genepool", Name = occName[i], Region = region[i], High_coverage_area = high_conf_percent,  low_coverage_area = low_conf_percent, Gap_model_AUC_cost = average_cost, Gap_model_AUC_del = average_del, Gap_model_AUC = GAP_AUC)
    
   # final_table <- list(summary_metrics = summary, cost_metrics = average_cost, del_metrics = average_del)
    
    cat(paste0(crop[i], " ", occName[i], " Done!"), "\n")
    
    return(summary)
    
  } )
  
  summm <- do.call(rbind, summm)

  write.csv(summm, paste0(baseDir, "/summary_metrics.csv"), row.names = FALSE)
  
  return(summm)
}


#### Test funcion ### 

coverage_metrics(crop = c("sorghum", "sorghum", "sorghum", "sorghum", "sorghum",  "banana", "banana", "banana"), occName = c("guinea","kafir","caudatum","bicolor","durra","all","2" ,"3"), level = level, region = c("sgh_custom","sgh_custom","sgh_custom","sgh_custom","sgh_custom",  "banana_custom", "banana_custom", "banana_custom"), mask_dir = mask_dir)