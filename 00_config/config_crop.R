
config_crop_dirs <- function(baseDir, crop, level_1, level_2, level_3){

# input_data directory
input_data_dir <-  paste0(baseDir, "/input_data")
# results_dir
results_dir <-  paste0(baseDir, "/results")
# Dinamic crop input data
layers_dir <- paste(input_data_dir, "/by_crop", sep = "")
# input dir for each crop 
crop_dir <- paste(layers_dir, "/", crop, sep = "")
# input dir for each crop
crop_dir_results <- paste(results_dir, "/", crop, sep = "")
# mask dir
mask_dir       <- paste0(input_data_dir, "/mask");  if (!file.exists(mask_dir)) {dir.create(mask_dir, recursive = TRUE)}
# create folder of auxiliar rasters
aux_dir        <- paste0(input_data_dir, "/auxiliar_rasters");  if (!file.exists(aux_dir)) {dir.create(aux_dir, recursive = TRUE)}

#create ecosystems folder

eco_dir <- paste0(input_data_dir, "/ecosystems");  if (!file.exists(eco_dir)) {dir.create(eco_dir, recursive = TRUE)}

##Level 1 directories
if(length(level_1) > 0){
lapply(1:length(level_1), function(i){
  # Input data directories for crop
  # class_dir <- paste(crop_dir, "/lvl_1/classification", sep = ""); if(!file.exists(class_dir)){dir.create(class_dir)}
  class_dir_results <- paste(crop_dir_results, "/lvl_1/classification", sep = ""); if(!file.exists(class_dir_results)){dir.create(class_dir_results, recursive = TRUE)}
  level_dir_i <- paste(crop_dir, "/lvl_1/", level_1[[i]], sep = ""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i, recursive = TRUE)}
  occDir <- paste(level_dir_i, "/occurrences", sep = ""); if (!file.exists(occDir)) {dir.create(occDir, recursive = TRUE)}
  backDir <- paste(level_dir_i, "/background", sep = ""); if (!file.exists(backDir)) {dir.create(backDir, recursive = TRUE)}
  swdDir <- paste(level_dir_i, "/swd", sep = ""); if (!file.exists(swdDir)) {dir.create(swdDir, recursive = TRUE)}
  
  # result directories for crop
  level_dir_r <- paste(crop_dir_results, "/lvl_1/", level_1[[i]], sep = ""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r, recursive = TRUE)}
  })
} else {
  cat("Level 1 not available","\n")
}
##Level 2 directories
if(length(level_2) > 0){
lapply(1:length(level_2), function(i){
  # Input data directories for crop
  # class_dir <- paste(crop_dir,"/","lvl_2","/","classification",sep=""); if(!file.exists(class_dir)){dir.create(class_dir)}
  class_dir_results <- paste(crop_dir_results, "/lvl_2/classification", sep = ""); if(!file.exists(class_dir_results)){dir.create(class_dir_results, recursive = TRUE)}
  level_dir_i <- paste(crop_dir, "/lvl_2/", level_2[[i]], sep = ""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i, recursive = TRUE)}
  occDir <- paste(level_dir_i, "/occurrences", sep = ""); if (!file.exists(occDir)) {dir.create(occDir, recursive = TRUE)}
  backDir <- paste(level_dir_i, "/background", sep = ""); if (!file.exists(backDir)) {dir.create(backDir, recursive = TRUE)}
  swdDir <- paste(level_dir_i, "/swd", sep = ""); if (!file.exists(swdDir)) {dir.create(swdDir, recursive = TRUE)}
  # result directories for crop
  level_dir_r <- paste(crop_dir_results,"/lvl_2/", level_2[[i]], sep = ""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r, recursive = TRUE)}
  })
} else {
  cat("Level 2 not available","\n")
}  
##Level 3 directories
if(length(level_3) > 0){
  lapply(1:length(level_3), function(i){
    # Input data directories for crop
    # class_dir <- paste(crop_dir, "/lvl_3/classification", sep = ""); if(!file.exists(class_dir)){dir.create(class_dir)}
    class_dir_results <- paste(crop_dir_results, "/lvl_3/classification", sep = ""); if(!file.exists(class_dir_results)){dir.create(class_dir_results, recursive = TRUE)}
    level_dir_i <- paste(crop_dir, "/lvl_3/", level_3[[i]], sep=""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i, recursive = TRUE)}
    occDir <- paste(level_dir_i, "/occurrences", sep = ""); if (!file.exists(occDir)) {dir.create(occDir, recursive = TRUE)}
    backDir <- paste(level_dir_i, "/background", sep = ""); if (!file.exists(backDir)) {dir.create(backDir, recursive = TRUE)}
    swdDir <- paste(level_dir_i, "/swd", sep = ""); if (!file.exists(swdDir)) {dir.create(swdDir, recursive = TRUE)}
    # result directories for crop
    level_dir_r <- paste(crop_dir_results, "/lvl_3/", level_3[[i]], sep = ""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r, recursive = TRUE)}
   }) 
  }
}
