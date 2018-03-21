

config_crop_dirs <- function(baseDir,crop,level_1,level_2,level_3){

# input_data directory
input_data_dir <-  paste0(baseDir,"/input_data")
#results_dir
results_dir <-  paste0(baseDir,"/results")
#Dinamic crop input data
layers_dir <- paste(input_data_dir,"/by_crop",sep="")
#input dir for each crop
crop_dir <- paste(layers_dir,"/",crop,sep="")
#input dir for each crop
crop_dir_results <- paste(results_dir,"/",crop,sep="")

##Level 1 directories
if(length(level_1)>0){
lapply(1:length(level_1),function(i){
  # Input data directories for crop
  class_dir <- paste(crop_dir,"/","lvl_1","/","classification",sep=""); if(!file.exists(class_dir)){dir.create(class_dir)}
  class_dir_results <- paste(crop_dir_results,"/","lvl_1","/","classification",sep=""); if(!file.exists(class_dir_results)){dir.create(class_dir_results)}
  level_dir_i <- paste(crop_dir,"/","lvl_1","/",level_1[[i]],sep=""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i)}
  occDir <- paste(level_dir_i,"/occurrences",sep=""); if (!file.exists(occDir)) {dir.create(occDir)}
  backDir <- paste(level_dir_i,"/background",sep=""); if (!file.exists(backDir)) {dir.create(backDir)}
  swdDir <- paste(level_dir_i,"/swd",sep=""); if (!file.exists(swdDir)) {dir.create(swdDir)}
 
  # result directories for crop
  level_dir_r <- paste(crop_dir_results,"/","lvl_1","/",level_1[[i]],sep=""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r)}
  })
} else {
  cat("Level 1 not available","\n")
}
##Level 2 directories
if(length(level_2)>0){
lapply(1:length(level_2),function(i){
  # Input data directories for crop
  class_dir <- paste(crop_dir,"/","lvl_2","/","classification",sep=""); if(!file.exists(class_dir)){dir.create(class_dir)}
  class_dir_results <- paste(crop_dir_results,"/","lvl_2","/","classification",sep=""); if(!file.exists(class_dir_results)){dir.create(class_dir_results)}
  level_dir_i <- paste(crop_dir,"/","lvl_2","/",level_2[[i]],sep=""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i)}
  occDir <- paste(level_dir_i,"/occurrences",sep=""); if (!file.exists(occDir)) {dir.create(occDir)}
  backDir <- paste(level_dir_i,"/background",sep=""); if (!file.exists(backDir)) {dir.create(backDir)}
  swdDir <- paste(level_dir_i,"/swd",sep=""); if (!file.exists(swdDir)) {dir.create(swdDir)}
  # result directories for crop
  level_dir_r <- paste(crop_dir_results,"/","lvl_2","/",level_2[[i]],sep=""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r)}
  })
} else {
  cat("Level 2 not available","\n")
}  
##Level 3 directories
if(length(level_3)>0){
  lapply(1:length(level_3),function(i){
    # Input data directories for crop
    class_dir <- paste(crop_dir,"/","lvl_3","/","classification",sep=""); if(!file.exists(class_dir)){dir.create(class_dir)}
    class_dir_results <- paste(crop_dir_results,"/","lvl_3","/","classification",sep=""); if(!file.exists(class_dir_results)){dir.create(class_dir_results)}
    level_dir_i <- paste(crop_dir,"/","lvl_3","/",level_2[[i]],sep=""); if (!file.exists(level_dir_i)) {dir.create(level_dir_i)}
    occDir <- paste(level_dir_i,"/occurrences",sep=""); if (!file.exists(occDir)) {dir.create(occDir)}
    backDir <- paste(level_dir_i,"/background",sep=""); if (!file.exists(backDir)) {dir.create(backDir)}
    swdDir <- paste(level_dir_i,"/swd",sep=""); if (!file.exists(swdDir)) {dir.create(swdDir)}
    # result directories for crop
    level_dir_r <- paste(crop_dir_results,"/","lvl_3","/",level_2[[i]],sep=""); if (!file.exists(level_dir_r)) {dir.create(level_dir_r)}
   }) 
  }
}




#Base dir to perform analysis
baseDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs"

crop <- c(
   "common_bean"
  # "potato",
  # "barley",
  # "banana",
  # "rice",
  # "chickpea",
  # "wheat_bread",
  # "wheat_durum",
  # "cassava",
  # "maize",
  # "yam",
  # "grass_pea",
  # "lentil",
  # "sweet_potato",
  # "sorghum",
  # "groundnut",
  # "cowpea",
  # "pea",
  # "faba_bean",
  # "pigeonpea",
  # "finger_millet",
  # "pearl_millet",
  # "forages"
)
# 
# level_1 <- c("andean","mesoamerican")
# level_2 <- c("nueva_granada","peru","chile","durango-Jalisco","mesoamerica","guatemala")
# 
# level_3 <- NULL

#x <- config_crop_dirs(baseDir,crop,level_1,level_2,level_3)
