#Run SDMs
#Chrystiam Sosa, Julian Ramirez-Villegas
#CIAT, Nov 2017

#base directory
#baseDir <- "~/nfs/workspace_cluster_9/gap_analysis_landraces"
#baseDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces"

#software directory
#srcDir <- paste(baseDir,"/Scripts",sep="")

#source functions

model_driver <- function(occName,extension_r,all,overwrite,clsModel,correlation){
  
  #output directory
  
  mask <- paste(baseDir,"/Input_data/mask_wb_c_ant_AMERICAS.tif",sep="")
  
  #selected genepool
  #occName <- "Mesoamerican" #"Mesoamerican" #"Andean"
  occFile <- paste(classResults,"/genepool_predicted.RDS",sep="")
  
  #SWD using all variables provided
  if(all==T){
    
    var_names <- list.files(current_clim_layer_Dir,pattern = paste0(extension_r,"$"),full.names = F)
    var_names <-gsub(extension_r,"",var_names)
  } else {
    
    #SWD variables provided for classification
    
    #load variables for race
    var_names <- readRDS(paste(classResults,"/genepool_predictors.RDS",sep=""))
    var_names <- var_names[,c("Variable",paste(occName,clsModel,sep=""))]
    var_names <- paste(var_names[which(var_names[,paste(occName,clsModel,sep="")] >= 25),"Variable"])
  }
  
  #create occurrences, background, and swd (occ+bg)
  
  if(overwrite==T){
    
    swd <- samples_create(occFile,occName,backDir,occDir,swdDir,mask,current_clim_layer_Dir,extension_r,var_names,overwrite=T,correlation=correlation);gc()
  } else {
    swd <- samples_create(occFile,occName,backDir,occDir,swdDir,mask,current_clim_layer_Dir,extension_r,var_names,overwrite=F,correlation=correlation);gc()
    
  }
  #Getting variables to use in the SDM
  var_names <- colnames(swd)[!colnames(swd) %in% c("id","species","status","lon","lat")]
  
  return(var_names)
}


#raster file extension
# extension_r <- ".tif"
# clsModel <- ".RF"
# occName <- occName
# var_names<-model_driver(occName,extension_r,all=T,overwrite=F,clsModel)
