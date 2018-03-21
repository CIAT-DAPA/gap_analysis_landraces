require(raster)


current_clim_layer_Dir<-"//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/2_5m"
extension_r<-".tif"
extension_r<-paste0(extension_r,"$")
outDir<-"//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/raster_sdm/asc"
files<-list.files(current_clim_layer_Dir,pattern=extension_r,full.names=T)
files_names<-list.files(current_clim_layer_Dir,pattern=extension_r,full.names=F)
files_names<-sub(".tif",".asc",files_names)
current_clim_layer<-lapply(1:length(files),function(i){
  x<-raster(files[[i]])
  cat("saving: ",as.character(files_names[[i]]),"\n")
  writeRaster(x,paste0(outDir,"/",files_names[[i]]))
  
})
  


