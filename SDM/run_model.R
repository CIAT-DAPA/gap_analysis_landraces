#Run SDMs
#Chrystian Sosa, Julian Ramirez-Villegas
#CIAT, Nov 2017

#base directory
#baseDir <- "~/nfs/workspace_cluster_9/gap_analysis_landraces"
baseDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces"

#software directory
srcDir <- paste(baseDir,"/Scripts",sep="")

#Temporal directory for Temporal raster files
raster::rasterOptions(tmpdir="D:/TEMP/CSOSSA")

#Calling Species to run
occName <- "Mesoamerican" #"Mesoamerican"

source(paste(srcDir,"/SDMs/config.R",sep=""))

extension_r <- ".tif"
clsModel <- ".RF"
correlation <- 3 #1 Correlation, 2 VIF, 3 PCA +VIF
var_names <- model_driver(occName,extension_r,all=F,overwrite=F,clsModel,correlation=correlation)


#swd file and occurrence data

swdFile <- paste0(swdDir,"/swd_",occName,".csv")
spData <- read.csv(swdFile)
spData <- spData[,c(3:ncol(spData))]
names(spData)[1] <- occName

#Loading raster files

mask <- raster(paste(baseDir,"/Input_data/mask_wb_c_ant_AMERICAS.tif",sep=""))
clim_layer <- lapply(paste0(climDir,"/","/",paste0(var_names,".tif")),raster)
clim_layer <- stack(clim_layer)

#Calibration step
cat("Performing calibration step","\n")

if(file.exists(paste0(sp_Dir,"/","calibration.csv"))){
  calibration <- read.csv(paste0(sp_Dir,"/","calibration.csv"))
  feat <- CreateMXArgs(calibration);
  beta <- feat[(grepl("betamultiplier=",feat))];beta <- as.numeric(gsub("betamultiplier=","",beta))
  feat <- feat[(!grepl("betamultiplier=",feat))]
  rm(calibration)
} else {
  feat <- Calibration_function(spData=spData,save=T,sp_Dir=sp_Dir,ommit=F)
beta <- feat[(grepl("betamultiplier=",feat))];beta <- as.numeric(gsub("betamultiplier=","",beta))

feat <- feat[(!grepl("betamultiplier=",feat))]
}

#running SDMs
cat("Running modeling approach","\n")

m2 <- sdm_approach_function(occName,spData,sp_Dir,eval_sp_Dir,model_outDir,var_names,nCores=5,nReplicates=5,beta,feat)

#model evaluation per replicates (nReplicates x 5)
cat("Evaluating models performance","\n")
m2_eval <- evaluation_function(m2,eval_sp_Dir)


##model projecting
cat("projecting models","\n")
#Detaching caret to avoid packages conflict
detach("package:caret", unload=TRUE) # MANDATORY!

model <- projecting_function(m2,m2_eval,model_outDir,nCores=5,obj.size=3)
#model <- model

#final evaluation table
cat("Validating model","\n")
m2_eval_final <- final_evaluation(m2_eval,occName)


#Run buffer approach
cat("Creating buffer around 50 Km","\n")

if(!file.exists(paste0(gap_outDir,"/","buffer.tif"))){
buffer <- create_buffers(xy=spData[which(spData[,1]==1),c("lon","lat")], msk=mask, buff_dist=0.5, format="GTiff", filename=paste0(gap_outDir,"/","buffer.tif"))
buffer[which(buffer[]==0)] <- NA; buffer[which(buffer[]!=0)] <- 10
} else {
  buffer <- raster(paste0(gap_outDir,"/","buffer.tif"))
}


#gap maps
cat("Making gap map")

if(!file.exists(paste0(gap_outDir,"/","gap_map.tif"))){
gap_map <- sum(model,buffer,na.rm=T)
gap_map[which(gap_map[]==10)] <- NA 
gap_map[which(gap_map[]==11)] <- NA 
gap_map[which(gap_map[]==0)] <- NA 
writeRaster(gap_map,paste0(gap_outDir,"/","gap_map.tif"))
} else {
  gap_map <- raster(paste0(gap_outDir,"/","gap_map.tif"))
}

#Kernel function #1 spatstat, #2 Adehabitat, #3 Kernsmooth

if(!file.exists(paste0(gap_outDir,"/","kernel.tif"))){
  occurrences=spData[spData[,1]==1,]
kernel <- raster_kernel(mask=mask,occurrences=spData[spData[,1]==1,],out_dir=gap_outDir,kernel_method=3,scale=T)
} else {
kernel <- raster(paste0(gap_outDir,"/","kernel.tif")) 
}
