#Base dir to perform analysis
baseDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs"; if(!file.exists(baseDir)){dir.create(baseDir)}
# Scripts directory
srcDir <- paste0(baseDir,"/scripts"); if(!file.exists(srcDir)){dir.create(srcDir)}
# input_data directory
input_data_dir <-  paste0(baseDir,"/input_data"); if(!file.exists(input_data_dir)){dir.create(input_data_dir)}
# raster layers dir
#layers_dir <- paste(input_data_dir,"/layers",sep=""); if(!file.exists(layers_dir)){dir.create(layers_dir)}
#generic raster layers
gen_raster_dir <- paste(input_data_dir,"/generic_rasters",sep=""); if(!file.exists(gen_raster_dir)){dir.create(gen_raster_dir)}
#auxiliar raster layers
aux_dir <- paste(input_data_dir,"/auxiliar_rasters",sep=""); if(!file.exists(aux_dir)){dir.create(aux_dir)}

#mask dirs
mask_dir <- paste(input_data_dir,"/mask",sep=""); if(!file.exists(mask_dir)){dir.create(mask_dir)}

#shapefile directories
shp_dir <- paste(input_data_dir,"/shapefiles",sep=""); if(!file.exists(shp_dir)){dir.create(shp_dir)}

#Dinamic crop input data
layers_dir <- paste(input_data_dir,"/by_crop",sep=""); if(!file.exists(layers_dir)){dir.create(layers_dir)}

#results_dir
results_dir <-  paste0(baseDir,"/results"); if(!file.exists(results_dir)){dir.create(results_dir)}

crops <- c(
  "common_bean",
  "potato",
  "barley",
  "banana",
  "rice_asian",
  "rice_african",
  "chickpea",
  "wheat_bread",
  "wheat_durum",
  "cassava",
  "maize",
  "yam",
  "grass_pea",
  "lentil",
  "sweet_potato",
  "sorghum",
  "groundnut",
  "cowpea",
  "pea",
  "faba_bean",
  "pigeonpea",
  "finger_millet",
  "pearl_millet",
  "forages"
)

levels <- c("lvl_1","lvl_2")

lapply(1:length(crops),function(i){
   lapply(1:length(levels),function(j){
  #input dir for each crop
  crop_dir <- paste(layers_dir,"/",crops[[i]],sep=""); if(!file.exists(crop_dir)){dir.create(crop_dir)}
  #input dir for each crop level
  level_dir <- paste(crop_dir,"/",levels[[j]],sep=""); if(!file.exists(level_dir)){dir.create(level_dir)}
  #classification input data dir per crop leveol
  class_dir <- paste(level_dir,"/","classification",sep=""); if(!file.exists(class_dir)){dir.create(class_dir)}
  #results dir for each crop
  crop_result_dir <- paste(results_dir,"/",crops[[i]],sep=""); if(!file.exists(crop_result_dir)){dir.create(crop_result_dir)}
  #results dir for each crop level
  level_result_dir <- paste(crop_result_dir,"/",levels[[j]],sep=""); if(!file.exists(level_result_dir)){dir.create(level_result_dir)}
    })
})

rm(crops,levels,shp_dir,input_data_dir,gen_raster_dir,results_dir,layers_dir)
