#load libraries
cat("Loading R packages","\n")
library(sdm); library(raster); library(rgdal); library(dismo)
library(rJava); library(maptools); library(deldir); library(rgeos)

#source functions
cat("Loading R scripts","\n")

source(paste(srcDir,"/SDMs/sample_files.R",sep=""))
source(paste(srcDir,"/SDMs/null_model.R",sep=""))
source(paste(srcDir,"/SDMs/delaunay.R",sep=""))
source(paste(srcDir,"/SDMs/calibration_function.R",sep=""))
source(paste(srcDir,"/SDMs/evaluation_function.R",sep=""))
source(paste(srcDir,"/SDMs/model_driver.R",sep=""))
source(paste(srcDir,"/SDMs/create_buffers.R",sep=""))
source(paste(srcDir,"/SDMs/sdm_approach_function.R",sep=""))
source(paste(srcDir,"/SDMs/projecting_function.R",sep=""))
source(paste(srcDir,"/Miscellanous/kernel_function.R",sep=""))
source(paste(srcDir,"/Miscellanous/kernel_indicator.R",sep=""))
#working directories
cat("Loading working directories","\n")

classResults <- paste(baseDir,"/Results/classification_analysis",sep="")
occDir <- paste(baseDir,"/Input_data/SDMs/occurrences",sep="")
backDir <- paste(baseDir,"/Input_data/SDMs/background",sep="")
swdDir <- paste(baseDir,"/Input_data/SDMs/swd",sep="")
climDir <- paste(baseDir,"/Input_data/raster_sdm/2_5m_Americas_1",sep="")
modDir <- paste(baseDir,"/Results/SDM_modelling",sep=""); if (!file.exists(modDir)) {dir.create(modDir)}
densDir <- paste(baseDir,"/Results/accession_density",sep=""); if (!file.exists(densDir)) {dir.create(densDir)}
current_clim_layer_Dir <- paste(baseDir,"/Input_data/raster_sdm/2_5m",sep="")


#species model output directories
cat("Loading species model output directories","\n")
cat(" ","\n")
sp_Dir <- paste0(modDir,"/",occName); if(!file.exists(sp_Dir)){dir.create(sp_Dir)}
eval_sp_Dir <- paste0(sp_Dir,"/","evaluation"); if(!file.exists(eval_sp_Dir)){dir.create(eval_sp_Dir)}
eval_sp_Dir_rep <- paste0(eval_sp_Dir,"/","replicates");if(!file.exists(eval_sp_Dir_rep)){dir.create(eval_sp_Dir_rep)}
model_outDir <- paste0(sp_Dir,"/","prj_models"); if(!file.exists(model_outDir)){dir.create(model_outDir)}
model_outDir_rep <- paste0(model_outDir,"/","replicates");if(!file.exists(model_outDir_rep)){dir.create(model_outDir_rep)}
gap_outDir <- paste0(sp_Dir,"/","gap_models"); if(!file.exists(gap_outDir)){dir.create(gap_outDir)}
gap_del_outDir <- paste0(gap_outDir,"/","delaunay"); if(!file.exists(gap_del_outDir)){dir.create(gap_del_outDir)}
#Loading files to perform Delaunay analysis

#cont_mask <- paste(baseDir,"/Input_data/_maps/GAUL_2014/G2014_2013_0_NO_ANT.shp",sep="")
cont_mask <- paste(baseDir,"/Input_data/_maps/GAUL_2014/CONTINENTAL.shp",sep="")
friction <- paste(baseDir,"/Input_data/_friction_surface/frsurface_4.tif",sep="")
#
cat(" ","\n")