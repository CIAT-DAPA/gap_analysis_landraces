#Julian Ramirez-Villegas
#March 2018

#open environmental score, geographic score and sdm probability
#and combine them into a single score map

#function details
wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
crop_name="common_bean"
level="1"
lv_name="mesoamerican"
clus_method="kmeans"
gap_method="cost_dist" #kernel #delaunay

#directories
res_dir <- paste(wd,"/results/",crop_name,"/lvl_",level,"/",lv_name,sep="")
sdm_dir <- paste(res_dir,"/prj_models",sep="")
occ_dir <- paste(wd,"/input_data/by_crop/",crop_name,"/lvl_",level,"/",lv_name,"/occurrences",sep="")
env_dir <- paste(wd,"/input_data/generic_rasters/americas",sep="")

#load sdm projection
sdm_prj <- raster(paste(sdm_dir,"/",lv_name,"_prj_median.tif",sep=""))
sdm_prj <- readAll(sdm_prj)

#load accession accessibility
occ_access <- raster(paste(res_dir,"/gap_models/cost_dist.tif",sep=""))
occ_access <- readAll(occ_access)
occ_access <- crop(occ_access, sdm_prj)

#mask geo_score to sdm, and then normalise (so that geo score is relative to the sdm)
geo_score <- occ_access
geo_score <- mask(geo_score, sdm_prj)
geo_score[which(is.na(geo_score[]) & !is.na(sdm_prj[]))] <- max(geo_score[],na.rm=T)
geo_score <- geo_score / max(geo_score[], na.rm=T)

#load environmental score
env_score <- raster(paste(res_dir,"/gap_models/env_score.tif",sep=""))
env_score <- readAll(env_score)


