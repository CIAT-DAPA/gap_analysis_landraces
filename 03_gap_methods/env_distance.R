#Julian Ramirez-Villegas
#March 2018

#calculate environmental distance to closest accession for each evironmental cluster
#and return environmental score

#test function
#wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
#env_score <- calc_env_score(wd,crop_name="common_bean",level="1",lv_name="mesoamerican",region="americas",clus_method="hclust_mahalanobis")

calc_env_score <- function(wd,crop_name,level,lv_name,region,clus_method="hclust_mahalanobis") {
  #load packages
  require(raster); require(sdm); require(distances); require(matrixStats)
  
  #directories
  res_dir <- paste(wd,"/results/",crop_name,"/lvl_",level,"/",lv_name,"/",region,sep="")
  sdm_dir <- paste(res_dir,"/prj_models",sep="")
  occ_dir <- paste(wd,"/input_data/by_crop/",crop_name,"/lvl_",level,"/",lv_name,"/",region,"/occurrences",sep="")
  env_dir <- paste(wd,"/input_data/generic_rasters/",region,sep="")
  
  #load sdm object
  sdm_obj <- read.sdm(paste(res_dir,"/sdm.sdm",sep=""))
  
  #load sdm projection
  sdm_prj <- raster(paste(sdm_dir,"/",lv_name,"_prj_median.tif",sep=""))
  sdm_prj <- readAll(sdm_prj)
  
  #load accessions
  occ_data <- read.csv(paste(occ_dir,"/occ_",lv_name,".csv",sep=""),header=T)
  
  #load environmental layers
  env_names <- names(sdm_obj@data@features)[2:ncol(sdm_obj@data@features)][c(2,14,19)]
  env_data <- stack(paste(env_dir,"/",env_names,".tif",sep=""))
  env_data <- readAll(env_data)
  
  #load cluster dataset
  clus_rs <- raster(paste(res_dir,"/gap_models/ecogeo_",clus_method,".tif",sep=""))
  
  #list of clusters
  lclus <- unique(na.omit(clus_rs[]))
  
  #calculate minimum Euclidean distance for each cluster using the accessions within that cluster
  rs_euc <- raster(clus_rs)
  occ_exist <- c()
  for (i in lclus) {
    cat("i=",i,"\n")
    #get accessions from that cluster
    euc_occ <- occ_data
    euc_occ$cluster <- extract(clus_rs, euc_occ[,c("lon","lat")])
    euc_occ <- euc_occ[complete.cases(euc_occ),]
    euc_occ <- euc_occ[which(euc_occ$cluster == i),env_names]
    
    #extract cluster data
    xy_clus <- data.frame(cellid=which(clus_rs[] == i))
    xy_clus <- cbind(xy_clus, xyFromCell(clus_rs, xy_clus$cellid))
    xy_clus <- cbind(xy_clus, extract(env_data, xy_clus[,c("x","y")]))
    xy_clus <- xy_clus[,env_names]
    
    #matrix of cluster data
    if (nrow(euc_occ) > 0) {
      #control list
      occ_exist <- c(occ_exist,T)
      
      #normalise variables
      td_all <- rbind(xy_clus,euc_occ)
      td_all <- as.data.frame(scale(td_all))
      
      #calculate Euclidean distance
      td_dist <- distances(td_all, normalize="none")
      td_matrix <- distance_columns(td_dist, c((nrow(xy_clus)+1):nrow(td_all)),
                                    c(1:nrow(xy_clus)))
      colnames(td_matrix) <- 1:nrow(euc_occ)
      dist_vals <- rowMins(td_matrix)
    } else {
      #control list
      occ_exist <- c(occ_exist,F)
      dist_vals <- rep(NA, times=nrow(xy_clus))
    }
    rs_euc[which(clus_rs[] == i)] <- dist_vals
  }
  
  #now if any of the clusters did not have accessions need to assign maximum
  #distance value so that the env. score comes to 1 in those areas
  for (i in 1:length(lclus)) {if (!occ_exist[i]) {rs_euc[which(clus_rs[] == lclus[i])] <- max(rs_euc[],na.rm=T)}}
  
  #normalise entire raster to be on 0-1 scale (environmental score)
  #1 is more likely there is a gap, 0 is less likely there is a gap
  rs_euc_norm <- rs_euc / max(rs_euc[],na.rm=T)
  
  #write output
  writeRaster(rs_euc, paste(res_dir,"/gap_models/euclidean_dist_",clus_method,".tif",sep=""),format="GTiff")
  writeRaster(rs_euc_norm, paste(res_dir,"/gap_models/env_score_",clus_method,".tif",sep=""),format="GTiff")
  
  #return rasters
  return(rs_euc_norm)
}
