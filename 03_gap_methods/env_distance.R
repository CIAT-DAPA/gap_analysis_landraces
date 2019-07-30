#Julian Ramirez-Villegas
#March 2018

#calculate environmental distance to closest accession for each evironmental cluster
#and return environmental score

#test function
#crop_name="common_bean"; level="1"; lv_name="mesoamerican"; region="americas"
#wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
#wd <- "Z:/workspace_cluster_9/gap_analysis_landraces/runs"
#res_dir <- paste(wd,"/results/",crop_name,"/lvl_",level,"/",lv_name,"/",region,sep="")
#sdm_dir <- paste(res_dir,"/prj_models",sep="")
#occ_dir <- paste(wd,"/input_data/by_crop/",crop_name,"/lvl_",level,"/",lv_name,"/",region,"/occurrences",sep="")
#env_dir <- paste(wd,"/input_data/generic_rasters/",region,sep="")
#gap_dir <- paste(res_dir,"/gap_models/", sep="")
#out_dir <- gap_dir
#env_score <- calc_env_score(lv_name="mesoamerican",clus_method="hclust_mahalanobis",sdm_dir,gap_dir,occ_dir,env_dir,out_dir)

calc_env_score <- function(lv_name, clus_method = "hclust_mahalanobis", sdm_dir, gap_dir, occ_dir, env_dir, out_dir, var_names){
  cat("Intializing calc environmental score process \n \n")
  if(!file.exists(paste(out_dir, "/env_score_", clus_method, ".tif", sep = ""))){
    
    #load packages
   
    
    #load sdm object
    #sdm_obj <- read.sdm(paste(sdm_dir,"/sdm.sdm",sep=""))
    
    #load sdm projection
    sdm_prj <- raster(paste(sdm_dir,"/prj_models/",lv_name,"_prj_median.tif",sep="")) 
    sdm_prj <- readAll(sdm_prj)
    
    #load accessions
    if(length(grep(pattern = "gap_validation", x = occ_dir)) > 0){
      occ_data <- read.csv(paste(occ_dir,"/occ_",lv_name,".csv",sep=""), header = T)
    } else {
      occ_data <- shapefile(paste0(occ_dir, "/Occ.shp"))
      occ_data <- unique(as.data.frame(occ_data)); rownames(occ_data) <- 1:nrow(occ_data)
      names(occ_data)[2:3] <- c("lon", "lat")
    }
    
    #load environmental layers
    env_names <- var_names[paste0(var_names,".tif") %in% list.files(env_dir)]
      #names(sdm_obj@data@features)[2:ncol(sdm_obj@data@features)]
    if ("monthCountByTemp10" %in% env_names) env_names <- env_names[-which(env_names=="monthCountByTemp10")]

    env_data <- stack(paste(env_dir,"/",env_names,".tif",sep=""))
    env_data <- readAll(env_data)
    occ_data <- cbind(occ_data, raster::extract(x = env_data, y = occ_data[,c("lon", "lat")]))
    
    #load cluster dataset
    if(!file.exists(paste(gap_dir,"/ecogeo_",clus_method,".tif",sep=""))){
      clus_rs <- ecogeo_clustering(n.sample = 6000, k.clust = 10, var_names = var_names)
    } else {
      clus_rs <- raster(paste(gap_dir,"/ecogeo_",clus_method,".tif",sep=""))
    }
    
    #list of clusters
    lclus <- unique(clus_rs)
    
    #calculate minimum Euclidean distance for each cluster using the accessions within that cluster
    rs_euc <- raster(clus_rs)
    occ_exist <- c()
    for (i in lclus) {
      cat("i=",i,"\n")
      #get accessions from that cluster
      euc_occ <- occ_data
      euc_occ$cluster <- raster::extract(clus_rs, euc_occ[,c("lon","lat")])
      euc_occ <- euc_occ[complete.cases(euc_occ),]
      euc_occ <- euc_occ[which(euc_occ$cluster == i),env_names]
      
      #extract cluster data
      xy_clus <- data.frame(cellid=which(clus_rs[] == i))
      xy_clus <- cbind(xy_clus, xyFromCell(clus_rs, xy_clus$cellid))
      xy_clus <- cbind(xy_clus, raster::extract(env_data, xy_clus[,c("x","y")]))
      xy_clus <- xy_clus[,env_names]
      
      #matrix of cluster data
      if (nrow(euc_occ) > 0) {
        #control list
        occ_exist <- c(occ_exist,T)
        
        #normalise variables
        td_all <- rbind(xy_clus,euc_occ)
        #if all values of a variable are the same.
        zero_var <- caret::nearZeroVar(td_all)
        if(length(zero_var) != 0){ td_all <- td_all[, -zero_var]}
        td_all <- as.data.frame(scale(td_all))
        td_all <- td_all[complete.cases(td_all), ]
        
        #calculate Euclidean distance
        td_dist <- distances::distances(td_all, normalize="none")
        td_matrix <- distances::distance_columns(td_dist, c((nrow(xy_clus)+1):nrow(td_all)),
                                      c(1:nrow(xy_clus)))
        colnames(td_matrix) <- 1:nrow(euc_occ)
        #dist_vals <- matrixStats::rowMins(td_matrix)
        dist_vals <- apply(td_matrix, 1, min)
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
    
    SDM <- raster(paste(sdm_dir,"/prj_models/",lv_name,"_prj_median.tif",sep="")) 
    
    rs_euc_norm <- raster::mask(rs_euc_norm, SDM)
    #idenfy outliers using IQR 
    qls <- raster::quantile(rs_euc_norm, na.rm = T)
    up_limit <- qls[4] + (1.5* (qls[4] - qls[2]))
    rs_euc_norm <- rs_euc_norm/up_limit
    rs_euc_norm[which(rs_euc_norm[] > 1 )] <- 1
    
    #write output
    #writeRaster(rs_euc, paste(out_dir,"/euclidean_dist_",clus_method,".tif",sep=""),format="GTiff", overwrite = TRUE)
    writeRaster(rs_euc_norm, paste(out_dir,"/env_score_",clus_method,".tif",sep=""),format="GTiff", overwrite = TRUE)
    #return rasters
    #return(rs_euc_norm)
    
  } else {
    cat("File already created ... \n \n")
   # rs_euc_norm <- raster::raster(paste(out_dir,"/env_score_",clus_method,".tif",sep=""))
    #return(rs_euc_norm)
    
  }
  
}
