#Julian Ramirez-Villegas
#March 2018

#open environmental score, geographic score and sdm probability
#and combine them into a single score map

#function details
#note: gap_method can be "cost_dist" "kernel" or "delaunay"
#wd <- "~/nfs/workspace_cluster_9/gap_analysis_landraces/runs"
#wd <- "Z:/gap_analysis_landraces/runs"
#res_dir <- paste(wd,"/results/",crop_name,"/lvl_",level,"/",lv_name,"/",region,sep="")
#sdm_dir <- paste(res_dir,"/prj_models",sep="")
#gap_dir <- paste(res_dir,"/gap_models/", sep="")
#out_dir <- gap_dir
#calc_gap_score(lv_name="mesoamerican",clus_method="hclust_mahalanobis",gap_method="cost_dist",sdm_dir,gap_dir,out_dir)

calc_gap_score <- function(lv_name, clus_method = "hclust_mahalanobis", gap_method = "cost_dist", sdm_dir, gap_dir, out_dir){
  #load libraries
  
  
  
  if(!file.exists(paste(out_dir,"/gap_score_",gap_method,".tif",sep=""))){
    #load sdm projection
    sdm_prj <- raster(paste(sdm_dir,"/",lv_name,"_prj_median.tif",sep="")) #paste(sdm_dir,"/",lv_name,"_prj_median.tif",sep="")
    sdm_prj <- readAll(sdm_prj)
    
    #load specified gap_method raster
    if (gap_method == "cost_dist") {
      #load accession accessibility
      occ_access <- raster(paste(gap_dir,"/cost_dist.tif",sep=""))
      occ_access <- readAll(occ_access)
      occ_access <- crop(occ_access, sdm_prj)
      
      #mask geo_score to sdm, and then normalise (so that geo score is relative to the sdm)
      geo_score <- occ_access
      geo_score <- mask(geo_score, sdm_prj)
      geo_score[which(is.na(geo_score[]) & !is.na(sdm_prj[]))] <- max(geo_score[],na.rm=T)
      geo_score <- geo_score / max(geo_score[], na.rm=T)
      
    } else if (gap_method == "kernel") {
      #method kernel, load kernel
      kern_dens <- raster(paste(gap_dir,"/kernel.tif",sep=""))
      kern_dens <- readAll(kern_dens)
      kern_dens <- crop(kern_dens, sdm_prj)
      
      #mask geo_score to sdm, and then normalise (so that geo score is relative to the sdm)
      geo_score <- kern_dens
      geo_score <- mask(geo_score, sdm_prj)
      geo_score[which(is.na(geo_score[]) & !is.na(sdm_prj[]))] <- max(geo_score[],na.rm=T)
      geo_score <- geo_score / max(geo_score[], na.rm=T)
      geo_score <- 1-geo_score #kernel is density so low values would be gaps
    } else if (gap_method == "delaunay") {
      
      del_triang <- raster(paste(gap_dir,"/delaunay.tif",sep=""))
      del_triang <- readAll(del_triang)
      del_triang <- crop(del_triang, sdm_prj)
      geo_score <- del_triang
      geo_score <- mask(geo_score, sdm_prj)
      geo_score[which(is.na(geo_score[]) & !is.na(sdm_prj[]))] <- max(geo_score[],na.rm=T)
      
    }
    
    #load environmental score (per-cluster env. distance)
    env_score <- raster(paste(gap_dir,"/env_score_",clus_method,".tif",sep=""))
    env_score <- readAll(env_score)
    
    #produce a single gap map by multiplying the three scores: p(x), geo_score, env_score
    # gap_score <- sdm_prj * geo_score * env_score
    # gap_score <- sqrt(sdm_prj * max(geo_score, env_score)) # Calculating geometric mean
    gap_score <- sdm_prj * mean(geo_score, env_score, na.rm = T) # Calculating geometric mean
    
    #get gap threshold
    #gap_thr <- read.csv(paste(res_dir,"/gap_validation/validation_results.csv"), header=T)
    #gap_thr <- gap_thr$threshold
    # gap_thr <- quantile(na.omit(gap_score[]), probs=c(0.9,0.95)) #temporary
    # 
    #classify gap score
    # gap_class <- raster(gap_score)
    # gap_class[which(gap_score[] < gap_thr[1])] <- 0
    # gap_class[which(gap_score[] >= gap_thr[1] & gap_score[] < gap_thr[2])] <- 1
    # gap_class[which(gap_score[] >= gap_thr[2])] <- 2
    #  
    #write gap_score and gap_class rasters
    writeRaster(gap_score, paste(out_dir,"/gap_score_",gap_method,".tif",sep=""), format="GTiff")
    #writeRaster(gap_class, paste(out_dir,"/gap_class_",gap_method,".tif",sep=""), format="GTiff")
    
    #return stack with rasters
   # rstk <- stack(c(gap_score,gap_class))
    return(gap_score)
    
  } else {
    gap_score <- raster::raster(paste(out_dir,"/gap_score_",gap_method,".tif",sep=""))
    return(gap_score)
  }
  
}