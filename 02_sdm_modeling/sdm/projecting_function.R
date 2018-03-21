require(raster)

projecting_function <-function(m2, m2_eval, model_outDir, nCores, obj.size){
  
  # Creating projected models replicates directory
  n_projs <- length(list.files(path = model_outDir_rep, pattern = "_prj_th_rep-"))
  if(n_projs != nrow(m2_eval)*.5){
    
    # Predicting replicates 
    p2m_all <-lapply(1:nrow(m2_eval),function(m_i){
      
      if (!file.exists(paste0(model_outDir_rep,"/",occName,"_prj_rep-",m_i,".tif"))) {
        
        cat("...processing model replicate =", m_i, "\n")
        
        p2m <- predict(object = m2, method = "maxent",
                       w = m_i,
                       newdata = clim_layer,
                       mean = F,
                       overwrite = F,
                       nc = nCores,
                       method = "foreach",
                       obj.size = obj.size,
                       filename = paste0(model_outDir_rep,"/",occName,"_prj_rep-",m_i,".tif"))#,obj.size=3)
        
        #thresholding
        p2m_th <- p2m
        p2m_th[which(p2m_th[] < m2_eval$threshold[m_i])] <- NA
        writeRaster(p2m_th, paste(model_outDir_rep,"/",occName,"_prj_th_rep-",m_i,".tif",sep=""),format="GTiff")
        
        #writeRaster(p2m,paste0(model_outDir_rep,"/",occName,"_prj_rep-",m_i,".tif"))
        
      } else {
        
        cat("loading model replicate=",m_i,"\n")
        
        p2m <- raster(paste0(model_outDir_rep,"/",occName,"_prj_rep-",m_i,".tif"))
      }
      return(p2m)
    })
  }
  
  #calculate mean, median, s.d. of models
  
  cat("Calculating Mean, median and sd for model replicates","\n")
  
  prj_stk <- stack(paste(model_outDir_rep,"/",occName,"_prj_th_rep-",1:nrow(m2_eval),".tif",sep=""))
  
  #Mean
  
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_mean.tif",sep=""))) {
    prj_men <- calc(prj_stk, fun=function(x) {mean(x,na.rm=T)})
    writeRaster(prj_men, paste(model_outDir,"/",occName,"_prj_mean.tif",sep=""),format="GTiff")
  } else {
    prj_men <- raster(paste(model_outDir,"/",occName,"_prj_mean.tif",sep=""))
  }
  
  #Median
  
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_median.tif",sep=""))) {
    prj_mdn <- calc(prj_stk, fun=function(x) {median(x,na.rm=T)})
    writeRaster(prj_mdn, paste(model_outDir,"/",occName,"_prj_median.tif",sep=""),format="GTiff")
  } else {
    prj_mdn <- raster(paste(model_outDir,"/",occName,"_prj_median.tif",sep=""))
  }
  
  # Standard deviation
  
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_std.tif",sep=""))) {
    prj_std <- calc(prj_stk, fun=function(x) {sd(x,na.rm=T)})
    writeRaster(prj_std, paste(model_outDir,"/",occName,"_prj_std.tif",sep=""),format="GTiff")
  } else {
    prj_std <- raster(paste(model_outDir,"/",occName,"_prj_std.tif",sep=""))
  }
  
  
  cat("Calculating ensemble approaches","\n")
  
  #Converting median file to a thresholded raster file
  
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_median_thr.tif",sep=""))) {
    prj_th <- prj_mdn
    prj_th[which(!is.na(prj_th[]))] <- 1
    writeRaster(prj_th, paste(model_outDir,"/",occName,"_prj_median_thr.tif",sep=""),format="GTiff")
  } else {
    prj_th <- raster(paste(model_outDir,"/",occName,"_prj_median_thr.tif",sep=""))
  }
  
  #Using quantile 0.75 to get a fitted distribution.
  
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_median_q.tif",sep=""))) {
    prj_mdn_q <- prj_mdn
    prj_mdn_q[which(prj_mdn_q[] < as.numeric(quantile(prj_mdn[],na.rm=T,probs=0.75)))] <- NA 
    writeRaster(prj_mdn_q, paste(model_outDir,"/",occName,"_prj_median_q.tif",sep=""),format="GTiff")
  } else{
    prj_mdn_q <- raster(paste(model_outDir,"/",occName,"_prj_median_q.tif",sep=""))  
  }
  
  #thresholding quantile median approach
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_median_q_thr.tif",sep=""))) {
    prj_mdn_q_th <- prj_mdn_q
    prj_mdn_q_th[which(!is.na(prj_mdn_q_th[]))] <- 1
    writeRaster(prj_mdn_q_th, paste(model_outDir,"/",occName,"_prj_median_q_thr.tif",sep=""),format="GTiff")
  } else {
    prj_mdn_q_th <- raster(paste(model_outDir,"/",occName,"_prj_median_q_thr.tif",sep=""))
  }
  
  
  #Getting MCAA approach (Number of models matching each other, using thresholded replicates)
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_ens_MCAA.tif",sep=""))) {
    prj_stk <- stack(lapply(1:nlayers(prj_stk),function(i){
      cat(i,"\n")
      x<-prj_stk[[i]]
      x[which(!is.na(x[]))]<-1
      return(x)
    })
    )
    #Saving matched models frequencies
    prj_stk_ens <- sum(prj_stk,na.rm = T)/nlayers(prj_stk); prj_stk_ens[which(prj_stk_ens[]==0)] <- NA
    writeRaster(prj_stk_ens, paste(model_outDir,"/",occName,"_prj_ens_MCAA.tif",sep=""),format="GTiff")
  } else {
    prj_stk_ens <-  raster(paste(model_outDir,"/",occName,"_prj_ens_MCAA.tif",sep=""))
  }
  
  
  #Using quantile 50% (a half of model) approach to get areas availables
  if (!file.exists(paste(model_outDir,"/",occName,"_prj_ens_MCAA_thr.tif",sep=""))) {
    prj_stk_ens_th <- prj_stk_ens
    prj_stk_ens_th[which(prj_stk_ens_th[]< as.numeric(quantile(prj_stk_ens_th[],na.rm=T,probs=0.5)))] <- NA
    prj_stk_ens_th[which(prj_stk_ens_th[] >= as.numeric(quantile(prj_stk_ens_th[],na.rm=T,probs=0.5)))] <- 1
    writeRaster(prj_stk_ens_th, paste(model_outDir,"/",occName,"_prj_ens_MCAA_thr.tif",sep=""),format="GTiff")
  } else {
    prj_stk_ens_th <-  raster(paste(model_outDir,"/",occName,"_prj_ens_MCAA_thr.tif",sep=""))
  }
  #_prj_thr_MCAA_thr.tif
  return(prj_th)
  cat("Done!","\n")
}