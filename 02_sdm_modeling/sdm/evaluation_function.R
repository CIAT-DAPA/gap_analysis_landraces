###################################################################
####ASD_15 function using Ramirez Villegas et al., 2010 approach###
###################################################################

ASD15_function <- function(model_outDir){
  suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
  
  ###ASD15
  esdCpt <- raster(paste(model_outDir,"/",occName,"_prj_std.tif",sep=""))
  dumm <- raster(paste(model_outDir,"/",occName,"_prj_median_thr.tif",sep=""))
  
  esdCpt[which(dumm[] < 0.001)] <- NA
  
  #create 0,1 raster with areas below 0.15 STD (below=1, above=0)
  esdCpt_ref <- esdCpt
  esdCpt_ref[which(!is.na(esdCpt[]))] <- 1
  
  #create 0,1 raster with areas above 0.15 STD (below=0, above=1)
  esdCpt_a15 <- esdCpt
  esdCpt_a15[which(esdCpt[] >= 0.15)] <- 1
  esdCpt_a15[which(esdCpt[] < 0.15)] <- 0
  
  #make a raster of area
  dist_area <- raster::area(esdCpt)
  
  #calculate size of distribution within native area, and within thresholded distribution
  #total, and above 0.15 STD.
  szCpt <- dist_area * esdCpt_ref
  szCptUncertain <- dist_area * esdCpt_a15
  rateCpt <- sum(szCptUncertain[],na.rm=T) / sum(szCpt[],na.rm=T) * 100
  

  rm(dumm,esdCpt)

  return(rateCpt)
}

###############################
####Evaluation per_replicate###
###############################

evaluation_function <- function(m2, eval_sp_Dir, spData){
  suppressMessages(if(!require(sdm)){install.packages("sdm");library(sdm)}else{library(sdm)})
  
  cat("Performing evaluation for 10 thresholds\n")
  
  if (!file.exists(paste(eval_sp_Dir,"/",occName,"_eval.csv",sep=""))) {
    #thresholds
    thr<-c("sp=se", "max(se+sp)", "min(cost)", "minROCdist", "max(kappa)", "max(ppv+npv)", "ppv=npv", "max(NMI)", "max(ccr)", "prevalence")
    #Checking if there is a replicates folder 
    eval_sp_Dir_rep <- paste0(eval_sp_Dir,"/","replicates");if(!file.exists(eval_sp_Dir_rep)){dir.create(eval_sp_Dir_rep)}
    #Gathering evaluation per replicate
    m2_eval<- lapply(1:10,function(i){
    m2_eval <- sdm::getEvaluation(m2, opt=i, stat=c('AUC', #Area Under the ROC Curve
                                                 'COR', #Correlation
                                                 'Deviance',
                                                 'obs.prevalence',
                                                 'threshold',
                                                 'sensitivity',
                                                 'specificity',
                                                 'TSS', #True Skill Score
                                                 'Kappa', #Cohens Kappa
                                                 'NMI', #Normalized Mutual Information
                                                 'phi', #Correlation coefficient for binary data
                                                 'ppv', #Positive Predictive Value
                                                 'npv', #Negative Predictive Value
                                                 'ccr', #Correct Classification Rate
                                                 'prevalence'))
      m2_eval$FalseNeg <- NA; m2_eval$FalseNeg <- 1-m2_eval$sensitivity
      m2_eval$threshold_meth <- NA;m2_eval$threshold_meth <- thr[[i]]
      
      m2_eval <- merge(sdm::getModelInfo(m2),m2_eval,by="modelID")
      write.csv(m2_eval, paste(eval_sp_Dir_rep,"/",occName,"_",thr[[i]],".csv",sep=""),row.names=F,quote=F)
      
return(m2_eval)
      
})
    
    #Summarizing all 10 thresholds in one file using median.
    
    cat("Getting summarized file for the 10 replicates","\n")
    
    m2_eval_FN <- lapply(1:10, function(i){
      m2_eval_mean <- as.data.frame(matrix(ncol=18,nrow=1))
      colnames(m2_eval_mean) <- c("Threshold_meth","opt",colnames(m2_eval[[i]][,10:(ncol(m2_eval[[i]])-1)]))
      m2_eval_mean[1,1] <- thr[[i]] 
      m2_eval_mean[1,2] <- i
      m2_eval_mean[1,3:18] <- apply(m2_eval[[i]][,10:(ncol(m2_eval[[i]])-1)], 2, FUN = median)#colMeans(m2_eval[[i]][,10:ncol(m2_eval[[i]])],na.rm=T)
      return(m2_eval_mean)
    })
    m2_eval_FN <- do.call (rbind,m2_eval_FN)
    
    write.csv(m2_eval_FN, paste(eval_sp_Dir,"/",occName,"_eval_thresholds.csv",sep=""),row.names=F,quote=F)
    
    cat("Choosing threshold using maximum TSS and maximum specificity","\n")
    
   #Choosing threshold using maximum TSS and maximum specificity.  
   m2_eval_FN <- m2_eval_FN[which(m2_eval_FN$TSS == max(m2_eval_FN$TSS)),]
   m2_eval_FN <- m2_eval_FN[which(m2_eval_FN$specificity == max(m2_eval_FN$specificity)),]
   if(nrow(m2_eval_FN)>1){
     m2_eval_FN <- m2_eval_FN[1,]
   } else {
   m2_eval_FN <- m2_eval_FN[1,]
   }
   
   m2_eval<- m2_eval[[m2_eval_FN$opt]]
   
   #Perform null model
    nAUC <- nullModel_calculate(spData,mask)
    m2_eval$cAUC <- m2_eval$AUC+.5-max(c(.5,nAUC,na.rm=T))
    #Saving evaluation file
    write.csv(m2_eval, paste(eval_sp_Dir,"/",occName,"_eval.csv",sep=""),row.names=F,quote=F)
  } else {
    m2_eval <- read.csv(paste(eval_sp_Dir,"/",occName,"_eval.csv",sep=""),header=T)
  }
   return(m2_eval)
}


#####################################
#FINAL EVALUATION()

final_evaluation <-function(m2_eval,occName){
m2_eval_final <- read.csv(paste0(eval_sp_Dir,"/",occName,"_eval_thresholds.csv"),header=T)
m2_eval_final <- m2_eval_final[which(as.character(m2_eval_final$Threshold_meth)==as.character(m2_eval$threshold_meth[[1]])),]
m2_eval_final$SDAUC <- NA;m2_eval_final$SDAUC <- sd(m2_eval$AUC,na.rm=T)
m2_eval_final$ASD15 <- NA;m2_eval_final$ASD15 <- ASD15_function(model_outDir)
m2_eval_final$cAUC <-NA;m2_eval_final$cAUC <-  median(m2_eval$cAUC,na.rm=T)
m2_eval_final$IS_VALID <- NA

if(m2_eval_final$AUC >= 0.7 &
   m2_eval_final$ASD15 <= 10  &
   m2_eval_final$cAUC >= 0.4 & 
   m2_eval_final$SDAUC < 0.15
)
{
  m2_eval_final$IS_VALID <- TRUE
} else {
  m2_eval_final$IS_VALID <- FALSE
  
      }  

write.csv(m2_eval_final,paste0(eval_sp_Dir,"/","Final_evaluation.csv"),quote=F,row.names=F)

return(m2_eval_final)

}