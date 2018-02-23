require(sdm)

sdm_approach_function <- function(occName,spData,sp_Dir,eval_sp_Dir,model_outDir,var_names,nCores,nReplicates,beta,feat){
  
  
  #specify dataset for model
  frm <- as.formula(paste(occName,"~",paste(names(spData)[4:ncol(spData)],sep="+",collapse="+"),"+coords(lon+lat)",sep=""))
  d <- sdm::sdmData(frm,train=spData)

  #running SDMs
  cat("running sdm platform using MaxEnt 3.4.1","\n")
  
  if (!file.exists(paste0(sp_Dir,"/","sdm.sdm"))) {
    #create model
    #frm <- as.formula(paste(occName,"~.",sep=""))
    m2 <- sdm::sdm(frm,data=d,methods=c('maxent'), #brt
                   replication='cv',
                   test.percent=20,
                   cv.folds=5, #5
                   n=nReplicates,#5
                   modelSettings=list(maxent=list(feat=feat,beta=beta)), #beta,prevalence,feat,args
                   var.selection=F,
                   nc=nCores,
                   overwrite=F)
    
    #write model
    sdm::write.sdm(m2,paste0(sp_Dir,"/","sdm"))
  } else {
    m2 <- sdm::read.sdm(paste0(sp_Dir,"/","sdm.sdm"))
  }
  
  return(m2)
 
}