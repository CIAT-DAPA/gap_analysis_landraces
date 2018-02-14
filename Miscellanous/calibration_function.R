############################################
CreateMXArgs <- function(calibration){
  mxnt.args<- c("linear=TRUE")
  
  if (!is.null(calibration)) {
    best.ind<- which.min(calibration$tuning$deltaAICc)
    features <- calibration$tuning$classes[best.ind]
    betamultiplier <- calibration$tuning$regMult[best.ind]
    if(grepl("q", features)){
      mxnt.args <- c(mxnt.args, "quadratic=TRUE")
    } else {
      mxnt.args <- c(mxnt.args, "quadratic=FALSE")
    }
    if(grepl("q", features)){
      mxnt.args <- c(mxnt.args, "quadratic=TRUE")
    } else {
      mxnt.args <- c(mxnt.args, "quadratic=FALSE")
    }
    if(grepl("h", features)){
      mxnt.args <- c(mxnt.args, "hinge=TRUE")
    } else {
      mxnt.args <- c(mxnt.args, "hinge=FALSE")
    }
    if(grepl("p", features)){
      mxnt.args <- c(mxnt.args, "product=TRUE")
    } else {
      mxnt.args <- c(mxnt.args, "product=FALSE")
    }
    if(grepl("t", features)){
      mxnt.args <- c(mxnt.args, "threshold=TRUE")
    } else {
      mxnt.args <- c(mxnt.args, "threshold=FALSE")
    }
    mxnt.args <- c(mxnt.args, paste0("betamultiplier=",betamultiplier))
  } else {
    mxnt.args <- c(mxnt.args, "quadratic=TRUE", "hinge=TRUE", "product=TRUE", "threshold=FALSE", "betamultiplier=1.0")
  }
  return(mxnt.args)
}

###

Calibration_function<-function(spData,save,sp_Dir,ommit){
  
  
  library(devtools)
  if(!require(enmSdm)){
    install_github('adamlilith/omnibus')
    install_github('adamlilith/enmSdm')
    library(omnibus)
    library(enmSdm)
  } else {
    library(omnibus)
    library(enmSdm)
  }
  
  if(ommit==F){
    
    calibration <- trainMaxNet(data = spData[,c(1,4:ncol(spData))], regMult = seq(0.5, 6, 0.5), out = c('tuning'), verbose = TRUE)#, jackknife = F,forceLinear=T)
    
    if(save==T){
      write.csv(calibration$tuning,paste0(sp_Dir,"/","calibration.csv"),header=T,quote=F)
    }else{
      cat("Not file to save","\n")
    }
  }else{
    cat("Ommiting calibration step","\n")
    calibration <- NULL
  }
  args <- CreateMXArgs(calibration)
  return(args)
}


#args<-Calibration_function(spData=spData,save=T,sp_Dir=sp_Dir,ommit=F)
