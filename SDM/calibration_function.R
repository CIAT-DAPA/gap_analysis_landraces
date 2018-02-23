###############################################################################################
####Creating arguments to run MaxEnt in the SDM Packages (Modified of Jorge Velasquez Script###
###############################################################################################
CreateMXArgs <- function(calibration){
  mxnt.args<- c("linear")
  
  if (!is.null(calibration)) {
    best.ind<- which.min(calibration$deltaAICc)
    features <-as.data.frame(cbind(calibration$linear,calibration$quadratic,calibration$product,calibration$hinge,calibration$threshold))
    names(features) <- c("l","q","p","h","t")
    features <- features[best.ind,]
    features <- unlist(lapply(1:ncol(features),function(i){
      if(features[,i]==TRUE){
        x<-colnames(features[i])
      }else{
        x<-NULL
      }
      return(x)
    }))
    features <- paste(features,collapse="")
    betamultiplier <- calibration$regMult[best.ind]
    
    if(grepl("q", features)){
      mxnt.args <- c(mxnt.args, "quadratic")
    } else {
      mxnt.args <- c(mxnt.args, "")
    }
    if(grepl("h", features)){
      mxnt.args <- c(mxnt.args, "hinge")
    } else {
      mxnt.args <- c(mxnt.args, "")
    }
    if(grepl("p", features)){
      mxnt.args <- c(mxnt.args, "product")
    } else {
      mxnt.args <- c(mxnt.args, "")
    }
    if(grepl("t", features)){
      mxnt.args <- c(mxnt.args, "threshold")
    } else {
      mxnt.args <- c(mxnt.args, "")
    }
    mxnt.args <- c(mxnt.args, paste0("betamultiplier=",betamultiplier))
  } else {
    mxnt.args <- c(mxnt.args, "quadratic", "hinge", "product", "", "betamultiplier=1.0")
  }
  
  mxnt.args <- mxnt.args[which(mxnt.args!="")]
  return(mxnt.args)
}

###############################################################################################
####Calibration function using wright et al., 2014 approach###
###############################################################################################

Calibration_function<-function(spData,save,sp_Dir,ommit){
   library(devtools)
  #require(maxnet)
  if(!require(enmSdm)){
    install_github('adamlilith/omnibus')
    install_github('adamlilith/enmSdm')
    library(omnibus)
    library(enmSdm)
  } else {
    library(omnibus)
    library(enmSdm)
    require(maxnet)
  }
  
if(ommit==F){
  spData <- spData[complete.cases(spData),]
  for(i in 1:ncol(spData)){
    spData[,i]<-as.numeric(spData[,i])
  }
  
  #spData2 <- spData
  # spData2_pres <- spData2[spData2$Andean==1,]
  # spData2_bg <- spData2[spData2$Andean==0,]
  # spData2_bg <- spData2_bg[sample(x=nrow(spData2_bg),size=nrow(spData2_pres)),]
  # spData2 <- rbind(spData2_pres,spData2_bg)
  
  #Calibration using MaxEnt instead of Maxnet R package.
  cat("Performing calibration step","\n")
  tryCatch(expr = {calibration <- enmSdm::trainMaxEnt(data = spData[,c(1,4:ncol(spData))], regMult = seq(0.5, 6, 0.5), out = 'tuning', verbose = FALSE,jackknife = F)#, jackknife = F,forceLinear=T))
  },
  error = function(e){
    cat("Calibration process failed:","\n")
   
    return("Done\n")  
  })
  
  #Testing if the calibration was made
  if(exists("calibration")){
    calibration <- calibration
  } else {
    cat("The calibration was not successful. Using default parametr using Default options","\n")
    calibration <- NULL
  }
  
if(save==T){
  #Saving the calibration file
  write.csv(calibration,paste0(sp_Dir,"/","calibration.csv"),quote=F,row.names = F)
           } else {
  cat("Not file to save","\n")
           }
  } else {
  cat("Ommiting calibration step","\n")
  calibration <- NULL
  }
args <- CreateMXArgs(calibration)
return(args)
cat("Calibration step Done!","\n")

}


#args<-Calibration_function(spData=spData,save=T,sp_Dir=sp_Dir,ommit=F)
  