####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################



nullModel_calculate <- function(spData,mask) {
  
  if(class(mask)=="character"){
    mask <- raster(mask)
  }else{
    mask <- mask
  }
  #1.1a Load the data and fitting to a Native area
  inData <- spData[which(spData[,occName]==1),]
  nOcc <- nrow(inData)
  if(nOcc==0){
    AUC<-NA
  } else if (nOcc > 2) {
    cat(nOcc," records","\n")
    
    # Step 2: training and testing sets
    set.seed(1235)
    training <- inData[which(kfold(inData, k=3)!=3),c("lon", "lat")]
    rownames(training) <- 1:nrow(training)
    set.seed(1235)
    testing  <- inData[which(kfold(inData, k=3)==3),c("lon", "lat")]
    rownames(testing) <- 1:nrow(testing)
    
    # Step 3: Geographic distance model
    Null_model <- geoDist(training, lonlat=T)
    
    # Step 4: evaluate model
    #pabs <- randomPoints(mask=env_data[[1]], n=nOcc*10)
    #pabs <- as.data.frame(pabs)
    #names(pabs) <- c("lon", "lat")
    
    nSamples <- nOcc*10
    xran <- xyFromCell(mask, which(!is.na(mask[])))
    xran <- cbind(cell=which(!is.na(mask[])), xran)
    xran <- as.data.frame(xran)
    spDataCells <- unique(cellFromXY(mask, inData[,c("lon","lat")]))
    xran <- xran[c(!xran$cell %in% spDataCells),]
    xranSample <- xran[sample(nrow(xran),size=nSamples,replace=F),]
    row.names(xranSample) <- 1:nrow(xranSample)
    xranSample <- xranSample[,c("x","y")]
    names(xranSample) <- c("lon","lat")
    pabs <- xranSample
    #plot(xranSample,pch=20)
    
    eval <- dismo::evaluate(p=testing, a=pabs, model=Null_model)
    AUC <- eval@auc
  }else if (nOcc ==2) {
    cat("WARNING...",nOcc," records ","\n")
    set.seed(1235)
    training <- inData[which(kfold(inData, k=2)!=2),c("lon", "lat")]
    rownames(training) <- 1:nrow(training)
    set.seed(1235)
    testing  <- inData[which(kfold(inData, k=2)==2),c("lon", "lat")]
    rownames(testing) <- 1:nrow(testing)
    
    # Step 3: Geographic distance model
    Null_model <- geoDist(training, lonlat=T)
    
    # Step 4: evaluate model
    pabs <- randomPoints(mask=env_data[[1]], n=10000)
    pabs <- as.data.frame(pabs)
    names(pabs) <- c("lon", "lat")
    eval <- dismo::evaluate(p=testing, a=pabs, model=Null_model)
    AUC <- eval@auc
  } else if (nOcc ==1) {
    cat("WARNING...",nOcc," records","\n")
    set.seed(1235)
    training <- inData[which(kfold(inData, k=1)==1),c("lon", "lat")]
    rownames(training) <- 1:nrow(training)
    set.seed(1235)
    testing  <- inData[which(kfold(inData, k=1)==1),c("lon", "lat")]
    rownames(testing) <- 1:nrow(testing)
    
    # Step 3: Geographic distance model
    Null_model <- geoDist(training, lonlat=T)
    
    # Step 4: evaluate model
    pabs <- randomPoints(mask=env_data[[1]], n=10000)
    pabs <- as.data.frame(pabs)
    names(pabs) <- c("lon", "lat")
    eval <- dismo::evaluate(p=testing, a=pabs, model=Null_model)
    AUC <- eval@auc
    #AUC<-0.5
  }
  return(AUC)
}  

