#Create sample (background, occurrences, swd) for SDMs
#Chrystiam Sosa, Julian Ramirez-Villegas
#CIAT, Nov 2017

#function to create background, occurrence, and swd (bg+occ) samples
samples_create <- function(occFile, occName, backDir, occDir, swdDir, mask, climDir, clim_spDir, extension_r, var_names_generic, var_names_sp, overwrite = F, correlation = 0){
  library(dismo); library(raster); library(caret); library(usdm); library(plsdepot); library(FactoMineR)
  
  #load raster files
  cat("Loading raster files","\n")
  current_clim_layer_generic <- lapply(paste0(climDir, "/", var_names_generic, extension_r), raster)
  current_clim_layer_sp <- lapply(paste0(baseDir, "/input_data/by_crop/", crop, "/raster/", region, "/", var_names_sp, extension_r), raster)
  current_clim_layer <- stack(current_clim_layer_generic, current_clim_layer_sp)
  
  # background samples file name
  outBackName <- paste0(backDir, "/bg_", occName, ".csv")
  outOccName <- paste0(occDir, "/occ_", occName, ".csv")
  outSWDName <- paste0(swdDir, "/swd_", occName, ".csv")
  outSWDComplete_Name <- paste0(swdDir, "/swd_Complete_", occName, ".csv")
  #create background if it doesnt exist
  if (!file.exists(outBackName) | overwrite) {
    cat("Processing:", paste(occName), "\n")
   # spData <- readRDS(occFile)
    spData <- read.csv(occFile, header = T)
    spData[,clsModel] <- tolower(spData[,clsModel])
    spData <- spData[which(spData[,clsModel] == occName),]
    
    #create random points
    cat("Creating random points\n")
    
    mask <- raster(mask)
    #verification of unique locations #commented out
    #tcell <- unique(cellFromXY(mask,spData[,c("Longitude","Latitude")]))
    #xycell <- xyFromCell(mask,tcell)
    #plot(spData[,c("Longitude","Latitude")], ty="p", pch=20, col="red")
    #points(xycell, pch=21, col="black")
    
    #number of samples
    nSamples <- length(unique(cellFromXY(mask, spData[,c("Longitude", "Latitude")]))) * 10
    cat("generating", nSamples, "pseudoabsences for n =", nSamples/10, "presences\n")
    
    xran <- xyFromCell(mask, which(!is.na(mask[])))
    xran <- cbind(cell=which(!is.na(mask[])), xran)
    xran <- as.data.frame(xran)
    spDataCells <- unique(cellFromXY(mask, spData[,c("Longitude","Latitude")]))
    xran <- xran[c(!xran$cell %in% spDataCells),]
    set.seed(1234)
    xranSample <- xran[sample(nrow(xran), size = nSamples, replace = F),]
    row.names(xranSample) <- 1:nrow(xranSample)
    xranSample <- xranSample[,c("x","y")]
    names(xranSample) <- c("lon","lat")
    #plot(xranSample$x, xranSample$y, ty="p", pch=20, col="red")
    #points(spData$Longitude, spData$Latitude, pch=20, col="black")
    cat(nrow(xranSample), "pseudoabsences generated for n =", nSamples/10, "presences\n")
    
    #extract variable data
    ex_raster_env <- as.data.frame(raster::extract(current_clim_layer, xranSample))
    z <- cbind(id = 1:nrow(xranSample), species = occName, status = 0, xranSample, ex_raster_env)
    z <- z[complete.cases(z),]
    cat(nrow(z), "pseudoabsences ready to use\n")
    occ <- z
    #preparing samples
    occSample <- unique(spData[,c("Longitude", "Latitude")])
    names(occSample) <- c("lon", "lat")
    occ_env_data <- as.data.frame(raster::extract(current_clim_layer, occSample))
    occSample <- cbind(id = 1:nrow(occSample), species = occName, status = 1, occSample, occ_env_data)
    occSample <- occSample[complete.cases(occSample),]
    
    #prepare swd
    swdSample_Complete <- rbind(occSample, z)
    swdSample <- swdSample_Complete
    #excluiding correlation
    if(correlation == 0){
      cat("Ommiting the correlation approach\n")
      
      swdSample <- swdSample_Complete
      
    }
    
    #Using choose variables algorithms (Correlation, VIF, or PCA + VIF)
    if(correlation == 1){
      cat("Using Pearson correlation approach\n")
      
      descrCor <- cor(swdSample[,-c(1:5)])
      highlyCorDescr <- caret::findCorrelation(descrCor, cutoff = .75)
      swdSample <- swdSample[,!colnames(swdSample) %in% (colnames(descrCor)[highlyCorDescr])]
      
    }
    
    if(correlation == 2){
      cat("Using VIF approach\n")
      
      descrCor <- vifstep(swdSample[,-c(1:5)], th = 5)
      highlyCorDescr <- descrCor@excluded
      swdSample <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
      
    }
    
    if(correlation==3){
      cat("Using PCA + VIF approach","\n")
      
      z <- FactoMineR::PCA(X = swdSample[,-c(1:5)], ncp = 5, scale.unit = T, graph = F)
      # Selecting a number of components based on the cumulative ratio of variance which has more than 70%
      ncomp <- as.numeric(which(z$eig[,ncol(z$eig)] >= 70)[1])
      vars <- rownames(z$var$cos2)[unlist(lapply(X = 1:nrow(z$var$cos2[,1:ncomp]), FUN = function(r){
        if(length(which(z$var$cos2[r,1:ncomp] >= .15)) > 0){
          res <- T
        } else {
          res <- F
        }
        return(res)
      }))]
      #vars1 <- z$var$cor[,1] > 0.7 | z$var$cor[,1] < -0.7
      #vars2 <- z$var$cor[,2] > 0.7 | z$var$cor[,2] < -0.7
      #vars <- c(vars1, vars2)
      #vars <- names(vars[which(vars == TRUE)])
      
      descrCor <- usdm::vifstep(swdSample[,vars], th = 10)
      highlyCorDescr <- descrCor@excluded
      swdSample <- swdSample[,!colnames(swdSample) %in% highlyCorDescr]
  }
    cat("Saving csv files","\n")

    write.csv(swdSample_Complete, outSWDComplete_Name, quote = F, row.names = F)
    write.csv(occ, outBackName, quote = F, row.names = F)
    write.csv(occSample, outOccName, quote = F, row.names = F)
    write.csv(swdSample, outSWDName, quote = F, row.names = F)
    
  } else {
    
    cat("already processed\n")
    swdSample <- read.csv(outSWDName, header = T)
    
  }
  return(swdSample)
}
