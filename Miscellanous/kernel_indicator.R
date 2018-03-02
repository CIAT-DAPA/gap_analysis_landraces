#Kernel indicator

kernel_indicator <- function(kernel,friction,model_outDir,gap_outDir){
  #loading Maxent Model
  model <- raster(paste(model_outDir,"/",occName,"_prj_median.tif",sep=""))
  #loading friction surface
  friction <-  raster(friction)
  #Transforming Kernel in 0 - 1 variable
  kernel2 <- (kernel/max(kernel[],na.rm=T))
  # Returning inverse Kernel
  kernel2 <- 1 - kernel2
  #Gathering first Indicator (kernel * Maxent probability)
  mm1 <- kernel2 * model
  #Gathering second Indicator (First indicator * Friction surface)
  mm2 <- mm1 * friction
  # Saving indicators
  writeRaster(mm1,paste0(gap_outDir,"/","Kernel_indicator.tif"))
  writeRaster(mm2,paste0(gap_outDir,"/","Kernel_indicator_FS.tif"))
  
  return(mm1)
  
}