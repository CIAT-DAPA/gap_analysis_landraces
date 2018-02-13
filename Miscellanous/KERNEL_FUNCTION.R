#####################################
# KERNEL DENSITY RASTER ESTIMATION  #
# Author: Chrystian C. Sosa         #
# Date: 2017/02/12                  #
#####################################
#https://cran.r-project.org/web/packages/adehabitatHR/adehabitatHR.pdf

require(spatstat);require(raster);require(sp);require(adehabitatHR);require(SDMTools)

###############

RASTER_KERNEL<-function(species,mask,occurrences,out_dir,spatstat){
  cat("Reading mask and occurrences","\n")
  
  ### Reading occurrences 
  sp::coordinates(occurrences)<-~lon+lat
  crs(occurrences) <- crs(mask)
  
  if(spatstat==TRUE){
    ver<-"SPAT"}else{
      ver<-"ADE"  
    }
  #####
  if(spatstat==TRUE){
    cat("Using Kernel Classical version!","\n")
    
    
    ### Transforming mask to owin object 
    w <- spatstat::owin(xrange=c(extent(mask)[1],extent(mask)[2]),
                        yrange =c(extent(mask)[3],extent(mask)[4]),
                        mask=matrix(TRUE,dim(mask)[1],dim(mask)[2])
    )
    
    ### Transforming occurrences to ppp object 
    occurrences_ppp <- spatstat::ppp(x=occurrences@coords[,1],y=occurrences@coords[,2],window=w)
    
    ### Calculating Cross Validated Bandwidth Selection for Kernel Density usingh MSE
    cat("Cross Validated Bandwidth Selection for Kernel Density usingh MSE","\n")
    bw_dig <- spatstat::bw.diggle(occurrences_ppp)
    
    ### Calculating density
    cat("Calculating Kernel density using Cross Validated Bandwidth Selection for Kernel Density parameter","\n")
    
    kernel <- spatstat::density.ppp(x=occurrences_ppp,sigma=bw_dig,at="pixels",verbose=F,diggle=T)
    kernel <- raster::raster(kernel);rm(w,bw_dig);gc()
    
  }else{
    cat("Using Adehabitat Kernel UD version!","\n")
    rAsc <- asc.from.raster(mask);rAsc <- adehabitatMA::asc2spixdf(rAsc);gc()
    kernel <- adehabitatHR::kernelUD(occurrences,h="href",grid=rAsc)
    res <- as.data.frame(kernel[[1]])
    sp::coordinates(res) <- coordinates(kernel)
    sp::gridded(res) <- TRUE
    sp::proj4string(res) <- CRS(proj4string(kernel))
    
    kernel<- raster::raster(res);rm(res,rAsc);gc()
  }
  
  
  ### Rasterizing density object
  
  crs(kernel) <- crs(mask)
  
  ### Fitting to mask parameters
  cat("Fitting Kernel density to mask","\n")
  kernel <- raster::crop(kernel, mask)
  kernel <- kernel * mask
  
  ### Saving raster object
  cat("Saving raster object to be used","\n")
  raster::writeRaster(kernel,paste0(out_dir,"/",species,"_",ver,"_Kernel.tif"))
  
  return(kernel)
  
  
  cat("     ","\n")
  cat("DONE!","\n")
  cat("     ","\n")
  
}



# species <- "Andean"
# 
# ### Calling paths to perform analysis
# root <- "U:"
# input_dir <- paste0(root,"/","Input_data")
# sdm_dir <- paste0(input_dir,"/","SDMs")
# out_dir <- input_dir
# ### Reading mask
# mask <- raster::raster(paste0(input_dir,"/","mask_wb_c_ant.tif"))
# 
# ### Reading occurrences
# occurrences <- read.csv(paste0(sdm_dir,"/","occurrences","/","occ_",species,".csv"),header=T)
# occurrences <- occurrences[,c("lon","lat")]


#x<-RASTER_KERNEL(species,mask,occurrences,out_dir,spatstat=F)
#x<-RASTER_KERNEL(species,mask,occurrences,out_dir,spatstat=T)
