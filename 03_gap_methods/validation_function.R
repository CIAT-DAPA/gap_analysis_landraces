# Validation process: landrace gap analysis
# Harold Achicanoy
# CIAT, March 2018


####################################################################################################################################
######################## FUNCTION TO CREATE ALL NECCESSARY FILES TO VALIDATE THE GAP SCORES ########################################
####################################################################################################################################


# cat(">>> Applying validation process for gap metrics <<<\n")
# gap_valDir <- paste0(baseDir, "/results/", crop, "/", level, "/", occName, "/", region,"/gap_validation")



allglobal <- function() {
  lss <- ls(envir = parent.frame())
  for (i in lss) {
    assign(i, get(i, envir = parent.frame()), envir = .GlobalEnv)
  }
}
allglobal()

# occName = occName
# gap_valDir = gap_valDir
# buffer_radius = 1
# density_pattern = 3
# geo_score = c("cost_dist", "delaunay")

validation_process <- function(occName         = occName,
                               gap_valDir      = gap_valDir,
                               buffer_radius   = 1, # Radius of 100 km for excluding occurrences
                               density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
                               geo_score       = c("cost_dist", "delaunay"),# Can be: "cost_dist", "kernel", "delaunay"
                               use.Arcgis      = TRUE,
                               n.points        = 5,
                               doPar           = FALSE,
                               use.maxnet      = FALSE) 
{
  
  cat(">>> Loading occurrence data ... \n")
  spData <- raster::shapefile(paste0(occDir,"/Occ.shp"))
  spData <- unique(as.data.frame(spData)); rownames(spData) <- 1:nrow(spData)
  names(spData)[2:3] <- c("lon", "lat")
  
  cat(">>> Loading SDM raster... \n")
  SDM <- raster(paste0(model_outDir,"/", occName,"_prj_median.tif"))
  
  cat(">>> Loading kernel density map for all points ... \n")
  kernel <- raster(paste0(sp_Dir, "/gap_models/kernel.tif"))
  kernel_class <- raster(paste0(sp_Dir, "/gap_models/kernel_classes.tif"))
  densities <- c("low", "medium", "high")
  
  kernel_class[kernel_class[] != density_pattern] <- NA
  kernel_upt <- raster::mask(x = kernel_class, mask = SDM)
  
  points_to_sample <- as.data.frame(kernel_upt, xy = TRUE, na.rm = TRUE)
  names(points_to_sample) <- c("lon", "lat", "value")
  
  
  rm(kernel_class, SDM); g <- gc(); rm(g)
  
  occ <- spData
  
  cat(">>> Selecting 5 points randomly using kernel density level as weights ... \n")
  set.seed(1234)
  seedList <- round(runif(n = n.points, min = 1, max = 10000))
  
  # ----------------------------------------------------------------------------------- #
  allglobal()
  
  if(doPar){
    cat("Doing process in multiple cores... \n")
    use.Arcgis <- FALSE
    cl <- makeSOCKcluster(n.points)
    registerDoSNOW(cl)
    #on.exit(stopCluster(cl))
    pb <- tkProgressBar( max = n.points, label = "Creating needed files for validation, pls be patient...")
    progress <- function(n) setTkProgressBar(pb, n)
    opts <- list(progress=progress)
    
    foreach( i = 1:n.points, 
             .packages = devtools::loaded_packages()$package, 
             .options.snow=opts,  
             .export = ls(globalenv())) %dopar% {
               
              source(paste0(srcDir, "/03_gap_methods/validation_helpers.R"), local = TRUE)  
              
             }
    
    stopCluster(cl)
    
  }else{
    
    for(i in 1:n.points){
      cat(paste0(">>> Processing point: ", i, " <<<\n"))
      
      source(paste0(srcDir, "/03_gap_methods/validation_helpers.R"), local = TRUE)  
       
      
    }
    
  }
  
  
    
    
    

  
}#end function

# validation_process(occName = occName,
#                    gap_valDir = gap_valDir,
#                    buffer_radius = 1, # Radius of 100 km for excluding occurrences
#                    density_pattern = 3, # Density pattern (1: low density, 2: medium density, 3: high density)
#                    geo_score = c("cost_dist", "delaunay"))
