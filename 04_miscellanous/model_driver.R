# Run SDMs
# Chrystiam Sosa, Julian Ramirez-Villegas
# CIAT, Nov 2017

# Base directory
# baseDir <- "~/nfs/workspace_cluster_9/gap_analysis_landraces"
# baseDir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces"

# Software directory
# srcDir <- paste(baseDir,"/Scripts",sep="")

# Source functions
model_driver <- function(sp_Dir, mask, occName, extension_r, all, overwrite, clsModel, correlation, pa_method){
  
  if(!file.exists(paste0(sp_Dir, "/sdm_variables_selected.csv"))){
    
    cat(">>> Variables for SDM process have not been selected. Processing ...\n")
    
    # loading mask
    mask <- mask
    
    # selected genepool occurrences
    occFile <- paste0(classResults, "/", crop, "_", level, "_bd.csv")
    
    # SWD using all variables provided
    
    var_names_generic <- list.files(climDir, pattern = paste0(extension_r, "$"), full.names = F); var_names_generic <- gsub(extension_r, "", var_names_generic)
    var_names_sp      <- list.files(paste0(baseDir, "/input_data/by_crop/", crop, "/raster/", region), pattern = paste0(extension_r, "$"), full.names = F); var_names_sp <- gsub(extension_r, "", var_names_sp)
    
    # create occurrences, background, and swd (occ+bg)
    if(overwrite == T){
      swd <- samples_create(occFile, occName, backDir, occDir, swdDir, mask, climDir, clim_spDir, clsModel, extension_r, var_names_generic, var_names_sp, overwrite = T, correlation = correlation, pa_method = pa_method); gc()
    } else {
      swd <- samples_create(occFile, occName, backDir, occDir, swdDir, mask, climDir, clim_spDir, clsModel, extension_r, var_names_generic, var_names_sp, overwrite = F, correlation = correlation, pa_method = pa_method); gc()
    }
    
    # Getting variables to use in the SDM
    var_names <- colnames(swd)[!colnames(swd) %in% c("id", "species", "status", "lon", "lat")]
    write.csv(x = var_names, file = paste0(sp_Dir, "/sdm_variables_selected.csv"), row.names = F)
    
    return(var_names)
    
  } else {
    
    cat(">>> Variables for SDM process have been selected. Loading ...\n")
    var_names <- read.csv(paste0(sp_Dir, "/sdm_variables_selected.csv"))
    var_names <- as.character(var_names$x)
    
  }
  
}
