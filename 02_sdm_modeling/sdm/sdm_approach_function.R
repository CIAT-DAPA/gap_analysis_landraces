library(sdm)

sdm_approach_function <- function(occName, spData, model_outDir, var_names, nCores, nFolds, beta, feat){
  
  # Specify dataset for model
  frm <- as.formula(paste(occName, "~", paste(var_names, sep = "+", collapse = "+"), "+coords(lon+lat)", sep = ""))
  d <- sdm::sdmData(frm, train = spData)

  # Running SDMs
  cat("running sdm platform using MaxEnt 3.4.1\n")
  
  if (!file.exists(paste0(model_outDir, "/sdm.sdm"))) {
    
    # Create model
    m2 <- sdm::sdm(frm, data = d, methods = c('maxent'),
                   replication = 'cv',
                   test.percent = 20,
                   cv.folds = nFolds,
                   n = 1, # Replicates
                   modelSettings = list(maxent = list(feat = feat, beta = beta)),
                   var.selection = F,
                   parallelSettings = list(ncore = nCores, method = "parallel"),
                   overwrite = F)
    
    #write model
    sdm::write.sdm(m2, paste0(model_outDir, "/sdm"))
  } else {
    m2 <- sdm::read.sdm(paste0(model_outDir, "/sdm.sdm"))
  }
  
  return(m2)
 
}
