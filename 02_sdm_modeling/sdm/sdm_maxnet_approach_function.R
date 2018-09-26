# Maxent implementation using maxnet R package
# H. Achicanoy
# CIAT, 2018

suppressMessages(pacman::p_load(maxnet))

# Old function
# cat("Running modeling approach\n")
# m2 <- sdm_approach_function(occName      = occName,
#                             spData       = spData,
#                             model_outDir = sp_Dir,
#                             var_names    = var_names,
#                             nCores       = 5,
#                             nFolds       = 5,
#                             beta         = beta,
#                             feat         = feat)

# What is missing
# 1. Create replicates manually: k-fold cross-validation
# 1.1 Select independently k training points (presences, pseudo-absences)
# 1.2 Fit the model using previous subsets
# 2. Paralellization of the process (if necessary)
# 3. Validation metrics

sdm_approach_function <- function(occName      = occName,
                                  spData       = spData,
                                  model_outDir = sp_Dir,
                                  var_names    = var_names,
                                  nCores       = 5,
                                  nFolds       = 5,
                                  beta         = beta,
                                  feat         = feat){
  
  if(nFolds > 1){
    
    set.seed(1234)
    id <- sample(nFolds, nrow(spData), replace = T, prob = rep(1/nFolds, nFolds))
    rplc <- 1:nFolds %>% purrr::map(.f = function(i){
      p <- spData[id == i, occName]
      data <- spData[id == i, var_names]
      fit.maxent <- maxnet::maxnet(p = p,
                                   data = data,
                                   regmult = beta,
                                   maxnet.formula(p, data, classes = paste0(substr(feat, start = 1, stop = 1), collapse = "")))
      return(fit.maxent)
    })
    saveRDS(rplc, "./maxnet_replicates_results.rds")
    prdc <- rplc %>%
      purrr::map(.f = function(x){raster::predict(clim_layer,
                                                  x,
                                                  type = "cloglog")})
    prdc %>%
      purrr::map(.f = function(x){
        raster::writeRaster(x, paste0("./sdm_replicate_"))})
    
    fnlr <- raster::calc(raster::stack(prdc), fun = function(x){median(x, na.rm = T)})
    plot(fnlr)
  }
  
}


system.time(expr = {
  
  p <- spData[,1]
  data <- spData[,4:ncol(spData)]
  fit.maxent.1 <- maxnet::maxnet(p = p,
                                 data = data,
                                 regmult = beta,
                                 maxnet.formula(p, data, classes = paste0(substr(feat, start = 1, stop = 1), collapse = "")))
  
})

system.time(expr = {
  prj.maxent.1 <- raster::predict(clim_layer, fit.maxent.1, type="cloglog")
})
plot(prj.maxent.1$layer)

library(raster)

raster::writeRaster(prj.maxent.1$layer, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/bicolor/sgh_custom/prj_models/bicolor_prj_maxnet_complete.tif")

system.time(expr = {
  
  p <- spData[,1]
  data <- spData[,5:ncol(spData)]
  fit.maxent.2 <- maxnet::maxnet(p = p,
                                 data = data,
                                 regmult = beta,
                                 maxnet.formula(p, data, classes = paste0(substr(feat, start = 1, stop = 1), collapse = "")))
  
})

system.time(expr = {
  prj.maxent.2 <- raster::predict(clim_layer[[-1]], fit.maxent.2, type="cloglog")
})
plot(prj.maxent.2$layer)
raster::writeRaster(prj.maxent.2$layer, "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/results/sorghum/lvl_1/bicolor/sgh_custom/prj_models/bicolor_prj_maxnet_complete_sin_accessibility.tif")

# Evaluate
pred.at.fitting.pres <- raster::extract(prj.maxent.1$layer, spData[which(spData$bicolor == 1), c("lon", "lat")]) # get predictions at pres locations
pred.at.fitting.bg   <- raster::extract(prj.maxent.1$layer, spData[which(spData$bicolor == 0), c("lon", "lat")]) # get predictions at background locations
rocr.pred            <- ROCR::prediction(predictions = c(pred.at.fitting.pres, pred.at.fitting.bg),
                           labels = c(rep(1,length(pred.at.fitting.pres)),rep(0,length(pred.at.fitting.bg)))) # define the prediction object needed by ROCR
perf.fit <- performance(rocr.pred, measure = "tpr", x.measure = "fpr") # calculate perfomance 
plot(perf.fit) # plot ROC curve
abline(0,1)

performance(rocr.pred, measure = c("acc"))
