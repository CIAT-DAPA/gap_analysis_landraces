#### ANDRES CAMILO MENDEZ
#### function to calculate the summary for the gap validation process


summary_function <- function(area, group, crop, lvl, pnt, filename, radius,baseDir, dens.level, ncores){
 
  cat("Importing packages...", "\n")

  cat(
    "        oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N`                 `..`               `.....`        `..`         `...              `N       
    N`                 omMh-            -sdddmddd/      `oMNd/       .sNNd.             `N       
    N`                /mooms.         `omh:`   .-.      `sNyhd:     `omsdm-             `N       
    N`               -dy.`sN+`        /mh-              `sNo:dh-   `+mo-dm-             `N       
    N`              `yd-  `hm:        +ms`              `sN+ /mh. `/ms`-dm-             `N       
    N`              sNmddddmMh-       /my-              `sN+ `+Ns`:dy` -dm-             `N       
    N`             /my:-----sNs.      `sNy:`   `-.      `sN+  .sNhdh.  -dm-             `N       
    N`            -dd-      `yN+`      `/yddddddh/      `om+   .yMd-   .dd.             `N       
    N`            `.`        `..          `.....`        `.`    `..     ..              `N       
    N`                                                                                  `N       
    N`                                                                                  `N       
    N++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++N       
    \n \n")

#CALCULATE ALL METRICS FOR GAP_SCORES IN EACH PNT
cat(">>>Calculating performance measure for all radius \n \n ")
  
apply(expand.grid(pnt, 1:2), 1, function(x){
  cat("********* Processing pnt:", x[1], "\n")
l <- as.numeric(x[2])
    if(!file.exists(paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x[1], "/03_gap_models/validation_metrics_",substr(filename[ l ], 11, 14),"_all_radius.rds") )){
      
    cat("             Calculating metrics for:", filename[l], "\n" )
  cl <- makeSOCKcluster(ncores)
  registerDoSNOW(cl)
  #on.exit(stopCluster(cl))
  pb <- tkProgressBar( max = length(radius))
  progress <- function(n) setTkProgressBar(pb, n)
  opts <- list(progress=progress)
  validation_results <- foreach( i = 1:length(radius), 
                                 .packages = c("raster", "pROC", "dplyr", "sdm"), 
                                 .options.snow=opts,  
                                 .export = c("validation_metrics", "area", "group", "crop", "lvl", "pnt", "filename", "radius","baseDir", "dens.level")) %dopar% {
    
    validation_metrics(n.sample = 200, 
                       bf_rad = radius[i], 
                       baseDir = baseDir,
                       area = area, 
                       group = group, 
                       crop = crop, 
                       lvl = "lvl_1", 
                       pnt = x[1], 
                       ncores = NULL, 
                       dens.level = dens.level ,
                       filename = filename[l]
                       )
   
    
  }
  stopCluster(cl)
  names(validation_results) <- paste0(radius, "km")
  
  saveRDS(validation_results, paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x[1], "/03_gap_models/validation_metrics_",substr(filename[l], 11, 14),"_all_radius.rds") )

  gc()
    }else{
      
      cat("              Metrics already calculated for:", filename[l], "\n" )
    }#end if 
 

})#end lapply

cat ("Making boxplots \n \n")
#### save plots
apply( expand.grid(pnt, 1:2), 1, function(x){
  l <- as.numeric(x[2])
   #cat(paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x[1], "/03_gap_models/validation_metrics_",substr(filename[ l ], 11, 14),"_all_radius.rds"),"\n")
    validation_results <- readRDS( paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x[1], "/03_gap_models/validation_metrics_",substr(filename[l], 11, 14),"_all_radius.rds") )
    all <-  validation_results %>% mapply(function(x, y){ add_column(x, radius = rep(factor(y), nrow(x)) ) }, x = ., y = names(.), SIMPLIFY = FALSE)  %>% do.call(rbind, .) %>%  as_tibble() 
    
    jpeg(paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x[1], "/03_gap_models/validation_metrics_",substr(filename[l], 11, 14),"_all_radius.jpg"),  width = 800, height = 600)
    plot(all$auc ~ all$radius, ylab = "AUC", xlab = " Buffer Radius (Km)", main = filename[l])
    dev.off()
    cat("    ", x[1], " Boxplot ", filename[ l ], " done... \n")
    
  
  
})


#*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+ TRESHOLDING PROCESS *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+**+*+*+*+*+*+
currRow <- 2
wb <- createWorkbook() #library(openxlsx)
sheet <- createSheet(wb,"summary")

#import rds files with all radius metrics and merging them to one single file
for(l in 1:2){

km_metrics <- lapply(pnt, function(x){ 
  cat( " Initializing Tresholding process for", x," ",  filename[l],"\n \n")
readRDS(paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/", dens.level, "/", x, "/03_gap_models/validation_metrics_",substr(filename[l], 11, 14),"_all_radius.rds")) %>%
    .[ which(names(.) %in% paste0(60:80, "km") )] %>% 
    mapply(function(x,y){ add_column(x, km = rep(factor(y), nrow(x)) ) }, 
           x = ., 
           y = names(.), 
           SIMPLIFY = FALSE) %>%
    do.call(rbind, .) %>%  as_tibble()

})

all_rs <-  km_metrics %>% 
  mapply(function(x,y){ add_column(x, pnt = rep(factor(y), nrow(x)) ) }, 
         x = ., 
         y = pnt,
         SIMPLIFY = FALSE)  %>% 
  do.call(rbind, .) %>%  as_tibble() 

score_mean <- function(x,li,ls, se, es){
#x %>%  dplyr:: filter(., auc >= li & auc <= ls ) %>% dplyr::select(., score) %>% mean(., na.rm = TRUE)
y <- mean(  x$score[which(x$auc >= li & x$auc <= ls)], na.rm = TRUE)
z <- mean(  x$se[which(x$auc >= li & x$auc <= ls)], na.rm = TRUE)
w <- mean(  x$es[which(x$auc >= li & x$auc <= ls)], na.rm = TRUE)
  return( c(y, z, w))
}

cat("     Calculating  summary metrics \n ")
#calculate summary measures for gap scores
summary_gap <- all_rs %>% dplyr::group_by(., pnt) %>% summarise(.,auc.median = round(median(auc, na.rm = T),3)
                                                           , auc.mean = round(mean(auc, na.rm = T), 3)
                                                           #, skewness = round(skew(auc), 3)
                                                           , auc.sd = round(sd(auc, na.rm = TRUE), 3)
                                                           , lower.ic = round(t.test(auc, conf.int = TRUE, conf.level = 0.95, na.rm = TRUE)$conf.int[1], 3)
                                                           , upper.ic = round(t.test(auc, conf.int = TRUE, conf.level = 0.95, na.rm = TRUE)$conf.int[2], 3)
                                                           , threshold = round(score_mean(x = data.frame(score, auc, se, es), li = lower.ic, ls = upper.ic)[1], 3)
                                                           , se.mean = round(score_mean(x = data.frame(score, auc, se, es), li = lower.ic, ls = upper.ic)[2],3)
                                                           , es.mean = round(score_mean(x = data.frame(score, auc, se, es), li = lower.ic, ls = upper.ic)[3] ,3)
                                                              ) 


means <- colMeans(summary_gap %>% dplyr::select(., -pnt), na.rm = TRUE)
summary_gap[nrow(summary_gap)+1,] <- c(as.factor("Mean"), means)

cat("     Writing  summary metrics in:,", paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/validation_results.xlsx") ," \n ")
#write a excel file with the results

cs <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("BOTTOM", "LEFT", "TOP", "RIGHT"))
addDataFrame(data.frame( x= filename[l]  ),
             sheet,
             row.names = FALSE,
             col.names = FALSE,
             startRow = currRow - 1,
             colnamesStyle = cs)

addDataFrame(as.data.frame(summary_gap), 
             sheet, 
             row.names = FALSE,
             startRow = currRow,
             colnamesStyle=cs)
currRow <- currRow + nrow(summary_gap) + 4 

 

}#END lapply
saveWorkbook(wb,file = paste0(baseDir, "/results/", crop,"/", lvl, "/", group ,"/", area, "/gap_validation/buffer_100km/validation_results.xlsx"))

cat( "PROCESS DONE \n")
}#END SUMMARY FUNCTION


# summary_function(area ="americas",
#                  group = "tuberosum",
#                  crop = "potato",
#                  lvl = "lvl_1",
#                  pnt = paste0("pnt", 1:5),
#                  filename = c("gap_score_cost_dist.tif"   ,"gap_score_delaunay.tif"),
#                  radius = seq(6,100, 1), #number of radius size to evaluate
#                  baseDir = baseDir,
#                  dens.level = "high_density",
#                  ncores = (detectCores()-8)
# )



