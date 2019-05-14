
filepath <- paste0(results_dir, "/", crop, "/lvl_1/final_results_report")
generate_report <- function(filepath, class_name = "common_bean_descriptive_results.rds", level_1, region, is.everything.finished = FALSE, disk = "Z:"){
  
  if(is.everything.finished){
    pacman::p_load(rmarkdown, knitr, kableExtra, xlsx)
    
    
    
    cat("Generating HTML report... /n")
    
sink(paste0(filepath,".R"))
  
cat("#' ---
#' title: \"Gap Analysis Results Report\" 
#' author: \"Andres Camilo Mendez, Harold Achicanoy, Maria Victoria, Julian ramirez and Colin Khoury.\" 
#' ---  "  )
cat("\n \n")
cat("#'  
#+  echo = FALSE
if(!is.null(disk)){
        baseDir <- paste0(disk, \"/gap_analysis_landraces/runs\")
}
#'
{{ paste(\"#  \", crop) }} 
#' 
#' Here you can check  for each specie, race, sub-race, etc. The main results from the gap analysis methodology that we've proposed.
#' 
#' Let's star with the descriptive analysis carried out by an ensemble model between statistical and machine learning models, 
#' as well as  PCA(Principal Components Analysis). 
#' 
#' * ## Accessions classification 
#'     + #### Testing Confusion Matrix: 
#' \n" )

cat("#+   cache=FALSE, echo = FALSE
class_res <- readRDS(paste0(input_data_dir, \"/by_crop/\", crop, \"/lvl_1/classification/\", class_name ))
kable(class_res$Testing_CM$ensemble$table) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F) %>% add_header_above(c(\" \" = 1, 'Observed' = ncol(class_res$Testing_CM$ensemble$table)), bold= TRUE ) %>% group_rows(\"Predicted\",1, length(level_1))" 
    )
cat("\n")
cat("#' *
#'    + #### Main performance measures: 
#+      echo = FALSE 
results <- class_res$Testing_CM$ensemble$byClass
n <- as.matrix(table(class_res$PCA_plot$data$specie))
if(is.null(nrow(results))){
lst1 <- lapply(class_res$Testing_CM, function(x){
x$byClass[c(1, 2)]
  })
sensi <- t(do.call(rbind, lst1))
row.names(sensi)<- level_1
sensi <- data.frame('n'= n, sensi)
kable(round(sensi, 3))  %>%  
  kable_styling(c(\"striped\", \"bordered\"), full_width = F) %>%  
    add_header_above(c(\" \" = 1, 'Sensitivity' = ncol(sensi)), bold= TRUE )

}else{
rn <- rownames(results[,c(1,2,5,7,11)]) 

lst1 <- lapply(class_res$Testing_CM, function(x){
x$byClass[, 1]
})
sensi <- t(do.call(rbind, lst1))

sensi <- data.frame('n'= n, sensi)

kable(round(sensi, 3) )  %>%  
  kable_styling(c(\"striped\", \"bordered\"), full_width = F) %>% 
    add_header_above(c(\" \" = 1, 'Sensitivity' = ncol(sensi)), bold= TRUE )
}

#'    + #### Matthews correlation coefficient  
#+      echo = FALSE
mcc <- class_res$Testing_MCC
mcc <- data.frame('Model' = names(mcc), 'MCC' = mcc )
row.names(mcc) <- NULL
kable(mcc) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F) 
 ")
cat("\n")
  
cat("#' * 
#'    + #### Five most important variables by model: 
#+ echo = FALSE
kable(class_res$Important_variables) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F)  " 
    )
cat("\n")
cat(
"#'  * ## Principal Component Analysis (PCA)
#+  echo = FALSE 
plot(class_res$PCA_plot)
" 
)
cat("\n")
cat("
#' * 
#'    + #### Variable contributions 
#+ echo = FALSE
kable(class_res$PCA$var$contrib) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F)
"
)
cat("\n")
cat("
#' ## Some aspects to bear in mind about the inputs files to calculate the gap map
#' * ### Cost distance raster 
#'     This input consider the accessibility to each occurrence expressed as a cost, taking in account the fact that the most accession
#'     were collected in places nearby to roads, this raster attempts to show us which places have a good accessibility but 
#'     are not being well represented in the accession collection. This raster is made up from:
#'     + **Friction surface**: a raster layer that represents the travel time (in Hours per kilometer) spent walking through a pixel, depending of the
#'     terrain type and some other variables (For more information visit [this website](http://forobs.jrc.ec.europa.eu/products/gam/description.php)).
#'     + **Occurrences**: Latitude and longitude of the occurrences in a Shapefile format.

#' * ### Delaunay triangulation raster
#'   This input is a network created from the Delaunay method (For more information 
#'   visit [this website](https://www.mathworks.com/help/matlab/math/delaunay-triangulation.html)),
#'   it consist in triangles linking the nearest three accessions, from each triangulations we can caculate
#'   an area, thus if the area is bigger i.e the accessions are far away from each other then it might means
#'   these area are not well represented in the collection. Moreover we have calculated others two measures
#'   for each triangulation and combine them with the area in order to get a better result. 

#' * ### Environmental raster
#'   to establish a measure of gaps from an environmental perspective the approach followed consists in measuring how well the 
#'   environment of the accessions are already represented in the genebanks and allows determining which environments are not well
#'   represented (environmental gaps). The Environmental score developed is based on the construction of environmental clusters in
#'   the spatial distribution of the landrace, and then calculate the environmental distance to closest accession for each 
#'   environmental cluster.  
    
#' * ### Spatial specie distribution(SDM)    
#'   The SDM represents the main input for the gaps scores calculation and is calculated through maxent model (for 
#'   more information visit [this website](https://biodiversityinformatics.amnh.org/open_source/maxent/)), this raster represents a
#'   predicted suitability of conditions for the specie expressed as a probability distribution which we're considering  as the 
#'   probability of find a landrace.  
#' \n")
  
cat("#+ echo = FALSE \n" )
cat("auc_avg <- c();high_conf_percent <- c();low_conf_percent <- c(); t_area <- c() ;k <- 1 \n")
cat("wmask <- raster::raster(mask) \n")
cat("rast_area <- raster::area(wmask) * wmask \n")

for(i in level_1){
cat(paste("\n#' #  Results for:", i, "\n" ) 
      )
  cat("\n")
  cat(paste("#' * ### Specie distribution model: \n"))
  cat(paste("#+ echo = FALSE \n "))
  cat(paste('include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i,'", "/","', region, '","/graphics/', i, "_",i,'_prj_median.png" ))' , sep = ""), "\n")
  cat("\n")
  cat(paste("#' * ### Cost distance gap score: \n"))
  cat(paste("#+ echo = FALSE \n "))
  cat(paste('include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i,'", "/","', region, '","/graphics/',i,'_gap_score_cost_dist.png" ))' , sep = ""), "\n")
  cat("\n")
  cat(paste("#' * ### Cost distance thresholded: \n"))
  cat(paste("#+ echo = FALSE \n"))
  cat(paste('knitr::include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/graphics/',i,'_gap_class_cost_dist.png" )) ' , sep = ""), "\n")
  cat("\n")
  cat(paste("#' * ### Delaunay gap score: \n"))
  cat(paste("#+ echo = FALSE \n"))
  cat(paste('knitr::include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/graphics/',i,'_gap_score_delaunay.png" )) ', sep = "" ), "\n")
  cat("\n")
  cat(paste("#' * ### Delaunay gap score thresholded: \n"))
  cat(paste("#+ echo = FALSE \n"))
  cat(paste('knitr::include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/graphics/',i,'_gap_class_delaunay.png" )) ', sep = "" ), "\n")
  cat("\n")
  cat(paste("#' * ### Final gap map (sum of both cost distance and delaunay gap scores thresholded): \n"))
  cat(paste("#+ echo = FALSE \n"))
  cat(paste('knitr::include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/graphics/',i,'_gap_class_final.png" )) ', sep = "" ), "\n")
  cat("\n")
  cat("#' * ### Validation results \n")
  cat("#+ echo =FALSE \n")
  cat(paste('res <- read.xlsx(paste0(baseDir, "/results/", crop, "/lvl_1/", "', i, '","/","', region, '","/gap_validation/buffer_100km/validation_results.xlsx"), sheetName = "summary")', sep = ""), "\n")
  cat("res_cost <- as.data.frame(res[1:7, ]);row.names(res_cost) <- NULL ;colnames(res_cost) <- as.character(unlist(res_cost[1, ])); res_cost <- res_cost[ -1, ]; res_cost[, 1] <- c(paste0('pnt', 1:5), 'mean')   \n")
  cat("res_dela <- as.data.frame(res[9:15,]);row.names(res_dela) <- NULL ;colnames(res_dela) <- as.character(unlist(res_dela[1, ])); res_dela <- res_dela[ -1, ]; res_dela[, 1] <- c(paste0('pnt', 1:5), 'mean') \n")
  cat("#' + #### Cost distance gap score validation results \n")
  cat("#+ echo = FALSE \n")
  cat("kable(res_cost) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F) %>% add_header_above(c(\" \" = 2, 'Cost distance' = 8), bold= TRUE ) \n")
  cat("#' + #### Delaunay gap score validation results \n")
  cat("#+ echo = FALSE \n")
  cat("kable(res_dela) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F) %>% add_header_above(c(\" \" = 2, 'Delaunay' = 8), bold= TRUE ) \n")
  cat("#+ echo = FALSE \n")
  cat(paste('auc_avg[k] <- 0.6*as.numeric(as.character(res_dela[nrow(res_dela), 3])) + 0.4*as.numeric(as.character(res_cost[nrow(res_cost), 3]))'), "\n")
  cat(paste('final_gap_rast <- raster(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/gap_models/gap_class_final.tif" )) ', sep = "" ), "\n")
  cat("sdm_mask <- raster::mask(wmask, final_gap_rast) \n")
  cat("sdm_area <- sdm_mask * rast_area
        total_sdm_area <- sum(sdm_area[], na.rm = TRUE)
        t_area[k] <- total_sdm_area

        high_conf <- final_gap_rast
        high_conf[which(high_conf[] != 2)] <- 1
        high_conf[which(high_conf[] == 2)] <- NA
        high_conf <- high_conf * rast_area
        total_hg_conf <- sum(high_conf[], na.rm = TRUE)
        high_conf_percent[k] <-  (total_hg_conf/total_sdm_area)*100
        

      
      
        low_conf <- final_gap_rast
        low_conf[which(low_conf[] != 0)] <- NA
        low_conf[which(low_conf[] == 0)] <- 1
        low_conf <- low_conf * rast_area
        total_lw_conf <- sum(low_conf[], na.rm = TRUE)
        
        low_conf_percent[k] <-  (total_lw_conf/total_sdm_area)*100
        k <- k+1
        \n ")
  cat("#' ***** \n")
    
}
cat("#' #  Summary table \n")
cat("#' This table contains a summary of... \n")
cat("#+ echo = FALSE \n")
cat("
    res <- class_res$Testing_CM$ensemble$byClass
    if(is.null(nrow(res))){
    avg_acc <- data.frame('average accuracy'= res[c(1, 2)] )
    row.names(avg_acc) <- level_1
    }else{
    avg_acc <- res[, 11]
    }
    
    summary_table <- data.frame('region' = region,'average accuracy' = avg_acc, 'High coverage area' =  high_conf_percent, 'Low coverage area' = low_conf_percent, 'Gap model performance' = auc_avg )
    kable(summary_table) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F)
    ")
sink()
  
rmarkdown::render(paste0(filepath, ".R"),  "html_document")

cat("Process done. Final report generated at:" , filepath,".html  \n")
file.remove(paste0(filepath, ".R"))
pacman::p_unload(rmarkdown, knitr, kableExtra, xlsx)

 }else{
    cat("Important: please, make sure everything is already finished... \n")
  }#end if
  
  

}#end function




