
filepath <- paste0(results_dir, "/", crop, "/lvl_1/final_results_report")
generate_report <- function(filepath, class_name = "five_bio_socio.rds", level_1, region, is.everything.finished = FALSE){
  
  if(is.everything.finished){
    pacman::p_load(rmarkdown, knitr, kableExtra)
    
    cat("Generating HTML report... /n")
    
sink(paste0(filepath,".R"))

cat("#' ---
#' title: \"Gap Analysis Results Report\" 
#' author: \"Andres Camilo Mendez, Harold Achicanoy, Maria Victoria, Julian ramirez and Colin Khoury.\" 
#' ---  "  )
cat("\n \n")
cat("#'  
{{ paste(\"# Crop: \", crop) }} 
#' 
#' Here you can check  for each specie, race, sub-race, etc. The main results from the gap analysis methodology that we've proposed.
#' 
#' Let's star with the descriptive analysis carried out by an ensemble model between statistical and machine learning models, 
#' as well as  PCA(Principal Components Analysis). 
#' 
#' * ## Model ensemble summary 
#'     + #### Confusion Matrix: 
#' \n" )

cat("#+   cache=FALSE, echo = FALSE
class_res <- readRDS(paste0(input_data_dir, \"/by_crop/\", crop, \"/lvl_1/classification/\", class_name )) 
\n
kable(class_res$Testing_CM$ensemble$table) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F) " 
    )
cat("\n")
cat("#' *
#'     + #### Main performance measures: 
#+      echo = FALSE 
rn <- rownames(class_res$Testing_CM$ensemble$byClass[,c(1,2,5,7,11)]) 
kable(data.frame(class = rn, round(as_tibble(class_res$Testing_CM$ensemble$byClass[,c(1,2,5,7,11)]), 3)) )  %>%  
kable_styling(c(\"striped\", \"bordered\"), full_width = F) " 
    )
cat("\n")

cat("#' * 
#'     + #### Five most important variables by model: 
#+ echo = FALSE \n
kable(class_res$Important_variables) %>% kable_styling(c(\"striped\", \"bordered\"), full_width = F)  " 
    )
cat("\n")
cat(
"#'  * ## Principal Component Analysis (PCA) 
#+ echo = FALSE 
![mi image](paste0(input_data_dir, \"/by_crop/\", crop, \"/lvl_1/classification/pca_plot.jpg\"  )) " 
)
cat("\n")
cat("
#' * 
#'     + #### Variable contributions 
#+ 
# poner una tabla con la contribucion de cada variable \n"
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

#' * ### Environmental distance
#'   The environmental distance raster is a ...   
  
#' * ### Spatial specie distribution(SDM)    
#'   The SDM represents the main input for the gaps scores calculation and is calculated through maxent model (for 
#'   more information visit [this website](https://biodiversityinformatics.amnh.org/open_source/maxent/)), this raster represents a
#'   predicted suitability of conditions for the specie expressed as a probability distribution which we're considering  as the 
#'   probability of find a landrace.  
#'   
    ")

for(i in level_1){
cat(paste("
#' #  Results for:", i, "\n" 
)  )
cat("\n")
cat(paste("#' * ### Cost distance gap score: \n"))
cat(paste("#+ echo = FALSE \n "))
cat(paste('include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i,'", "/","', region, '","/gap_models/gg_gap_cost_dist.jpg"   ))' , sep = ""), "\n")
cat("\n")
cat(paste("#' * ### Delaunay gap score: \n"))
cat(paste("#+ echo = FALSE \n"))
cat(paste('include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/gap_models/gg_gap_delaunay.jpg"   )) ' , sep = ""), "\n")
cat("\n")
cat(paste("#' * ### Final gap score(Combination of cost distance and delaunays raster): \n"))
cat(paste("#+ echo = FALSE \n"))
cat(paste('include_graphics(paste0(baseDir, "/results/", crop, "/lvl_1/","', i, '","/","', region, '","/gap_models/gg_gap_final.jpg"   )) ', sep = "" ), "\n")
cat("\n")
cat("#' ***** \n")
  
}
sink()

rmarkdown::render(paste0(filepath, ".R"),  "html_document", clean = FALSE  )

cat("Process done. Final report generated at:" , filepath,".html  \n")
file.remove(paste0(filepath, ".R"))
pacman::p_unload(rmarkdown, knitr, kableExtra)

 }else{
    cat("Important: please, make sure everything is already finished... \n")
  }#end if
  
  

}#end function
