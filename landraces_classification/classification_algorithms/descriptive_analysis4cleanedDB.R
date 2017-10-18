# Descriptive analysis for cleaned data
# A. Mendez & H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")

# Load packages
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(ncdf4)){install.packages("ncdf4");library(ncdf4)}else{library(ncdf4)})
suppressMessages(if(!require(rasterVis)){install.packages("rasterVis");library(rasterVis)}else{library(rasterVis)})
suppressMessages(if(!require(htmlwidgets)){install.packages("htmlwidgets");library(htmlwidgets)}else{library(htmlwidgets)})
suppressMessages(if(!require(compiler)){install.packages("compiler");library(compiler)}else{library(compiler)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(plotly)){install.packages("plotly");library(plotly)}else{library(plotly)})
suppressMessages(if(!require(d3heatmap)){install.packages("d3heatmap");library(d3heatmap)}else{library(d3heatmap)})
suppressMessages(if(!require(cluster)){install.packages("cluster");library(cluster)}else{library(cluster)})
suppressMessages(if(!require(FactoMineR)){install.packages("FactoMineR");library(FactoMineR)}else{library(FactoMineR)})
suppressMessages(if(!require(factoextra)){install.packages("factoextra");library(factoextra)}else{library(factoextra)})
suppressMessages(if(!require(Rtsne)){install.packages("Rtsne");library(Rtsne)}else{library(Rtsne)})
suppressMessages(if(!require(InformationValue)){install.packages("InformationValue");library(InformationValue)}else{library(InformationValue)})
suppressMessages(if(!require(corrplot)){install.packages("corrplot");library(corrplot)}else{library(corrplot)})
suppressMessages(if(!require(caTools)){install.packages("caTools");library(caTools)}else{library(caTools)})
suppressMessages(if(!require(caret)){install.packages("caret");library(caret)}else{library(caret)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(miniUI)){install.packages("miniUI");library(miniUI)}else{library(miniUI)})

# ==================================== #
# Univariate descriptive analysis
# ==================================== #

# Quantitative variables: histograms
test <- genotypic_climate %>% dplyr::select(Genepool,Altitude, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  gather(Variable, Value, -Genepool)
test <- test[complete.cases(test),] %>% as.data.frame

gg<-ggplot(data = test, aes(x = Value, alpha = .6, colour = factor(Genepool))) + # fill = Variable
  geom_density() +
  facet_wrap(~Variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(face = "bold")) +
  guides(alpha = F, fill = F) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/density_quantitative_variables_by_genepool.png"))){
  ggsave(paste0(root, "/gap_analysis_landraces/Results/density_quantitative_variables_by_genepool.png"), plot = gg, width = 22, height = 10, units = "in"); rm(gg)
}; rm(gg)




# Quantitative variables: descriptive statistics
genotypic_climate %>% dplyr::select(Altitude, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  psych::describe() %>% select(mean, sd, median, min, max, range) %>% as.data.frame %>%
  round(., digits = 2) %>% write.csv(., file = paste0(root, "/gap_analysis_landraces/Results/descriptiveStats_quantitative_variables.csv"), row.names = T)

# Qualitative variables: create a table of counts
fqTable <- genotypic_climate %>% dplyr::select(Vernacular.name, Genepool, Race.interpreted, Subgroup, Growth.habit, Seed.shape, Seed.brightness, Race.protein) %>%
  gather(measure, value) %>%
  count(measure, value) %>%
  spread(measure, n) %>%
  gather(key = Variable, value = Count, Genepool:Vernacular.name)
fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1] <- "Category"
fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(genotypic_climate))
fqTable <- fqTable %>% as.data.frame
# Color variable
fqTable <- rbind(fqTable, data.frame(Category = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% names %>% gsub(pattern = "Color_", replacement = "", x = .),
                                     Variable = "Color",
                                     Count = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))
# Protein variable
fqTable <- rbind(fqTable, data.frame(Category = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% names %>% gsub(pattern = "^Protein_", replacement = "", x = .),
                                     Variable = "Protein",
                                     Count = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))

# Qualitative variables: barplot per variable
gg <- fqTable %>% ggplot(aes(x =  reorder(Category, Percentage), y = Percentage*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Percentage (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/barplot_qualitative_variables.png"))){
  ggsave(paste0(root, "/gap_analysis_landraces/Results/barplot_qualitative_variables.png"), plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)
}; rm(gg)

# ==================================== #
# Multivariate descriptive analysis
# ==================================== #

# Principal Component Analysis for mixed data

# Cluster analysis
