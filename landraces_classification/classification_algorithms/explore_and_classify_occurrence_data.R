# Explore and classify occurrence data
# H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){ root <- "/mnt/workspace_cluster_9" } else {
  if(OSys == "Windows"){ root <- "//dapadfs/Workspace_cluster_9" }
}; rm(OSys)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(rgdal))
suppressMessages(library(sp))
suppressMessages(library(raster))
suppressMessages(library(ncdf4))
suppressMessages(library(rasterVis))
suppressMessages(library(htmlwidgets))
suppressMessages(library(compiler))
suppressMessages(library(leaflet))
suppressMessages(library(highcharter))
suppressMessages(library(plotly))
suppressMessages(library(d3heatmap))
suppressMessages(library(cluster))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(corrplot))
suppressMessages(library(Rtsne))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(modelr))
suppressMessages(library(broom))
suppressMessages(library(purrr))
suppressMessages(library(PCAmixdata))
suppressMessages(library(AppliedPredictiveModeling))
suppressMessages(library(googlesheets))

## =================================================================================================================== ##
## Blair's study
## =================================================================================================================== ##

# phenotypic_data <- read_excel(path = paste0(root, "/gap_analysis_landraces/Input_data/_phenotypic_data/Bean/Blair_et_al_2009_races_subgroups_phenotypic.xlsx"), sheet = 2)
# 
# par(mfrow = c(1,3))
# phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
# 
# par(mfrow = c(1,3))
# phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
# # phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot(is.corr = T, method = "ellipse", type = "upper")
# 
# par(mfrow = c(1,3))
# phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
# rm(phenotypic_data)

## =================================================================================================================== ##
## CIAT + USDA information with climate data (without phaseolin data and race/genepool classification)
## =================================================================================================================== ##

# PCA
# MFA
# Stepwise Discriminant analysis
# Canonical Discriminant analysis

# Fisher transformation para variables categóricas
# 1 componente: tamaño semilla muy util para discriminar
# 2. Habito crecimiento, dias a floracion
# 3. Color semilla, patron, brillo

# genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_climate.RDS"))
# genotypic_climate %>% glimpse
# genotypic_climate_cmplt <- genotypic_climate[complete.cases(genotypic_climate),]
# genotypic_climate_cmplt <- unique(genotypic_climate_cmplt); rownames(genotypic_climate_cmplt) <- 1:nrow(genotypic_climate_cmplt)
# 
# # Descriptive analysis: quantitative variables
# genotypic_climate_cmplt %>% dplyr::select(Elevation, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
#   gather(Variable, Value) %>% ggplot(aes(x = Value, fill = Variable, alpha = .6)) +
#   geom_histogram() +
#   facet_wrap(~ Variable, scales = "free") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   theme(legend.title = element_text(face = "bold")) +
#   guides(alpha = F, fill = F) +
#   theme(strip.text = element_text(size = 12, face = "bold")) +
#   theme(axis.title.x = element_text(size = 13, face = 'bold'),
#         axis.title.y = element_text(size = 13, face = 'bold'),
#         axis.text = element_text(size = 12))
# genotypic_climate_cmplt %>% dplyr::select(Elevation, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
#   psych::describe() %>% select(mean, sd, median, min, max, range) # %>% mutate(cv = sd/mean * 100)
# 
# # Descriptive analysis: qualitative variables
# fqTable <- genotypic_climate_cmplt %>% dplyr::select(Growth.habit, Seed.color, Seed.shape, Seed.brightness, Owner) %>%
#   gather(measure, value) %>%
#   count(measure, value) %>%
#   spread(measure, n) %>%
#   gather(key = Variable, value = Count, Growth.habit:Seed.shape)
# fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1] <- "Category"
# fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(genotypic_climate_cmplt))
# 
# fqTable %>% ggplot(aes(x =  reorder(Category, Percentage), y = Percentage*100)) +
#   geom_bar(stat = "identity") +
#   xlab("") + ylab("Percentage (%)") +
#   coord_flip() +
#   facet_wrap(~ Variable, scales = "free") +
#   theme_bw() +
#   theme(strip.text = element_text(size = 12, face = "bold")) +
#   theme(axis.title.x = element_text(size = 13, face = 'bold'),
#         axis.title.y = element_text(size = 13, face = 'bold'),
#         axis.text = element_text(size = 12))
# 
# # Identify multivariate outliers
# 
# 
# # Calculate Mahalanobis Distance with height and weight distributions
# m_dist <- mahalanobis(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19),
#                       colMeans(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19)),
#                       cov(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19)))
# 
# pca.res <- genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19) %>%
#   FactoMineR::PCA(X = ., scale.unit = T, graph = T)
# 
# test <- data.frame(pca.res$ind$coord[,1:2], m_dist)
# test %>% ggplot(aes(x = Dim.1, y = Dim.2, col = m_dist)) + geom_point()
# test <- data.frame(genotypic_climate_cmplt[,c("Longitude", "Latitude")], m_dist)
# test %>% ggplot(aes(x = Longitude, y = Latitude, col = m_dist)) + geom_point()
# 
# highchart() %>% 
#   hc_title(text = "Scatter chart with number of coordinates and median harvested area by country") %>% 
#   hc_add_series_scatter(x = test$Longitude, y = test$Latitude, color = test$m_dist, label = genotypic_climate_cmplt$Owner)
# 
# # Mahalanobis Outliers - Threshold set to 12
# df$outlier_maha <- "No"
# df$outlier_maha[df$m_dist > 12] <- "Yes"
# 
# 
# 
# # Factor Analysis of Mixed Data
# set.seed(1)
# famd_res <- FactoMineR::FAMD(base = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18), graph = T)
# fviz_screeplot(famd_res) + theme_bw()
# fviz_famd_var(famd_res) + theme_bw()
# fviz_famd_var(famd_res, "quanti.var", repel = TRUE, col.var = "black") + theme_bw()
# fviz_famd_var(famd_res, "quali.var", col.var = "black")
# fviz_famd_ind(famd_res, col.ind = "cos2",
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)
# 
# # Multiple Factor Analysis
# set.seed(1)
# mfa_res <- FactoMineR::MFA(base = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18), group = c(2, 4, 9), type = c("s", "n", "s"), graph = T)
# 
# # Random forest clustering
# no.forests <- 25
# no.trees <- 3000
# set.seed(1)
# distRF <- RFdist(datRF = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18) %>% unique(),
#                  mtry1 = 3, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
# 
# ### Clustering methods
# 
# ## Gower distance
# 
# set.seed(1)
# gower.dist <- daisy(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18),
#                     metric = "gower", type = list(logratio = 3))
# summary(gower.dist)
# gower.mat <- as.matrix(gower.dist)
# 
# fviz_dist(as.dist(gower.mat),
#           gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# 
# res.hc <- hclust(d = gower.dist, method = "ward.D2")
# fviz_dend(res.hc, cex = 0.5)
# 
# fviz_dend(res.hc, k = 3, cex = 0.4, horiz = TRUE, k_colors = "jco",
#           rect = TRUE, rect_border = "jco", rect_fill = TRUE)
# 
# library(ClustOfVar)
# stability(tree = res.hc, B = 40, graph = T)
# 
# 
# 
# 
# 
# 
# fviz_nbclust(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18),
#              kmeans, method = "gap_stat") + geom_vline(xintercept = 4, linetype = 2) + 
#   labs(subtitle = "Elbow method")
# 
# 
# # Fuzzy clustering
# 
# # T-SNE
# Labels <- train$label
# train$label <- as.factor(train$label)
# ## for plotting
# colors = rainbow(length(unique(train$label)))
# names(colors) = unique(train$label)
# 
# ## Executing the algorithm on curated data
# set.seed(1)
# tsne <- Rtsne(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18) %>% unique(),
#               dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
# 
# ## Plotting
# plot(tsne$Y, main = "tsne", pch = 20)
# text(tsne$Y, labels=train$label, col=colors[train$label])


# Stepwise Discriminant analysis

# Canonical Discriminant analysis


## =================================================================================================================== ##
## CIAT information with climate data (with phaseolin data and race/genepool classification)
## =================================================================================================================== ##

# genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/ciatOrganizedVariables_climate.RDS"))
genotypic_climate <- read.csv("D:/ciat_beans_filtered_with_climate.csv")
genotypic_climate$Genepool.lit <- as.character(genotypic_climate$Genepool.lit)
genotypic_climate$Genepool.lit[grep(pattern = "Andean_Spain_I", x = genotypic_climate$Genepool.lit)] <- "Andean"
genotypic_climate$Genepool.protein[genotypic_climate$Genepool.protein == "N/A"] <- NA
genotypic_climate$Accession.number <- as.factor(genotypic_climate$Accession.number)
genotypic_climate$Common.names <- as.factor(genotypic_climate$Common.names)
genotypic_climate$Interpreted.name.lit <- as.factor(genotypic_climate$Interpreted.name.lit)
genotypic_climate$Vernacular.name.lit <- as.factor(genotypic_climate$Vernacular.name.lit)
genotypic_climate$Genepool.lit <- as.factor(genotypic_climate$Genepool.lit)
genotypic_climate$Race.interpreted.lit <- as.factor(genotypic_climate$Race.interpreted.lit)
genotypic_climate$Subgroup.lit <- as.factor(genotypic_climate$Subgroup.lit)
genotypic_climate$Growth.habit <- as.factor(genotypic_climate$Growth.habit)
genotypic_climate$Seed.shape <- as.factor(genotypic_climate$Seed.shape)
genotypic_climate$Seed.brightness <- as.factor(genotypic_climate$Seed.brightness)
genotypic_climate$Genepool.protein <- as.factor(genotypic_climate$Genepool.protein)
genotypic_climate %>% glimpse

genotypic_climate <- genotypic_climate %>% filter(Analysis == "Americas")

# ==================================== #
# Univariate descriptive analysis
# ==================================== #

# Quantitative variables: histograms
gg <- genotypic_climate %>% dplyr::select(Altitude, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  gather(Variable, Value) %>% ggplot(aes(x = Value, alpha = .6)) + # fill = Variable
  geom_histogram() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(face = "bold")) +
  guides(alpha = F, fill = F) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/histograms_quantitative_variables.png"))){
  ggsave(paste0(root, "/gap_analysis_landraces/Results/histograms_quantitative_variables.png"), plot = gg, width = 22, height = 10, units = "in"); rm(gg)
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
pca.mix_res <- PCAmix(X.quanti = genotypic_climate[complete.cases(genotypic_climate),] %>% select(Altitude:Latitude, Seed.weight, Color_Black:bio_19) %>% Filter(function(x) sd(x) != 0, .) %>% as.data.frame,
                      X.quali = genotypic_climate[complete.cases(genotypic_climate),] %>% select(Genepool:Subgroup, Growth.habit:Seed.brightness, Race.protein) %>% as.data.frame, graph = T, rename.level=TRUE)
pca.mix_res %>% summary

# Cluster analysis
pca.mix_res$scores[,1:2] %>% plot

View(pca.mix_res$eig)

View(pca.mix_res$quanti$cos2)


library("cluster")
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = pca.mix_res$scores, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward") # Linkage method
factoextra::fviz_dend(res.agnes, cex = 0.6)

# Divisive Analysis Clustering
res.diana <- diana(x = pca.mix_res$scores, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean") # metric for distance matrix
factoextra::fviz_dend(res.diana, cex = 0.6)

res.diana$height %>% sort %>% barplot

# ==================================== #
# Classification algorithms
# ==================================== #

toPredict <- genotypic_climate %>% dplyr::select(ID, Longitude, Latitude, Genepool.lit, Altitude, Latitude, Growth.habit:bio_19)
toPredict <- toPredict %>% filter(is.na(Genepool.lit))
toPredict <- toPredict[which(complete.cases(toPredict[,-which(names(toPredict)=="Genepool.lit")])),]; rownames(toPredict) <- 1:nrow(toPredict)

## Genepool as response variable
genepool_data <- genotypic_climate %>% dplyr::select(ID, Longitude, Latitude, Genepool.lit, Altitude, Latitude, Growth.habit:bio_19)
genepool_data <- genepool_data[complete.cases(genepool_data),]; rownames(genepool_data) <- 1:nrow(genepool_data)
genepool_data$Genepool.lit <- factor(genepool_data$Genepool.lit)
genepool_data$Growth.habit <- factor(genepool_data$Growth.habit)
genepool_data$Seed.shape <- factor(genepool_data$Seed.shape)
genepool_data$Seed.brightness <- factor(genepool_data$Seed.brightness)
genepool_data$Genepool.protein <- factor(genepool_data$Genepool.protein)

# Descriptive analysis: quantitative
gg <- genepool_data %>% dplyr::select(Genepool.lit, Latitude, Altitude, Seed.weight, bio_1:bio_19) %>%
  gather(Variable, Value, -Genepool.lit) %>% ggplot(aes(x = Value, colour = Genepool.lit, alpha = .6)) + # fill = Variable
  geom_density() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(face = "bold")) +
  guides(alpha = F, fill = F) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))

# Descriptive analysis: qualitative
fqTable <- genepool_data %>% dplyr::select(Genepool, Growth.habit, Seed.shape, Seed.brightness, Race.protein) %>%
  gather(measure, value, -Genepool) %>%
  count(Genepool, measure, value) %>%
  spread(measure, n) %>%
  gather(key = Variable, value = Count, Growth.habit:Seed.shape)
fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[2] <- "Category"
fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(genotypic_climate))
fqTable <- fqTable %>% as.data.frame
# Color variable
fqTable <- rbind(fqTable, data.frame(Category = genepool_data[,c(1, grep(pattern = "^Color_", x = names(genepool_data)))] %>% as.data.frame %>% group_by(Genepool) %>% dplyr::do(color = base::colSums(.)) %>% names %>% gsub(pattern = "Color_", replacement = "", x = .),
                                     Variable = "Color",
                                     Count = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Color_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))
# Protein variable
fqTable <- rbind(fqTable, data.frame(Category = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% names %>% gsub(pattern = "^Protein_", replacement = "", x = .),
                                     Variable = "Protein",
                                     Count = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric,
                                     Percentage = genotypic_climate[,grep(pattern = "^Protein_", x = names(genotypic_climate))] %>% as.data.frame %>% apply(MARGIN = 2, FUN = sum) %>% as.numeric / nrow(genotypic_climate)))

# Qualitative variables: barplot per variable
gg <- fqTable %>% ggplot(aes(x =  reorder(Category, Percentage), y = Percentage*100, fill = Genepool)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Percentage (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))

transparentTheme(trans = .4)
featurePlot(x = genepool_data %>% select(Altitude, Latitude, Seed.weight), 
            y = genepool_data$Genepool,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

transparentTheme(trans = .4)
featurePlot(x = genepool_data %>% select(bio_1:bio_19), 
            y = genepool_data$Genepool,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

# Delete zero- and near zero-variance predictors
# nzv <- nearZeroVar(genepool_data, saveMetrics = TRUE)
# nzv[nzv$nzv,][1:10,]
nzv <- nearZeroVar(genepool_data)
filtered_genepool_data <- genepool_data[,-nzv]

# Random Forest
# set.seed(1)
# genepool_folds <- modelr::crossv_kfold(filtered_genepool_data, k = 5)
# genepool_folds <- genepool_folds %>% mutate(model = map(train, ~ randomForest(Genepool ~ ., data = .)))
# 
# varImpPlot(x = genepool_folds$model$`1`)
# varImpPlot(x = genepool_folds$model$`2`)
# varImpPlot(x = genepool_folds$model$`3`)
# varImpPlot(x = genepool_folds$model$`4`)
# varImpPlot(x = genepool_folds$model$`5`)
# 
# CM = table(genepool_folds$test$`2`$data$Genepool, predict(genepool_folds$model$`2`, genepool_folds$test$`2`$data[,-ncol(genepool_folds$test$`2`$data)]))

set.seed(825)
ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 5, savePredictions = T)
grid <- expand.grid(mtry = round((ncol(filtered_genepool_data)-4)/3))
rfFit <- train(Genepool.lit ~ ., data = filtered_genepool_data[,3:ncol(filtered_genepool_data)], 
               method = "rf",
               tuneGrid = grid,
               importance = TRUE,
               ntree = 2000,
               trControl = ctrol2
               )
rfFit$finalModel$confusion
used.variables <- names(filtered_genepool_data[,3:ncol(filtered_genepool_data)])
used.variables <- used.variables[-which(used.variables == "Genepool.lit")]

toPredict[,used.variables] %>% dim
toPredict[,used.variables][complete.cases(toPredict[,used.variables]),] %>% dim

toPredict$Genepool.predicted.rf <- predict(object = rfFit, newdata = toPredict[,used.variables])

set.seed(825)
svmFit <- train(Genepool.lit ~ ., data = filtered_genepool_data[,3:ncol(filtered_genepool_data)], 
                method = "svmRadial", # Radial kernel
                tuneLength = 9,       # 9 values of the cost function
                trControl = ctrol2,
                importance = T
                )
mean(svmFit$results$Accuracy)
toPredict$Genepool.predicted.svm <- predict(object = svmFit, newdata = toPredict[,used.variables])

write.csv(x = toPredict, "D:/predicted_coordinates_genepool.csv", row.names = F)

chrys_data <- read.csv("D:/ciat_beans_filtered_with_climate_reference.csv")
complete.data <- left_join(x = chrys_data, y = toPredict %>% dplyr::select(ID, Genepool.predicted.rf, Genepool.predicted.svm), by = "ID")
write.csv(x = complete.data, "D:/predicted_coordinates_genepool_rf_svm.csv", row.names = F)

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = complete.data[!is.na(complete.data$Race.interpreted.lit),], aes(x = Longitude, y = Latitude, fill = Race.interpreted.lit, colour = Race.interpreted.lit)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

importanceRF <- rfFit$finalModel$importance %>% as.data.frame()
importanceSVM <- varImp(svmFit)
importanceSVM <- importanceSVM$importance
importanceRF$Variable <- rownames(importanceRF); rownames(importanceRF) <- 1:nrow(importanceRF)
importanceSVM$Variable <- rownames(importanceSVM); rownames(importanceSVM) <- 1:nrow(importanceSVM)
importanceRF <- importanceRF %>% select(Variable, MeanDecreaseGini)
importanceSVM <- importanceSVM %>% select(Variable, Mesoamerican)
names(importanceRF)[2] <- "Importance"
names(importanceSVM)[2] <- "Importance"
importanceRF$Importance <- (importanceRF$Importance - min(importanceRF$Importance))/(max(importanceRF$Importance)-min(importanceRF$Importance))
importanceSVM$Importance <- (importanceSVM$Importance - min(importanceSVM$Importance))/(max(importanceSVM$Importance)-min(importanceSVM$Importance))
importanceRF$Model <- "RandomForest"
importanceSVM$Model <- "SVM"
generalImportance <- rbind(importanceRF, importanceSVM)

shp_wld <- rgdal::readOGR(dsn = "D:/Harold/_maps/ShapeFiles/world", layer = "all_countries")
proj4string(shp_wld) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
shp_wld$CONTINENT <- iconv(shp_wld$CONTINENT, from = "UTF-8", to = "latin1")
shp_wld <- shp_wld[shp_wld@data$CONTINENT == "North America" | shp_wld@data$CONTINENT == "South America",]

shp_wld <- fortify(shp_wld)

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict, aes(x = Longitude, y = Latitude, fill = Genepool.predicted.rf, colour = Genepool.predicted.rf)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict, aes(x = Longitude, y = Latitude, fill = Genepool.predicted.svm, colour = Genepool.predicted.svm)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

toPredict %>% ggplot(aes(x = Longitude, y = Latitude, fill = Genepool.predicted.rf, colour = Genepool.predicted.rf)) + geom_point()
toPredict %>% ggplot(aes(x = Longitude, y = Latitude, fill = Genepool.predicted.svm, colour = Genepool.predicted.svm)) + geom_point()

generalImportance %>% ggplot(aes(x = reorder(Variable, Importance), y = Importance, fill = Model)) +
  geom_bar(stat = "identity") + facet_wrap(~Model) + coord_flip() + theme_bw()



genepool_folds <- genepool_folds %>% mutate(model_svm = map(train, ~ svm(Genepool ~ ., kernel = "radial", scale = F, data = .)))
genepool_folds$model_svm$`1`
summary(genepool_folds$model_svm$`1`)

CM = table(genepool_folds$test$`2`$data$Genepool, predict(genepool_folds$model_svm$`2`, genepool_folds$test$`2`$data[,-ncol(genepool_folds$test$`2`$data)]))

# Andres Cortes calcular drought arity

genepool_data %>% ggplot(aes(x = Genepool, y = Latitude)) + geom_boxplot()
genepool_data %>% ggplot(aes(x = Genepool, y = bio_15)) + geom_boxplot()
genepool_data %>% ggplot(aes(x = Genepool, fill = factor(Race.protein))) + geom_bar()
genepool_data %>% ggplot(aes(x = Genepool, y = Seed.weight)) + geom_boxplot()

## Race as response variable

toPredict.race <- genotypic_climate %>% dplyr::select(ID, Longitude, Latitude, Race.interpreted.lit, Altitude, Latitude, Growth.habit:bio_19)
toPredict.race <- toPredict.race %>% filter(is.na(Race.interpreted.lit))
toPredict.race <- toPredict.race[which(complete.cases(toPredict.race[,-which(names(toPredict.race)=="Race.interpreted.lit")])),]; rownames(toPredict.race) <- 1:nrow(toPredict.race)

race_data <- genotypic_climate %>% dplyr::select(ID, Longitude, Latitude, Race.interpreted.lit, Altitude, Latitude, Growth.habit:bio_19)
race_data <- race_data[complete.cases(race_data),]; rownames(race_data) <- 1:nrow(race_data)
race_data$Race.interpreted.lit <- as.character(race_data$Race.interpreted.lit)
race_data$Race.interpreted.lit <- factor(race_data$Race.interpreted.lit)
race_data$Growth.habit <- factor(race_data$Growth.habit)
race_data$Seed.shape <- factor(race_data$Seed.shape)
race_data$Seed.brightness <- factor(race_data$Seed.brightness)
race_data$Genepool.protein <- factor(race_data$Genepool.protein)

nzv <- nearZeroVar(race_data)
filtered_race_data <- race_data[,-nzv]

set.seed(825)
ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 5, savePredictions = T)
grid <- expand.grid(mtry = round((ncol(filtered_race_data)-4)/3))
rfFit <- train(Race.interpreted.lit ~ ., data = filtered_race_data[,3:ncol(filtered_race_data)], 
               method = "rf",
               tuneGrid = grid,
               importance = TRUE,
               ntree = 2000,
               trControl = ctrol2
)
used.variables <- names(filtered_race_data[,3:ncol(filtered_race_data)])
used.variables <- used.variables[-which(used.variables == "Race.interpreted.lit")]

toPredict.race[,used.variables] %>% dim
toPredict.race[,used.variables][complete.cases(toPredict.race[,used.variables]),] %>% dim

toPredict.race$Race.predicted.rf <- predict(object = rfFit, newdata = toPredict.race[,used.variables])

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict.race, aes(x = Longitude, y = Latitude, fill = Race.predicted.rf, colour = Race.predicted.rf)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

set.seed(825)
svmFit <- train(Race.interpreted.lit ~ ., data = filtered_race_data[,3:ncol(filtered_race_data)], 
                method = "svmRadial", # Radial kernel
                tuneLength = 9,       # 9 values of the cost function
                trControl = ctrol2,
                importance = T
)
mean(svmFit$results$Accuracy)
toPredict.race$Race.predicted.svm <- predict(object = svmFit, newdata = toPredict.race[,used.variables])

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict.race, aes(x = Longitude, y = Latitude, fill = Race.predicted.svm, colour = Race.predicted.svm)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

importanceRF <- rfFit$finalModel$importance %>% as.data.frame()
importanceSVM <- varImp(svmFit)
importanceSVM <- importanceSVM$importance
importanceRF$Variable <- rownames(importanceRF); rownames(importanceRF) <- 1:nrow(importanceRF)
importanceSVM$Variable <- rownames(importanceSVM); rownames(importanceSVM) <- 1:nrow(importanceSVM)
importanceRF <- importanceRF %>% select(Variable, MeanDecreaseGini)
importanceSVM <- importanceSVM %>% select(Variable, Mesoamerican)
names(importanceRF)[2] <- "Importance"
names(importanceSVM)[2] <- "Importance"
importanceRF$Importance <- (importanceRF$Importance - min(importanceRF$Importance))/(max(importanceRF$Importance)-min(importanceRF$Importance))
importanceSVM$Importance <- (importanceSVM$Importance - min(importanceSVM$Importance))/(max(importanceSVM$Importance)-min(importanceSVM$Importance))
importanceRF$Model <- "RandomForest"
importanceSVM$Model <- "SVM"
generalImportance <- rbind(importanceRF, importanceSVM)

importanceRF %>% ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") + coord_flip() + theme_bw()

chrys_data <- read.csv("D:/ciat_beans_filtered_with_climate_reference.csv")
complete.data.race <- left_join(x = chrys_data, y = toPredict.race %>% dplyr::select(ID, Race.predicted.rf, Race.predicted.svm), by = "ID")
write.csv(x = complete.data.race, "D:/predicted_coordinates_race_rf_svm.csv", row.names = F)

# Filtered just for origin zones
filtered_race_data_origin <- filtered_race_data %>% filter(Longitude < -61.5 & Latitude < 30)
toPredict.race.origin <- genotypic_climate %>% dplyr::select(ID, Longitude, Latitude, Race.interpreted.lit, Altitude, Latitude, Growth.habit:bio_19)
toPredict.race.origin <- toPredict.race.origin %>% filter(is.na(Race.interpreted.lit))
toPredict.race.origin <- toPredict.race.origin[which(complete.cases(toPredict.race.origin[,-which(names(toPredict.race.origin)=="Race.interpreted.lit")])),]; rownames(toPredict.race.origin) <- 1:nrow(toPredict.race.origin)
toPredict.race.origin <- toPredict.race.origin %>% filter(Longitude < -61.5 & Latitude < 30)

set.seed(825)
ctrol2 <- trainControl(method = "LGOCV", p = 0.8, number = 5, savePredictions = T)
grid <- expand.grid(mtry = round((ncol(filtered_race_data_origin)-4)/3))
rfFit <- train(Race.interpreted.lit ~ ., data = filtered_race_data_origin[,3:ncol(filtered_race_data_origin)], 
               method = "rf",
               tuneGrid = grid,
               importance = TRUE,
               ntree = 2000,
               trControl = ctrol2
)
rfFit$results
used.variables <- names(filtered_race_data_origin[,3:ncol(filtered_race_data_origin)])
used.variables <- used.variables[-which(used.variables == "Race.interpreted.lit")]

toPredict.race.origin[,used.variables] %>% dim
toPredict.race.origin[,used.variables][complete.cases(toPredict.race.origin[,used.variables]),] %>% dim

toPredict.race.origin$Race.predicted.rf <- predict(object = rfFit, newdata = toPredict.race.origin[,used.variables])

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict.race.origin, aes(x = Longitude, y = Latitude, fill = Race.predicted.rf, colour = Race.predicted.rf)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

set.seed(825)
svmFit <- train(Race.interpreted.lit ~ ., data = filtered_race_data_origin[,3:ncol(filtered_race_data_origin)], 
                method = "svmRadial", # Radial kernel
                tuneLength = 9,       # 9 values of the cost function
                trControl = ctrol2,
                importance = T
)
mean(svmFit$results$Accuracy)
toPredict.race.origin$Race.predicted.svm <- predict(object = svmFit, newdata = toPredict.race.origin[,used.variables])

ggplot() + 
  geom_polygon(data = shp_wld, aes(long, lat, group = group)) +
  geom_point(data = toPredict.race.origin, aes(x = Longitude, y = Latitude, fill = Race.predicted.svm, colour = Race.predicted.svm)) +
  coord_cartesian(xlim = c(-180, 0)) + theme_bw()

importanceRF <- rfFit$finalModel$importance %>% as.data.frame()
importanceSVM <- varImp(svmFit)
importanceSVM <- importanceSVM$importance
importanceRF$Variable <- rownames(importanceRF); rownames(importanceRF) <- 1:nrow(importanceRF)
importanceSVM$Variable <- rownames(importanceSVM); rownames(importanceSVM) <- 1:nrow(importanceSVM)
importanceRF <- importanceRF %>% select(Variable, MeanDecreaseGini)
importanceSVM <- importanceSVM %>% select(Variable, Mesoamerican)
names(importanceRF)[2] <- "Importance"
names(importanceSVM)[2] <- "Importance"
importanceRF$Importance <- (importanceRF$Importance - min(importanceRF$Importance))/(max(importanceRF$Importance)-min(importanceRF$Importance))
importanceSVM$Importance <- (importanceSVM$Importance - min(importanceSVM$Importance))/(max(importanceSVM$Importance)-min(importanceSVM$Importance))
importanceRF$Model <- "RandomForest"
importanceSVM$Model <- "SVM"
generalImportance <- rbind(importanceRF, importanceSVM)

importanceRF %>% ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") + coord_flip() + theme_bw()

chrys_data <- read.csv("D:/ciat_beans_filtered_with_climate_reference.csv")
complete.data.race <- left_join(x = chrys_data, y = toPredict.race %>% dplyr::select(ID, Race.predicted.rf, Race.predicted.svm), by = "ID")
write.csv(x = complete.data.race, "D:/predicted_coordinates_race_rf_svm.csv", row.names = F)









# Random Forest
set.seed(1)
race_folds <- modelr::crossv_kfold(race_data, k = 5)
race_folds <- race_folds %>% mutate(model = map(train, ~ randomForest(Race.interpreted ~ ., data = .)))

varImpPlot(x = race_folds$model$`1`)
varImpPlot(x = race_folds$model$`2`)

## Subgroup as response variable
subgroup_data <- genotypic_climate %>% dplyr::select(Subgroup, Altitude, Latitude:bio_19)
subgroup_data <- subgroup_data[complete.cases(subgroup_data),]; rownames(subgroup_data) <- 1:nrow(subgroup_data)
subgroup_data$Subgroup <- factor(subgroup_data$Subgroup)
subgroup_data$Growth.habit <- factor(subgroup_data$Growth.habit)
subgroup_data$Seed.shape <- factor(subgroup_data$Seed.shape)
subgroup_data$Seed.brightness <- factor(subgroup_data$Seed.brightness)

# Random Forest
set.seed(1)
subgroup_folds <- modelr::crossv_kfold(subgroup_data, k = 5)
subgroup_folds <- subgroup_folds %>% mutate(model = map(train, ~ randomForest(Subgroup ~ ., data = .)))

varImpPlot(x = subgroup_folds$model$`1`)
varImpPlot(x = subgroup_folds$model$`2`)
