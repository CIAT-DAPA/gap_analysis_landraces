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

## =================================================================================================================== ##
## Blair's study
## =================================================================================================================== ##

phenotypic_data <- read_excel(path = paste0(root, "/gap_analysis_landraces/Input_data/_phenotypic_data/Bean/Blair_et_al_2009_races_subgroups_phenotypic.xlsx"), sheet = 2)

par(mfrow = c(1,3))
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(Meso.total, Andean.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()

par(mfrow = c(1,3))
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
# phenotypic_data %>% select(D_J1, D_J2, G, M1, M2, NG1, NG2, P1, P2) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot(is.corr = T, method = "ellipse", type = "upper")

par(mfrow = c(1,3))
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "pearson", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "spearman", use = "complete.obs") %>% corrplot.mixed()
phenotypic_data %>% select(D_J.total, G, M.total, NG.total, P.total) %>% cor(method = "kendall", use = "complete.obs") %>% corrplot.mixed()
rm(phenotypic_data)

## =================================================================================================================== ##
## CIAT + USDA information with climate data
## =================================================================================================================== ##

# PCA
# MFA
# Stepwise Discriminant analysis
# Canonical Discriminant analysis

# Fisher transformation para variables categóricas
# 1 componente: tamaño semilla muy util para discriminar
# 2. Habito crecimiento, dias a floracion
# 3. Color semilla, patron, brillo

genotypic_climate <- readRDS(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_climate.RDS"))
genotypic_climate %>% glimpse
genotypic_climate_cmplt <- genotypic_climate[complete.cases(genotypic_climate),]
genotypic_climate_cmplt <- unique(genotypic_climate_cmplt); rownames(genotypic_climate_cmplt) <- 1:nrow(genotypic_climate_cmplt)

# Descriptive analysis: quantitative variables
genotypic_climate_cmplt %>% dplyr::select(Elevation, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  gather(Variable, Value) %>% ggplot(aes(x = Value, fill = Variable, alpha = .6)) +
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
genotypic_climate_cmplt %>% dplyr::select(Elevation, Longitude, Latitude, Seed.weight, bio_1:bio_19) %>%
  psych::describe() %>% select(mean, sd, median, min, max, range)

# Descriptive analysis: qualitative variables
fqTable <- genotypic_climate_cmplt %>% dplyr::select(Growth.habit, Seed.color, Seed.shape, Seed.brightness, Owner) %>%
  gather(measure, value) %>%
  count(measure, value) %>%
  spread(measure, n) %>%
  gather(key = Variable, value = Count, Growth.habit:Seed.shape)
fqTable <- fqTable[complete.cases(fqTable),]; rownames(fqTable) <- 1:nrow(fqTable); colnames(fqTable)[1] <- "Category"
fqTable <- fqTable %>% dplyr::mutate(Percentage = Count/nrow(genotypic_climate_cmplt))

fqTable %>% ggplot(aes(x =  reorder(Category, Percentage), y = Percentage*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Percentage (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))

# Identify multivariate outliers


# Calculate Mahalanobis Distance with height and weight distributions
m_dist <- mahalanobis(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19),
                      colMeans(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19)),
                      cov(genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19)))

pca.res <- genotypic_climate_cmplt %>% dplyr::select(Elevation, Latitude, Seed.weight, bio_1:bio_19) %>%
  FactoMineR::PCA(X = ., scale.unit = T, graph = T)

test <- data.frame(pca.res$ind$coord[,1:2], m_dist)
test %>% ggplot(aes(x = Dim.1, y = Dim.2, col = m_dist)) + geom_point()
test <- data.frame(genotypic_climate_cmplt[,c("Longitude", "Latitude")], m_dist)
test %>% ggplot(aes(x = Longitude, y = Latitude, col = m_dist)) + geom_point()

highchart() %>% 
  hc_title(text = "Scatter chart with number of coordinates and median harvested area by country") %>% 
  hc_add_series_scatter(x = test$Longitude, y = test$Latitude, color = test$m_dist, label = genotypic_climate_cmplt$Owner)

# Mahalanobis Outliers - Threshold set to 12
df$outlier_maha <- "No"
df$outlier_maha[df$m_dist > 12] <- "Yes"



# Factor Analysis of Mixed Data
set.seed(1)
famd_res <- FactoMineR::FAMD(base = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18), graph = T)
fviz_screeplot(famd_res) + theme_bw()
fviz_famd_var(famd_res) + theme_bw()
fviz_famd_var(famd_res, "quanti.var", repel = TRUE, col.var = "black") + theme_bw()
fviz_famd_var(famd_res, "quali.var", col.var = "black")
fviz_famd_ind(famd_res, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Multiple Factor Analysis
set.seed(1)
mfa_res <- FactoMineR::MFA(base = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18), group = c(2, 4, 9), type = c("s", "n", "s"), graph = T)

# Random forest clustering
no.forests <- 25
no.trees <- 3000
set.seed(1)
distRF <- RFdist(datRF = genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18) %>% unique(),
                 mtry1 = 3, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)

### Clustering methods

## Gower distance

set.seed(1)
gower.dist <- daisy(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18),
                    metric = "gower", type = list(logratio = 3))
summary(gower.dist)
gower.mat <- as.matrix(gower.dist)

fviz_dist(as.dist(gower.mat),
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

res.hc <- hclust(d = gower.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)

fviz_dend(res.hc, k = 3, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

library(ClustOfVar)
stability(tree = res.hc, B = 40, graph = T)






fviz_nbclust(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% dplyr::select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18),
             kmeans, method = "gap_stat") + geom_vline(xintercept = 4, linetype = 2) + 
  labs(subtitle = "Elbow method")


# Fuzzy clustering

# T-SNE
Labels <- train$label
train$label <- as.factor(train$label)
## for plotting
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)

## Executing the algorithm on curated data
set.seed(1)
tsne <- Rtsne(genotypic_climate_cmplt[sample(x = 1:nrow(genotypic_climate_cmplt), size = 2000, replace = F),] %>% select(Elevation, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, wc2.0_bio_30s_01:wc2.0_bio_30s_18) %>% unique(),
              dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

## Plotting
plot(tsne$Y, main = "tsne", pch = 20)
text(tsne$Y, labels=train$label, col=colors[train$label])


# Stepwise Discriminant analysis

# Canonical Discriminant analysis
