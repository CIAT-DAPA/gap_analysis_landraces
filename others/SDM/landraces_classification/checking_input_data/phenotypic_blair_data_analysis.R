# Phenotypic data exploration
# H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")

# Load packages
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})

# Load data
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
