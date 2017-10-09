# Check and clean occurrence data
# H. Achicanoy
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(reset = T); rm(list = ls())

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
root     <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9", "Windows" = "//dapadfs/Workspace_cluster_9")

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
suppressMessages(library(factoextra))
suppressMessages(library(gtools))
suppressMessages(library(googlesheets))

## =================================================================================================================== ##
## CIAT database
## =================================================================================================================== ##

# Load database from Google Drive
ciat <- gs_ls("Bean_landrace_name_table")
ciat <- gs_title("Bean_landrace_name_table")
ciat %>% gs_browse(ws = "Pvulgaris_CIATdb")
ciat <- ciat %>% gs_read(ws = "Pvulgaris_CIATdb")
nrow(ciat) # 37987 (old and original), 23831 (new one with vernacular names)

# ------------------------------------ #
# Update columns names
# ------------------------------------ #

names(ciat) <- c("ID", "Source", "Cleaned.by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name.csosa", "To.use.ACID", "Common.name.ACID",
                 "Genepool.ACID", "Genepool.literature.ACID","Race.interpreted.ACID",
                 "Race.literature.ACID", "Subgroup.interpreted.ACID", "Subgroup.literature.ACID",
                 "Reference.ACID", "TEST.vernacular", "Name.literature.vernacular",
                 "Genepool.literature.vernacular", "Race.interpreted.vernacular", "Race.literature.vernacular",
                 "Subgroup.literature.vernacular", "Reference.vernacular", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Material.type", "CORE.collection", "Country", "Department", "County", "Place",
                 "Altitude", "Latitude", "Longitude", "Lat.geo", "Lon.geo", "Coord.status", "Collection.date", "Name",
                 "Name2", "Institution", "Country3", "Receipt.date", "Growth.habit", "Seed.color",
                 "Seed.shape", "Seed.brightness", "Seed.weight", "Protein", "Genepool.WEIGHT.fix",
                 "Genepool.protein", "Race.protein", "Responsible11")

# ------------------------------------ #
# Update coordinates
# ------------------------------------ #

# Replace empty spaces with georreferenced coordinates
ciat <- ciat %>% filter(Coord.status != "No coords") # 16038
ciat$Latitude[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))] <- ciat$Lat.geo[which(!is.na(ciat$Lat.geo) & is.na(ciat$Latitude))]
ciat$Longitude[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))] <- ciat$Lon.geo[which(!is.na(ciat$Lon.geo) & is.na(ciat$Longitude))]

# ------------------------------------ #
# Include altitude records from SRTM
# ------------------------------------ #

# Identify coordinates without altitude data
which(!is.na(ciat$Latitude) & is.na(ciat$Altitude)) %>% length
ciat %>% dplyr::filter(!is.na(Latitude) & is.na(Altitude)) %>% dplyr::select(Longitude, Latitude) %>% head

# # Density of altitude records
# plot(density(na.omit(ciat$Altitude)))
# dens <- density(na.omit(ciat$Altitude))
# integrate(approxfun(dens), lower = 3000, upper = 3500)
# # 0.006958508 with absolute error < 0.000024

srtm <- raster::raster(paste0(OSysPath, "/data_cluster_4/observed/gridded_products/srtm/Altitude_30s/alt"))
srtm.vals <- raster::extract(x = srtm,
                             y = ciat %>% dplyr::filter(!is.na(Latitude) & is.na(Altitude)) %>% dplyr::select(Longitude, Latitude))

# Density plots before and after update altitude records
ciat %>% ggplot(aes(x = Altitude)) + geom_density() # Before
srtm.vals %>% data.frame %>% ggplot(aes(x = .)) + geom_density() # SRTM values

ciat$Altitude[which(!is.na(ciat$Latitude) & is.na(ciat$Altitude))] <- srtm.vals

# # Density of altitude with new records according to georreferenced coordinates
# plot(density(as.numeric(na.omit(ciat$Altitude))))
# dens <- density(na.omit(ciat$Altitude))
# integrate(approxfun(dens), lower = 3000, upper = 5500)
# integrate(approxfun(dens), lower = 3500, upper = 5500)

rm(srtm.vals, srtm)

ciat <- ciat %>% filter(Altitude <= 3500)

# ------------------------------------ #
# Counts
# ------------------------------------ #

sum(!is.na(ciat$Longitude) & !is.na(ciat$Latitude)) # Accessions with coordinates
sum(!is.na(ciat$Place) & is.na(ciat$Longitude)) # Accessions with place but without coordinates
sum(is.na(ciat$Place) & is.na(ciat$Longitude)) # Accessions without place and coordinates
sum(ciat %>% select(Altitude:Genepool.protein) %>% complete.cases)
sum(ciat %>% select(Genepool, Race.interpreted, Subgroup, Altitude:Genepool.protein) %>% complete.cases)
sum(!is.na(ciat$Vernacular.name))
sum(!is.na(ciat$Vernacular.name) & !is.na(ciat$Longitude))

predictorList <- c("Altitude", "Latitude", "Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight", "Protein", "Genepool.protein")
for(i in 1:length(predictorList)){
  eval(parse(text = paste0("cat(sum(!is.na(ciat$Longitude) & !is.na(ciat$", predictorList[i], ")), '\n')")))
}

sum(ciat$Type.of.material == "Landrace", na.rm = T) # 27644 (old and original), 23831 (new one with vernacular names)
sum(!is.na(ciat$Common.names), na.rm = T) # 37987 (old and original), 15784 (new one with vernacular names)
sum(!is.na(ciat$Vernacular.name), na.rm = T) # 4196 (new one with vernacular names)
sum(ciat$Type.of.material == "Landrace" & !is.na(ciat$Vernacular.name), na.rm = T) # 4196

# ------------------------------------ #
# Biophysical information
# ------------------------------------ #

if(!file.exists(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE.RDS"))){
  
  coord_df <- ciat %>% dplyr::select(ID, Longitude, Latitude)
  
  # Environmental Rasters For Ecological Modeling
  envirem <- list.files(path = paste0(root, "/gap_analysis_landraces/Input_data/_maps/_envirem2_5"), full.names = T)
  envirem <- envirem[grep(pattern = "*.tif$", x = envirem)]
  envirem <- raster::stack(envirem)
  
  # Bioclim Variables
  bioVars <- list.files(paste0(OSysPath, "/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2"), full.names = T)
  bioVars <- bioVars[grep(pattern = "bio", x = bioVars)]
  bioVars <- bioVars[grep(pattern = "*.tif$", x = bioVars)] %>% mixedsort
  bioVars <- raster::stack(bioVars)
  
  coord_envirem <- raster::extract(x = envirem, y = coord_df[,c("Longitude", "Latitude")])
  colnames(coord_envirem) <- gsub(pattern = "current_2.5arcmin_", replacement = "", x = colnames(coord_envirem))
  coord_envirem <- cbind(coord_df, coord_envirem)
  
  coord_bioVars <- raster::extract(x = bioVars, y = coord_df[,c("Longitude", "Latitude")])
  coord_bioVars <- cbind(coord_df, coord_bioVars)
  
  biophysicalVars <- dplyr::inner_join(x = coord_envirem, y = coord_bioVars,  by = c("ID", "Longitude", "Latitude"))
  rm(coord_envirem, coord_bioVars)
  saveRDS(object = biophysicalVars, file = paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE.RDS"))
  
} else {
  biophysicalVars <- readRDS(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/BEAN-GRP-COORDINATES-CLIMATE.RDS"))
}

# ------------------------------------ #
# Filter database by physical descriptors
# ------------------------------------ #

ciat <- ciat %>% filter(To.use.ACID == 1)
ciat <- ciat %>% dplyr::filter(!is.na(Longitude) & !is.na(Altitude) &
                                 !is.na(Growth.habit) & !is.na(Seed.color) &
                                 !is.na(Seed.shape) & !is.na(Seed.brightness) &
                                 !is.na(Seed.weight) & !is.na(Protein) &
                                 !is.na(Genepool.protein))
nrow(ciat)
ciat %>% ggplot(aes(x = Longitude, y = Latitude)) + geom_point()

# ------------------------------------ #
# Arrange physical descriptors
# ------------------------------------ #

# Split colors
ciat <- ciat %>% tidyr::separate(Seed.color, into = c("Seed.color", "Seed.color2", "Seed.color3"), sep = ",") 
ciat$Seed.color[grep(pattern = "Crema ", x = ciat$Seed.color)] <- "Cream"
ciat$Seed.color[grep(pattern = "Rosaso", x = ciat$Seed.color)] <- "Pink"

ciat$Seed.color2[grep(pattern = " Black", x = ciat$Seed.color2)] <- "Black"
ciat$Seed.color2[grep(pattern = " Brown", x = ciat$Seed.color2)] <- "Brown"
ciat$Seed.color2[grep(pattern = " Cream", x = ciat$Seed.color2)] <- "Cream"
ciat$Seed.color2[grep(pattern = " Other", x = ciat$Seed.color2)] <- "Other"
ciat$Seed.color2[grep(pattern = " Pink", x = ciat$Seed.color2)] <- "Pink"
ciat$Seed.color2[grep(pattern = " Purple", x = ciat$Seed.color2)] <- "Purple"
ciat$Seed.color2[grep(pattern = " Red", x = ciat$Seed.color2)] <- "Red"
ciat$Seed.color2[grep(pattern = " White", x = ciat$Seed.color2)] <- "White"
ciat$Seed.color2[grep(pattern = " Yellow", x = ciat$Seed.color2)] <- "Yellow"

ciat$Seed.color3[grep(pattern = " Black", x = ciat$Seed.color3)] <- "Black"
ciat$Seed.color3[grep(pattern = " Brown", x = ciat$Seed.color3)] <- "Brown"
ciat$Seed.color3[grep(pattern = " Cream", x = ciat$Seed.color3)] <- "Cream"
ciat$Seed.color3[grep(pattern = " Other", x = ciat$Seed.color3)] <- "Other"
ciat$Seed.color3[grep(pattern = " Pink", x = ciat$Seed.color3)] <- "Pink"
ciat$Seed.color3[grep(pattern = " Purple", x = ciat$Seed.color3)] <- "Purple"
ciat$Seed.color3[grep(pattern = " Red", x = ciat$Seed.color3)] <- "Red"
ciat$Seed.color3[grep(pattern = " White", x = ciat$Seed.color3)] <- "White"
ciat$Seed.color3[grep(pattern = " Yellow", x = ciat$Seed.color3)] <- "Yellow"

color_list <- c(ciat$Seed.color %>% as.character %>% unique,
                ciat$Seed.color2 %>% as.character %>% unique,
                ciat$Seed.color3 %>% as.character %>% unique) %>% unique %>% sort

for(i in 1:length(color_list)){
  eval(parse(text = paste0("ciat$Color_", color_list[i], " <- 0")))
  col_id <- which(ciat$Seed.color == color_list[i] | ciat$Seed.color2 == color_list[i] | ciat$Seed.color3 == color_list[i])
  if(length(col_id) > 0){
    eval(parse(text = paste0("ciat$Color_", color_list[i], "[col_id] <- 1")))
  }
}; rm(i, col_id, color_list)

ciat$Seed.color <- ciat$Seed.color2 <- ciat$Seed.color3 <- NULL

# Split proteins
ciat$Protein[grep(pattern = "\\?", x = ciat$Protein)] <- NA
ciat <- ciat %>% tidyr::separate(Protein, into = c("Protein", "Protein2", "Protein3", "Protein4", "Protein5"), sep = ",")
ciat$Protein[grep(pattern = "Ca1\\(2D\\)", x = ciat$Protein)] <- "Ca1"
ciat$Protein[grep(pattern = "CAR\\(2D\\)", x = ciat$Protein)] <- "CAR"
ciat$Protein[grep(pattern = "CAR\\(2D\\)H1", x = ciat$Protein)] <- "CAR,H1"
ciat$Protein[grep(pattern = "CH \\(2D\\)", x = ciat$Protein)] <- "CH"
ciat$Protein[grep(pattern = "CH\\(2D\\)", x = ciat$Protein)] <- "CH"
ciat$Protein[grep(pattern = "H1\\(2D\\)", x = ciat$Protein)] <- "H1"
ciat$Protein[grep(pattern = "H2\\(2D\\)", x = ciat$Protein)] <- "H2"
ciat$Protein[grep(pattern = "HE\\(2D\\)", x = ciat$Protein)] <- "HE"
ciat$Protein[grep(pattern = "L \\(2D\\)", x = ciat$Protein)] <- "L"
ciat$Protein[grep(pattern = "LI\\(2D\\)", x = ciat$Protein)] <- "LI"
ciat$Protein[grep(pattern = "S\\(2D\\)", x = ciat$Protein)] <- "S"
ciat$Protein[grep(pattern = "Sd\\(2D\\)", x = ciat$Protein)] <- "Sd"
ciat$Protein[grep(pattern = "T \\(2D\\)", x = ciat$Protein)] <- "T"
ciat$Protein[grep(pattern = "T\\(2D\\)", x = ciat$Protein)] <- "T"
ciat$Protein[grep(pattern = "TI1\\(2D\\)", x = ciat$Protein)] <- "TI1"
ciat$Protein[grep(pattern = "TI2\\(2D\\)", x = ciat$Protein)] <- "TI2"

ciat$Protein2[grep(pattern = " H1\\(LCG\\)", x = ciat$Protein2)] <- "H1"
ciat$Protein2[grep(pattern = "^CAR\\(2D\\)", x = ciat$Protein2)] <- "CAR"
ciat$Protein2[grep(pattern = "^CH\\(2D\\)", x = ciat$Protein2)] <- "CH"
ciat$Protein2[grep(pattern = "^H1\\(2D", x = ciat$Protein2)] <- "H1"
ciat$Protein2[grep(pattern = "^S\\(2D", x = ciat$Protein2)] <- "S"

ciat$Protein3[grep(pattern = "^CAR\\(2D\\)", x = ciat$Protein3)] <- "CAR"

protein_list <- c(ciat$Protein %>% as.character %>% unique,
                ciat$Protein2 %>% as.character %>% unique,
                ciat$Protein3 %>% as.character %>% unique,
                ciat$Protein4 %>% as.character %>% unique,
                ciat$Protein5 %>% as.character %>% unique) %>% unique %>% sort
protein_list <- protein_list[-1]

for(i in 1:length(protein_list)){
  eval(parse(text = paste0("ciat$Protein_", protein_list[i], " <- 0")))
  protein_id <- which(ciat$Protein == protein_list[i] | ciat$Protein2 == protein_list[i] | ciat$Protein3 == protein_list[i] | ciat$Protein4 == protein_list[i] | ciat$Protein5 == protein_list[i])
  if(length(protein_id) > 0){
    eval(parse(text = paste0("ciat$Protein_", protein_list[i], "[protein_id] <- 1")))
  }
}; rm(i, protein_id, protein_list)

ciat$Protein <- ciat$Protein2 <- ciat$Protein3 <- ciat$Protein4 <- ciat$Protein5 <- NULL
names(ciat)

# ------------------------------------ #
# Define analyses to do
# ------------------------------------ #

shp_ame <- rgdal::readOGR(dsn = paste0(root, "/gap_analysis_landraces/Input_data/_maps/_shp_americas"), layer = "AMERICAS")
over_res <- sp::over(SpatialPoints(coords = data.frame(lon = ciat$Longitude, lat = ciat$Latitude), proj4string = CRS(projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), as(shp_ame, "SpatialPolygons"))
ciat$Analysis <- as.numeric(is.na(over_res)); rm(over_res)
ciat$Analysis[which(ciat$Analysis == "0")] <- "Americas"
ciat$Analysis[which(ciat$Analysis == "1")] <- "World"
rm(shp_ame)

suppressMessages(library(corrplot))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))

M <- cor(biophysicalVars[,-(1)], use = "complete.obs", method = "spearman")
corrplot(M, method = "square", type = "lower")
corrplot(M, method = "square", type = "lower", order = "hclust", addrect = 2)

bio_pca <- FactoMineR::PCA(X = biophysicalVars[,-(1:2)], scale.unit = T, ncp = 3, graph = F)
bio_pca$eig

bio_hcpc <- FactoMineR::HCPC(bio_pca, nb.clust = -1, graph = F)

fviz_dend(bio_hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

suppressMessages(library(Rtsne))
bio_tsne <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),-1] %>% unique, dims = 2, perplexity = 500, verbose = TRUE, max_iter = 500)
bio_tsne <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),-1] %>% unique, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
bio_tsne <- Rtsne(biophysicalVars[complete.cases(biophysicalVars),-1] %>% unique, dims = 2, perplexity = 50, verbose = TRUE, max_iter = 500)
plot(bio_tsne$Y, pch = 20, main = "tsne for biophysical variables")
