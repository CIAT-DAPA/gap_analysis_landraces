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

"ID"                                     "SOURCE"                                 "CLEANED_BY"                            
[4] "Accession number"                       "Synonyms"                               "Common names"                          
[7] "INTERPRETED_NAME_CSOSA"                 "TO_USE_ACID"                            "Common_name_ACID"                      
[10] "Genepool_literature_ACID"               "Genepool_literature_ACID_1"             "Race_interpreted_ACID"                 
[13] "Race_literature_ACID"                   "Subgroup_interpreted_ACID"              "Subgroup_literature_ACID"              
[16] "Reference_ACID"                         "TEST_vernacular"                        "name_literature_vernacular"            
[19] "Genepool_literature_vernacular"         "Race_interpreted_LITERATURE_vernacular" "Race_literature_vernacular"            
[22] "Subgroup_literature_vernacular"         "Reference_vernacular"                   "Genus"                                 
[25] "Species"                                "Subspecies"                             "Variety"                               
[28] "Biological status"                      "Type of material"                       "CORE collection"                       
[31] "Country"                                "Department"                             "County"                                
[34] "Place"                                  "Altitude (masl)"                        "Latitude (decimal)"                    
[37] "Longitude (decimal)"                    "lat_geor"                               "long_geor"                             
[40] "coord_status"                           "Date of collection ( dd-mm-yyyy )"      "Name"                                  
[43] "Name2"                                  "Institution"                            "Country3"                              
[46] "Date of receipt ( dd-mm-yyyy )"         "Growth habit"                           "Seed color"                            
[49] "Seed shape"                             "Seed brightness"                        "100 seed weight (g)"                   
[52] "Protein"                                "Genepool_WEIGHT_fix"                    "GENEPOOL_PROTEIN"                      
[55] "RACE_PROTEIN"                           "Responsible11"

names(ciat) <- c("ID", "Source", "Cleaned.by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name", "Test", "Vernacular.name",
                 "Genepool", "Race.interpreted", "Race", "Subgroup",
                 "Reference", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Material.type", "CORE.collection",
                 "Country", "Department", "County", "Place", "Altitude", "Latitude", "Longitude",
                 "Lat.geo", "Lon.geo", "Coord.status", "Collection.date",
                 "Name", "Name.2", "Institution", "Country.3", "Receipt.date",
                 "Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight",
                 "Protein", "Genepool.weight", "Genepool.protein", "Race.protein", "Responsible.11")

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
# # load world shapefile
# shp_wld <- rgdal::readOGR(dsn = paste0(root, "/gap_analysis_landraces/Input_data/_maps/Global_administrative_unit_layers/gaul_2014"), layer = "G2014_2013_1")
# 
# # Load world raster
# rst_wld <- raster::raster(paste0(root, "/gap_analysis_landraces/Input_data/presence_data/world_body_waters_2-5.asc"))
# 
# # Identify accession without coordinates to do Georreferenciation process
# ciat_empyCoordinates <- ciat[which(is.na(ciat$Longitude) & is.na(ciat$Latitude)),]
# ciat_empyCoordinates <- ciat_empyCoordinates %>% dplyr::filter((!is.na(Country) |
#                                                                  !is.na(Department) |
#                                                                  !is.na(County) |
#                                                                  !is.na(Place)) & !is.na(Protein))
# if(!file.exists(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/coord4georef.csv"))){
#   write.csv(x = ciat_empyCoordinates, file = paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/coord4georef.csv"), row.names = F)
# }; rm(ciat_empyCoordinates)

# Let just accessions with coordinates
ciat2 <- ciat %>% dplyr::filter(!is.na(Longitude) & !is.na(Altitude) &
                                 !is.na(Growth.habit) & !is.na(Seed.color) &
                                 !is.na(Seed.shape) & !is.na(Seed.brightness) &
                                 !is.na(Seed.weight) & !is.na(Protein) & !is.na(Genepool.protein))
nrow(ciat2) # 22032 (old and original), 12545 (new one with vernacular names)
write.csv(x = ciat2, file = "D:/ciat_beans_filtered_by_altitude_by_predictors.csv", row.names = F)

# # Identify wrong coordinates
# # Using a shapefile
# over_res <- sp::over(SpatialPoints(coords = data.frame(lon = ciat$Longitude, lat = ciat$Latitude), proj4string = CRS(projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), as(shp_wld, "SpatialPolygons"))
# ciat$Wrong.coordinates <- as.numeric(is.na(over_res)); rm(over_res)
# sum(ciat$Wrong.coordinates) # 153 (old and original), 87 (new one with vernacular names)
# # plot(shp_wld); points(ciat[,c("Longitude", "Latitude")], col = 4, pch = 20); points(ciat[ciat$Wrong.coordinates == 1, c("Longitude", "Latitude")], col = 2, pch = 20)
# 
# # Using a raster file
# ciat$Wrong.coordinates2 <- raster::extract(x = rst_wld, y = ciat[c("Longitude", "Latitude")])
# sum(is.na(ciat$Wrong.coordinates2)) # 69 (new one with vernacular names)
# rm(shp_wld, rst_wld)
# 
# mapspam <- raster::brick(paste0(root, "/gap_analysis_landraces/Input_data/_crop_areas/MapSPAM/Bean/spam2005v2r0_harvested-area_bean_total.nc"), lvar = 4)
# mapspam <- mapspam[[1]]
# 
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(mapspam), na.color = "transparent")
# m <- leaflet() %>% addTiles() %>%
#   addRasterImage(mapspam, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(mapspam),
#             title = "MapSPAM harvested area") %>%
#   addCircles(~Longitude, ~Latitude, weight = 3, radius = 40, 
#              color = "#ffa500", stroke = TRUE, fillOpacity = 0.8, data = ciat_landraces) # popup = ct$type, 
# # %>% addMarkers(~Longitude, ~Latitude, label = ~as.character(Common_names), data = ciat_landraces)
# # saveWidget(m, file =" m.html")
# 
# sum(!is.na(ciat$Longitude) & !is.na(ciat$Latitude)) # Accessions with coordinates
# sum(!is.na(ciat$Place) & is.na(ciat$Longitude)) # Accessions with place but without coordinates
# sum(is.na(ciat$Place) & is.na(ciat$Longitude)) # Accessions without place and coordinates
# sum(ciat %>% select(Altitude:Longitude, Growth.habit:Seed.weight, Protein) %>% complete.cases)
# sum(ciat %>% select(Genepool, Race.interpreted, Subgroup, Altitude:Longitude, Growth.habit:Seed.weight, Protein) %>% complete.cases)
# sum(!is.na(ciat$Vernacular.name))
# sum(!is.na(ciat$Vernacular.name) & !is.na(ciat$Longitude))
# 
# predictorList <- c("Altitude", "Latitude", "Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight", "Protein", "Race.protein")
# for(i in 1:length(predictorList)){
#   eval(parse(text = paste0("cat(sum(!is.na(ciat$Longitude) & !is.na(ciat$", predictorList[i], ")), '\n')")))
# }; rm(i)

# Split colors
ciat2 <- ciat2 %>% tidyr::separate(Seed.color, into = c("Seed.color", "Seed.color2", "Seed.color3"), sep = ",") 
ciat2$Seed.color[grep(pattern = "Crema ", x = ciat2$Seed.color)] <- "Cream"
ciat2$Seed.color[grep(pattern = "Rosaso", x = ciat2$Seed.color)] <- "Pink"

ciat2$Seed.color2[grep(pattern = " Black", x = ciat2$Seed.color2)] <- "Black"
ciat2$Seed.color2[grep(pattern = " Brown", x = ciat2$Seed.color2)] <- "Brown"
ciat2$Seed.color2[grep(pattern = " Cream", x = ciat2$Seed.color2)] <- "Cream"
ciat2$Seed.color2[grep(pattern = " Other", x = ciat2$Seed.color2)] <- "Other"
ciat2$Seed.color2[grep(pattern = " Pink", x = ciat2$Seed.color2)] <- "Pink"
ciat2$Seed.color2[grep(pattern = " Purple", x = ciat2$Seed.color2)] <- "Purple"
ciat2$Seed.color2[grep(pattern = " Red", x = ciat2$Seed.color2)] <- "Red"
ciat2$Seed.color2[grep(pattern = " White", x = ciat2$Seed.color2)] <- "White"
ciat2$Seed.color2[grep(pattern = " Yellow", x = ciat2$Seed.color2)] <- "Yellow"

ciat2$Seed.color3[grep(pattern = " Black", x = ciat2$Seed.color3)] <- "Black"
ciat2$Seed.color3[grep(pattern = " Brown", x = ciat2$Seed.color3)] <- "Brown"
ciat2$Seed.color3[grep(pattern = " Cream", x = ciat2$Seed.color3)] <- "Cream"
ciat2$Seed.color3[grep(pattern = " Other", x = ciat2$Seed.color3)] <- "Other"
ciat2$Seed.color3[grep(pattern = " Pink", x = ciat2$Seed.color3)] <- "Pink"
ciat2$Seed.color3[grep(pattern = " Purple", x = ciat2$Seed.color3)] <- "Purple"
ciat2$Seed.color3[grep(pattern = " Red", x = ciat2$Seed.color3)] <- "Red"
ciat2$Seed.color3[grep(pattern = " White", x = ciat2$Seed.color3)] <- "White"
ciat2$Seed.color3[grep(pattern = " Yellow", x = ciat2$Seed.color3)] <- "Yellow"

color_list <- c(ciat2$Seed.color %>% as.character %>% unique,
                ciat2$Seed.color2 %>% as.character %>% unique,
                ciat2$Seed.color3 %>% as.character %>% unique) %>% unique %>% sort

for(i in 1:length(color_list)){
  eval(parse(text = paste0("ciat2$Color_", color_list[i], " <- 0")))
  col_id <- which(ciat2$Seed.color == color_list[i] | ciat2$Seed.color2 == color_list[i] | ciat2$Seed.color3 == color_list[i])
  if(length(col_id) > 0){
    eval(parse(text = paste0("ciat2$Color_", color_list[i], "[col_id] <- 1")))
  }
}; rm(i, col_id, color_list)

ciat2$Seed.color <- ciat2$Seed.color2 <- ciat2$Seed.color3 <- NULL

# Split proteins
ciat2$Protein[grep(pattern = "\\?", x = ciat2$Protein)] <- NA
ciat2 <- ciat2 %>% tidyr::separate(Protein, into = c("Protein", "Protein2", "Protein3", "Protein4", "Protein5"), sep = ",")
ciat2$Protein[grep(pattern = "Ca1\\(2D\\)", x = ciat2$Protein)] <- "Ca1"
ciat2$Protein[grep(pattern = "CAR\\(2D\\)", x = ciat2$Protein)] <- "CAR"
ciat2$Protein[grep(pattern = "CAR\\(2D\\)H1", x = ciat2$Protein)] <- "CAR,H1"
ciat2$Protein[grep(pattern = "CH \\(2D\\)", x = ciat2$Protein)] <- "CH"
ciat2$Protein[grep(pattern = "CH\\(2D\\)", x = ciat2$Protein)] <- "CH"
ciat2$Protein[grep(pattern = "H1\\(2D\\)", x = ciat2$Protein)] <- "H1"
ciat2$Protein[grep(pattern = "H2\\(2D\\)", x = ciat2$Protein)] <- "H2"
ciat2$Protein[grep(pattern = "HE\\(2D\\)", x = ciat2$Protein)] <- "HE"
ciat2$Protein[grep(pattern = "L \\(2D\\)", x = ciat2$Protein)] <- "L"
ciat2$Protein[grep(pattern = "LI\\(2D\\)", x = ciat2$Protein)] <- "LI"
ciat2$Protein[grep(pattern = "S\\(2D\\)", x = ciat2$Protein)] <- "S"
ciat2$Protein[grep(pattern = "Sd\\(2D\\)", x = ciat2$Protein)] <- "Sd"
ciat2$Protein[grep(pattern = "T \\(2D\\)", x = ciat2$Protein)] <- "T"
ciat2$Protein[grep(pattern = "T\\(2D\\)", x = ciat2$Protein)] <- "T"
ciat2$Protein[grep(pattern = "TI1\\(2D\\)", x = ciat2$Protein)] <- "TI1"
ciat2$Protein[grep(pattern = "TI2\\(2D\\)", x = ciat2$Protein)] <- "TI2"

ciat2$Protein2[grep(pattern = " H1\\(LCG\\)", x = ciat2$Protein2)] <- "H1"
ciat2$Protein2[grep(pattern = "^CAR\\(2D\\)", x = ciat2$Protein2)] <- "CAR"
ciat2$Protein2[grep(pattern = "^CH\\(2D\\)", x = ciat2$Protein2)] <- "CH"
ciat2$Protein2[grep(pattern = "^H1\\(2D", x = ciat2$Protein2)] <- "H1"
ciat2$Protein2[grep(pattern = "^S\\(2D", x = ciat2$Protein2)] <- "S"

ciat2$Protein3[grep(pattern = "^CAR\\(2D\\)", x = ciat2$Protein3)] <- "CAR"

protein_list <- c(ciat2$Protein %>% as.character %>% unique,
                ciat2$Protein2 %>% as.character %>% unique,
                ciat2$Protein3 %>% as.character %>% unique,
                ciat2$Protein4 %>% as.character %>% unique,
                ciat2$Protein5 %>% as.character %>% unique) %>% unique %>% sort
protein_list <- protein_list[-1]

for(i in 1:length(protein_list)){
  eval(parse(text = paste0("ciat2$Protein_", protein_list[i], " <- 0")))
  protein_id <- which(ciat2$Protein == protein_list[i] | ciat2$Protein2 == protein_list[i] | ciat2$Protein3 == protein_list[i] | ciat2$Protein4 == protein_list[i] | ciat2$Protein5 == protein_list[i])
  if(length(protein_id) > 0){
    eval(parse(text = paste0("ciat2$Protein_", protein_list[i], "[protein_id] <- 1")))
  }
}; rm(i, protein_id, protein_list)

ciat2$Protein <- ciat2$Protein2 <- ciat2$Protein3 <- ciat2$Protein4 <- ciat2$Protein5 <- NULL
names(ciat2)

shp_ame <- rgdal::readOGR(dsn = paste0("D:/Americas"), layer = "AMERICAS")
over_res <- sp::over(SpatialPoints(coords = data.frame(lon = ciat2$Longitude, lat = ciat2$Latitude), proj4string = CRS(projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), as(shp_ame, "SpatialPolygons"))
ciat2$Analysis <- as.numeric(is.na(over_res)); rm(over_res)
ciat2$Analysis[which(ciat2$Analysis == "0")] <- "Americas"
ciat2$Analysis[which(ciat2$Analysis == "1")] <- "World"

write.csv(x = ciat2, file = "D:/ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv", row.names = F)

if(!file.exists(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/ciatOrganizedVariables.RDS"))){
  saveRDS(ciat2, paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/ciatOrganizedVariables.RDS"))
}

# Now, go to linux servers!!!

# Calculate biovars
bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min_v2", full.names = T)
tmax <- bioList[grep(pattern = "tmax", x = bioList)]
tmax <- tmax[grep(pattern = "tif$", x = tmax)] %>% mixedsort
tmax <- raster::stack(tmax)

tmin <- bioList[grep(pattern = "tmin", x = bioList)]
tmin <- tmin[grep(pattern = "tif$", x = tmin)] %>% mixedsort
tmin <- raster::stack(tmin)

prec <- bioList[grep(pattern = "prec", x = bioList)]
prec <- prec[grep(pattern = "tif$", x = prec)] %>% mixedsort
prec <- raster::stack(prec)

srad <- bioList[grep(pattern = "srad", x = bioList)]
srad <- srad[grep(pattern = "tif$", x = srad)] %>% mixedsort
srad <- raster::stack(srad)

bios <- dismo::biovars(prec = srad, tmin = srad, tmax = srad)

# Radiation - annual mean (Bio20)
# Radiation - highest period (Bio21)
# Radiation - lowest period (Bio22)
# Radiation - seasonality (Bio23)
# Radiation - wettest quarter (Bio24)
# Radiation - driest quarter (Bio25)
# Radiation - warmest quarter (Bio26)
# Radiation - coldest quarter (Bio27)
# 

###################################################################################
# Extract climate information on Linux servers
###################################################################################
# ciat2 <- readRDS("ciatOrganizedVariables.RDS")
ciat2 <- read.csv("ciat_beans_filtered_by_altitude_by_predictors_by_americas.csv")
bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "bio", x = bioList)]
bioList <- bioList[grep(pattern = "tif", x = bioList)] %>% mixedsort
grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
bioList <- raster::stack(bioList)

climate_data <- raster::extract(x = bioList, y = ciat2[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- rownames(ciat2[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- row_id %>% as.character() %>% as.numeric()

climate_data <- climate_data %>% as.data.frame()
climate_data$Accession.number <- ciat2$Accession.number[row_id]; rm(row_id, grep2)

genotypic_climate <- inner_join(x = ciat2, y = climate_data, by = "Accession.number")
write.csv(genotypic_climate, "ciat_beans_filtered_with_climate.csv", row.names = F)

genotypic_climate <- read.csv("D:/ciat_beans_filtered_with_climate.csv")
original <- gs_ls("Bean_landrace_name_table")
original <- gs_title("Bean_landrace_name_table")
original %>% gs_browse(ws = "Phaseolus_vulgaris_landraces_G")
original <- original %>% gs_read(ws = "Phaseolus_vulgaris_landraces_G")
nrow(original) # 37987 (old and original), 23831 (new one with vernacular names)

names(original) <- c("ID", "Source", "Cleaned_by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name.lit", "Test", "Vernacular.name.lit", "Genepool.lit", "Race.interpreted.lit", "Race",
                 "Subgroup.lit", "Reference", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Type.of.material", "CORE.collection",
                 "Country", "Department", "County", "Place", "Altitude", "Latitude", "Longitude", "Lat_geo", "Lon_geo",
                 "Coord.status",
                 "Date.collection", "Name", "Name2", "Institution", "Country3",
                 "Date.receipt", "Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight",
                 "Protein", "Genepool.weight.fix", "Genepool.protein", "Race.protein", "Responsible11")
original <- original %>% select(ID, Reference)

genotypic_climate <- inner_join(x = genotypic_climate, y = original, by = "ID")
write.csv(genotypic_climate, "D:/ciat_beans_filtered_with_climate_reference.csv", row.names = F)

## =================================================================================================================== ##
## Genesys database
## =================================================================================================================== ##

# # Occurrence data all
# coll  <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_all/coll.csv"))
# core  <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_all/core.csv"))
# geo   <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_all/geo.csv"))
# names <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_all/names.csv"))
# 
# names$genesysId <- as.integer(as.character(names$genesysId))
# 
# # CORE + GEO
# core_geo <- left_join(x = core, y = geo, by = "genesysId")
# sum(!is.na(core_geo$longitude & core_geo$latitude))
# 
# # NAMES + CORE
# names_core <- left_join(x = names, y = core, by = "genesysId")
# names_core_wout_g <- names_core[-grep(pattern = "^G[0-9]*", names_core$name),]
# names_core_wout_g <- names_core_wout_g[-grep(pattern = "^G-[0-9]*", names_core_wout_g$name),]
# names_core_wout_g <- names_core_wout_g[-grep(pattern = "^G [0-9]*", names_core_wout_g$name),]
# names_core_wout_g <- names_core_wout_g[-grep(pattern = "^PI [0-9]*", names_core_wout_g$name),]
# names_core_wout_g <- names_core_wout_g[-grep(pattern = "^W6 [0-9]*", names_core_wout_g$name),]
# names_core_wout_g <- names_core_wout_g[-which(!is.na(match(names_core_wout_g$name, coll$collNumb))),]
# rownames(names_core_wout_g) <- 1:nrow(names_core_wout_g)
# 
# 
# # Occurrence data landraces
# coll  <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_landraces/coll.csv"))
# core  <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_landraces/core.csv"))
# geo   <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_landraces/geo.csv"))
# names <- read.csv(paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_genesys_data/Bean/_landraces/names.csv"))
# 
# names$genesysId <- as.integer(as.character(names$genesysId))

## =================================================================================================================== ##
## GRIN database
## =================================================================================================================== ##

filename <- paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_usda_data/Bean/GRIN_GLOBAL_BEAN_LAND.xlsx")
sheet_names <- excel_sheets(filename)

GRIN <- lapply(sheet_names, function(x){
  read_excel(path = filename, sheet = x)
}); names(GRIN) <- sheet_names; rm(sheet_names, filename)

GRIN$SOURCE <- GRIN$SOURCE %>% select(Accession, Geography, `Elevation (meters)`, Longitude, Latitude)
GRIN$ACCESSION <- GRIN$ACCESSION %>% select(Accession, Taxon, Name, Origin, Status)
GRIN$CROP_TRAITS <- GRIN$CROP_TRAITS %>% select(Accession, `Crop Trait`, `Coded Value`, `Numeric Value`) # `Crop Trait Observation ID`

# Re-arrange crops traits data
GRIN$CROP_TRAITS <- GRIN$CROP_TRAITS %>% dplyr::filter(`Crop Trait` %in% c("Habit", "Seed color (base)", "Seed shape", "Seed luster", "100 Seed weight in grams"))
GRIN$CROP_TRAITS$Value <- GRIN$CROP_TRAITS$`Coded Value`
GRIN$CROP_TRAITS$Value[!is.na(GRIN$CROP_TRAITS$`Numeric Value`)] <- GRIN$CROP_TRAITS$`Numeric Value`[!is.na(GRIN$CROP_TRAITS$`Numeric Value`)]
GRIN$CROP_TRAITS$`Coded Value` <- GRIN$CROP_TRAITS$`Numeric Value` <- NULL
GRIN$CROP_TRAITS <- GRIN$CROP_TRAITS %>%
  group_by(`Crop Trait`, Accession) %>%
  mutate(ind = row_number()) %>%
  spread(`Crop Trait`, Value) %>% 
  select(Accession, Habit, `Seed color (base)`, `Seed shape`, `Seed luster`, `100 Seed weight in grams`)
GRIN$CROP_TRAITS$`100 Seed weight in grams` <- as.numeric(as.character(GRIN$CROP_TRAITS$`100 Seed weight in grams`))
names(GRIN$CROP_TRAITS)[2:ncol(GRIN$CROP_TRAITS)] <- c("Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight")

GRIN$SOURCE_CRTRAITS <- dplyr::left_join(x = GRIN$SOURCE, y = GRIN$CROP_TRAITS, by = "Accession")
names(GRIN$SOURCE_CRTRAITS)[3] <- "Elevation"
GRIN$SOURCE_CRTRAITS$Geography <- NULL

# Create Determinacy !!!!

GRIN$SOURCE_CRTRAITS$Growth.habit <- GRIN$SOURCE_CRTRAITS$Growth.habit %>% as.character()
GRIN$SOURCE_CRTRAITS$Seed.color <- GRIN$SOURCE_CRTRAITS$Seed.color %>% as.character()
GRIN$SOURCE_CRTRAITS$Seed.shape <- GRIN$SOURCE_CRTRAITS$Seed.shape %>% as.character()
GRIN$SOURCE_CRTRAITS$Seed.brightness <- GRIN$SOURCE_CRTRAITS$Seed.brightness %>% as.character()

GRIN$SOURCE_CRTRAITS$Growth.habit[grep(pattern = "Determinate bush", x = GRIN$SOURCE_CRTRAITS$Growth.habit)] <- "Bush"
GRIN$SOURCE_CRTRAITS$Growth.habit[grep(pattern = "Indeterminate bush", x = GRIN$SOURCE_CRTRAITS$Growth.habit)] <- "Bush-Indeterminate"
GRIN$SOURCE_CRTRAITS$Growth.habit[grep(pattern = "Indeterminate climbing", x = GRIN$SOURCE_CRTRAITS$Growth.habit)] <- "Climbing"
GRIN$SOURCE_CRTRAITS$Growth.habit[grep(pattern = "Indeterminate prostrate or vining but not climbing", x = GRIN$SOURCE_CRTRAITS$Growth.habit)] <- "Prostrate-Indeterminate"

GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Dark brown", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Brown"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Light brown", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Brown"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Cream-beige", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Cream"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Dark Pink", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Pink"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Dark purple", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Purple"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Light purple", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Purple"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Dark red", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Red"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Light red", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Red"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Red-purple", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Red"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Dark yellow", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Yellow"
GRIN$SOURCE_CRTRAITS$Seed.color[grep(pattern = "Light yellow", x = GRIN$SOURCE_CRTRAITS$Seed.color)] <- "Yellow"

GRIN$SOURCE_CRTRAITS$Seed.shape[grep(pattern = "Reniform or kidney type", x = GRIN$SOURCE_CRTRAITS$Seed.shape)] <- "Kidney"

GRIN$SOURCE_CRTRAITS$Seed.brightness[grep(pattern = "Brilliant or shiny", x = GRIN$SOURCE_CRTRAITS$Seed.brightness)] <- "Bright"
GRIN$SOURCE_CRTRAITS$Seed.brightness[grep(pattern = "Opaque or dull", x = GRIN$SOURCE_CRTRAITS$Seed.brightness)] <- "Opaque"

ciat_landraces2 <- ciat_landraces %>% select(Accession, Altitude, Longitude, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight)
ciat_landraces2 <- ciat_landraces2 %>% separate(Seed.color, into = c("Seed.color", "Seed.color2", "Seed.color3"), sep = ",") 

ciat_landraces2$Seed.color[grep(pattern = "Blanco ", x = ciat_landraces2$Seed.color)] <- "White"
ciat_landraces2$Seed.color[grep(pattern = "Crema ", x = ciat_landraces2$Seed.color)] <- "Cream"
ciat_landraces2$Seed.color[grep(pattern = "Morado ", x = ciat_landraces2$Seed.color)] <- "Purple"
ciat_landraces2$Seed.color[grep(pattern = "Rosaso", x = ciat_landraces2$Seed.color)] <- "Pink"

names(ciat_landraces2)[2] <- "Elevation"

ciat_landraces2 <- ciat_landraces2 %>% select(Accession, Elevation, Longitude, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight)
ciat_landraces2$Owner <- "CIAT"
GRIN$SOURCE_CRTRAITS$Owner <- "USDA"

ciat_usda <- rbind(ciat_landraces2, GRIN$SOURCE_CRTRAITS)
if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))){
  saveRDS(object = ciat_usda, file = paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))
} else {
  ciat_usda <- readRDS(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))
}

###################################################################################
# Extract climate information on Linux servers
###################################################################################
bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "bio", x = bioList)]
bioList <- bioList[grep(pattern = "tif", x = bioList)] %>% mixedsort
grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
# bioList <- bioList[grep2(pattern = c("bio_30s_01", "bio_30s_05", "bio_30s_07", "bio_30s_09", "bio_30s_10", "bio_30s_14", "bio_30s_17", "bio_30s_18"), x = bioList, fixed = F)]
bioList <- raster::stack(bioList)

if(!file.exists(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))){
  saveRDS(object = ciat_usda, file = paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))
} else {
  ciat_usda <- readRDS(paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_all.RDS"))
}

climate_data <- raster::extract(x = bioList, y = ciat_usda[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- rownames(ciat_usda[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- row_id %>% as.character() %>% as.numeric()

climate_data <- climate_data %>% as.data.frame()
climate_data$Accession <- ciat_usda$Accession[row_id]; rm(row_id, grep2)

genotypic_climate <- inner_join(x = ciat_usda, y = climate_data, by = "Accession")
saveRDS(genotypic_climate, paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_usda_climate.RDS"))
###################################################################################

## =================================================================================================================== ##
## Obtain statistics from MapSPAM and EarthStat
## =================================================================================================================== ##

# Set in a vector  the names of all countries
countryList <- unique(as.character(shp_wld@data$ADM0_NAME))
countryList <- countryList[which(countryList!="Antarctica")] # Discard antarctica's country

# Calculate sum of harvested area
calc_suma <- function(rObject ,i){
  
  country <- shp_wld[shp_wld@data$ADM0_NAME== countryList[i],]
  country_data <- raster::crop(rObject, extent(country)) # cut raster by conutries shape's
  
  country_data <- raster::mask(x = country_data, mask = country) #create a new raster from the cropped shape  
  
  values <- sum(!is.na(sp::over(SpatialPoints(coords = unique(data.frame(lon = ciat_landraces$Longitude,
                                                       lat = ciat_landraces$Latitude)),
                                   proj4string = CRS(projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
                     as(country, "SpatialPolygons"))), na.rm = T)
  values2 <- sum(country_data[], na.rm = T)
  
  country_data <- data.frame(ADM0_CODE = shp_wld@data$ADM0_CODE[which(shp_wld@data$ADM0_NAME == countryList[i])], Country = countryList[i], Count = sum(na.omit(values)), Harvested.area = values2)
  cat(paste0("Country: ", countryList[i], " done!\n"))
  return(country_data)

}
calc_suma <- cmpfun(calc_suma) # Compilate this function
is.compile <- function(func){
  # This function lets us know if a function has been byte-coded or not
  # If you have a better idea for how to do this - please let me know...
  if(class(func) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(func),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
} # Verify if it's compilated
is.compile(calc_suma); rm(is.compile)

## MapSPAM process
f <- list()
for(i in 1:length(countryList)){f[[i]] <- calc_suma(rObject = mapspam[[1]], i = i)}
df <- do.call(rbind, f)
df <- unique(df); rownames(df) <- 1:nrow(df)
rm(f, i)
df$Count <- (df$Count - min(df$Count, na.rm = T))/(max(df$Count, na.rm = T) - min(df$Count, na.rm = T))
df$Harvested.area <- (df$Harvested.area - min(df$Harvested.area, na.rm = T))/(max(df$Harvested.area, na.rm = T) - min(df$Harvested.area, na.rm = T))

highchart() %>% 
  hc_title(text = "Scatter chart with number of coordinates and total harvested area by country") %>% 
  hc_add_series_scatter(x = df$Count, y = df$Harvested.area, color = df$Harvested.area, label = df$Country)

plot_ly(df, x = df$Count, y = df$Harvested.area, 
        text = paste(df$Country),
        mode = "markers", color = df$Harvested.area, size = df$Count)

df[which(df$Count > 0), c("Country", "Count", "Harvested.area")] %>% gather(key = Variable, value = Value, -Country) %>%
  ggplot(aes(x = reorder(Country, Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") + theme_minimal() +
  xlab("Country") + ylab("Standarized value") + theme(axis.text.x = element_text(angle = 90))

rownames(df) <- df$Country

## EarthStat
earthstat <- raster::brick(paste0(root, "/gap_analysis_landraces/Input_data/_crop_areas/EarthStat/Bean/bean_AreaYieldProduction.nc"), lvar = 4)
earthstat <- earthstat[[5]]

f <- list()
for(i in 1:length(countryList)){f[[i]] <- calc_suma(rObject = earthstat, i = i)}
df2 <- do.call(rbind, f)
df2 <- unique(df2); rownames(df2) <- 1:nrow(df2)
rm(f, i)

df2$Count <- (df2$Count - min(df2$Count, na.rm = T))/(max(df2$Count, na.rm = T) - min(df2$Count, na.rm = T))
df2$Harvested.area <- (df2$Harvested.area - min(df2$Harvested.area, na.rm = T))/(max(df2$Harvested.area, na.rm = T) - min(df2$Harvested.area, na.rm = T))

highchart() %>% 
  hc_title(text = "Scatter chart with number of coordinates and total harvested area by country") %>% 
  hc_add_series_scatter(x = df2$Count, y = df2$Harvested.area, color = df2$Harvested.area, label = df2$Country)

plot_ly(df2, x = df2$Count, y = df2$Harvested.area, 
        text = paste(df2$Country),
        mode = "markers", color = df2$Harvested.area, size = df2$Count)

df2[which(df2$Count > 0), c("Country", "Count", "Harvested.area")] %>% gather(key = Variable, value = Value, -Country) %>%
  ggplot(aes(x = reorder(Country, Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") + theme_minimal() +
  coord_flip() + xlab("Country") + ylab("Standarized value")

rownames(df2) <- df2$Country
