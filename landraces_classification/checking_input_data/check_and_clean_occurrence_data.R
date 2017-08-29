# Check and clean occurrence data
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
suppressMessages(library(factoextra))
suppressMessages(library(gtools))

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
## CIAT database
## =================================================================================================================== ##

ciat <- read_excel(path = paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/CIAT_BEAN_DB_2017_08_25.xlsx"), sheet = "CLEANED_FILE")
nrow(ciat) # 37987 (old and original), 23831 (new one with vernacular names)

names(ciat) <- c("ID", "Source", "Cleaned_by", "Accession.number", "Synonyms", "Common.names",
                 "Interpreted.name", "Test", "Vernacular.name", "Genepool", "Race.interpreted", "Race",
                 "Subgroup", "Reference", "Genus", "Species", "Subspecies", "Variety",
                 "Biological.status", "Type.of.material", "CORE.collection",
                 "Country", "Department", "County", "Place", "Altitude", "Latitude", "Longitude",
                 "Date.collection", "Name", "Name2", "Institution", "Country3",
                 "Date.receipt", "Growth.habit", "Seed.color", "Seed.shape", "Seed.brightness", "Seed.weight",
                 "Days.to.flowering", "Place4", "Year", "Responsible", "First.harvest", "Last.harvest",
                 "Place5", "Year6", "Responsible7", "Reaction", "Responsible8", "Reaction9",
                 "Responsible10", "Date.evaluation", "Protein", "Responsible11")

sum(ciat$Type.of.material == "Landrace", na.rm = T) # 27644 (old and original), 23831 (new one with vernacular names)
sum(!is.na(ciat$Common.names), na.rm = T) # 37987 (old and original), 15784 (new one with vernacular names)
sum(!is.na(ciat$Vernacular.name), na.rm = T) # 4196 (new one with vernacular names)
sum(ciat$Type.of.material == "Landrace" & !is.na(ciat$Vernacular.name), na.rm = T) # 4196

# load world shapefile
shp_wld <- rgdal::readOGR(dsn = paste0(root, "/gap_analysis_landraces/Input_data/_maps/Global_administrative_unit_layers/gaul_2014"), layer = "G2014_2013_1")

# Load world raster
rst_wld <- raster::raster(paste0(root, "/gap_analysis_landraces/Input_data/presence_data/world_body_waters_2-5.asc"))

# Identify accession without coordinates to do Georreferenciation process
ciat_empyCoordinates <- ciat[which(is.na(ciat$Longitude) & is.na(ciat$Latitude)),]
ciat_empyCoordinates <- ciat_empyCoordinates %>% dplyr::filter((!is.na(Country) |
                                                                 !is.na(Department) |
                                                                 !is.na(County) |
                                                                 !is.na(Place)) & !is.na(Protein))
write.csv(x = ciat_empyCoordinates, file = paste0(root, "/gap_analysis_landraces/Input_data/_occurrence_data/_ciat_data/Bean/coord4georef.csv"), row.names = F)
rm(ciat_empyCoordinates)

# Let just accessions with coordinates
ciat <- ciat %>% dplyr::filter(!is.na(Longitude) & !is.na(ciat$Latitude) & !is.na(Protein))
nrow(ciat) # 22032 (old and original), 12545 (new one with vernacular names)

# Identify wrong coordinates
# Using a shapefile
over_res <- sp::over(SpatialPoints(coords = data.frame(lon = ciat$Longitude, lat = ciat$Latitude), proj4string = CRS(projargs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), as(shp_wld, "SpatialPolygons"))
ciat$Wrong.coordinates <- as.numeric(is.na(over_res)); rm(over_res)
sum(ciat$Wrong.coordinates) # 153 (old and original), 87 (new one with vernacular names)
# plot(shp_wld); points(ciat[,c("Longitude", "Latitude")], col = 4, pch = 20); points(ciat[ciat$Wrong.coordinates == 1, c("Longitude", "Latitude")], col = 2, pch = 20)

# Using a raster file
ciat$Wrong.coordinates2 <- raster::extract(x = rst_wld, y = ciat[c("Longitude", "Latitude")])
sum(is.na(ciat$Wrong.coordinates2)) # 69 (new one with vernacular names)
rm(shp_wld, rst_wld)

mapspam <- raster::brick(paste0(root, "/gap_analysis_landraces/Input_data/_crop_areas/MapSPAM/Bean/spam2005v2r0_harvested-area_bean_total.nc"), lvar = 4)
mapspam <- mapspam[[1]]

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(mapspam), na.color = "transparent")
m <- leaflet() %>% addTiles() %>%
  addRasterImage(mapspam, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(mapspam),
            title = "MapSPAM harvested area") %>%
  addCircles(~Longitude, ~Latitude, weight = 3, radius = 40, 
             color = "#ffa500", stroke = TRUE, fillOpacity = 0.8, data = ciat_landraces) # popup = ct$type, 
# %>% addMarkers(~Longitude, ~Latitude, label = ~as.character(Common_names), data = ciat_landraces)
# saveWidget(m, file =" m.html")

# Seed luster = Seed brightness

ciat2 <- ciat %>% dplyr::select(Test, Vernacular.name, Genepool, Race.interpreted, Subgroup, Altitude, Longitude, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, Protein)
# Split colors
ciat2 <- ciat2 %>% tidyr::separate(Seed.color, into = c("Seed.color", "Seed.color2", "Seed.color3"), sep = ",") 
ciat2$Seed.color[grep(pattern = "Crema ", x = ciat2$Seed.color)] <- "Cream"
ciat2$Seed.color[grep(pattern = "Rosaso", x = ciat2$Seed.color)] <- "Pink"
# ciat2$Seed.color[grep(pattern = "Blanco ", x = ciat2$Seed.color)] <- "White"
# ciat2$Seed.color[grep(pattern = "Morado ", x = ciat2$Seed.color)] <- "Purple"

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
  if(ciat2$Seed.color == color_list[i] | ciat2$Seed.color2 == color_list[i] | ciat2$Seed.color3 == color_list[i]){
    
  }
}; rm(i)

# Split proteins
ciat2 <- ciat2 %>% tidyr::separate(Protein, into = c("Protein", "Protein2", "Protein3", "Protein4", "Protein5"), sep = ",") 
# ciat2$Protein[grep(pattern = "^$", x = ciat2$Protein, fixed = T)] <- NA
ciat2$Protein[grep(pattern = "^B\\?", x = ciat2$Protein)] <- "B"
ciat2$Protein[grep(pattern = "^C\\?", x = ciat2$Protein)] <- "C"
ciat2$Protein[grep(pattern = "^Ca1\\(2D\\)?", x = ciat2$Protein)] <- "Ca1"
ciat2$Protein[grep(pattern = "CAR\\(2D\\)?$", x = ciat2$Protein)] <- "CAR"
ciat2$Protein[grep(pattern = "CAR\\(2D\\)H1", x = ciat2$Protein)] <- "CAR,H1"
ciat2$Protein[grep(pattern = "CH \\(2D\\)", x = ciat2$Protein)] <- "CH"
ciat2$Protein[grep(pattern = "CH\\(2D\\)", x = ciat2$Protein)] <- "CH"
ciat2$Protein[grep(pattern = "CH\\?", x = ciat2$Protein)] <- "CH"
ciat2$Protein[grep(pattern = "H\\?", x = ciat2$Protein)] <- "H"
ciat2$Protein[grep(pattern = "H1\\(2D\\)", x = ciat2$Protein)] <- "H1"
ciat2$Protein[grep(pattern = "H1\\?", x = ciat2$Protein)] <- "H1"
ciat2$Protein[grep(pattern = "H2\\(2D\\)", x = ciat2$Protein)] <- "H2"
ciat2$Protein[grep(pattern = "HE\\(2D\\)", x = ciat2$Protein)] <- "HE"
ciat2$Protein[grep(pattern = "L \\(2D\\)", x = ciat2$Protein)] <- "L"
ciat2$Protein[grep(pattern = "LI\\(2D\\)", x = ciat2$Protein)] <- "LI"
ciat2$Protein[grep(pattern = "M13\\? o M4\\?", x = ciat2$Protein)] <- "M13,M4"
ciat2$Protein[grep(pattern = "P1\\?", x = ciat2$Protein)] <- "P1"
ciat2$Protein[grep(pattern = "S\\(2D\\)", x = ciat2$Protein)] <- "S"
ciat2$Protein[grep(pattern = "S\\?$", x = ciat2$Protein)] <- "S"
ciat2$Protein[grep(pattern = "S\\?B\\?", x = ciat2$Protein)] <- "S,B"
ciat2$Protein[grep(pattern = "Sd\\(2D\\)", x = ciat2$Protein)] <- "Sd"
ciat2$Protein[grep(pattern = "SIMPLE \\?", x = ciat2$Protein)] <- "SIMPLE"
ciat2$Protein[grep(pattern = "T \\(2D\\)", x = ciat2$Protein)] <- "T"
ciat2$Protein[grep(pattern = "T\\(2D\\)", x = ciat2$Protein)] <- "T"
ciat2$Protein[grep(pattern = "T\\?", x = ciat2$Protein)] <- "T"
ciat2$Protein[grep(pattern = "TI1\\(2D\\)", x = ciat2$Protein)] <- "TI1"
ciat2$Protein[grep(pattern = "TI2\\(2D\\)", x = ciat2$Protein)] <- "TI2"
ciat2$Protein[grep(pattern = "To\\?", x = ciat2$Protein)] <- "To"

ciat2$Protein2[grep(pattern = " H1\\(LCG\\)", x = ciat2$Protein2)] <- "H1"
ciat2$Protein2[grep(pattern = "C\\?", x = ciat2$Protein2)] <- "C"
ciat2$Protein2[grep(pattern = "^Ca\\?", x = ciat2$Protein2)] <- "Ca"
ciat2$Protein2[grep(pattern = "^CAR\\(2D\\)", x = ciat2$Protein2)] <- "CAR"
ciat2$Protein2[grep(pattern = "^CH\\(2D\\)", x = ciat2$Protein2)] <- "CH"
ciat2$Protein2[grep(pattern = "^H1\\(2D", x = ciat2$Protein2)] <- "H1"
ciat2$Protein2[grep(pattern = "^Mu\\?", x = ciat2$Protein2)] <- "Mu"
ciat2$Protein2[grep(pattern = "^P1\\?", x = ciat2$Protein2)] <- "P1"
ciat2$Protein2[grep(pattern = "^S\\(2D", x = ciat2$Protein2)] <- "S"
ciat2$Protein2[grep(pattern = "^T\\?", x = ciat2$Protein2)] <- "T"

ciat2$Protein3[grep(pattern = "^C \\(\\?\\)", x = ciat2$Protein3)] <- "C"
ciat2$Protein3[grep(pattern = "^CAR\\(2D\\)", x = ciat2$Protein3)] <- "CAR"
ciat2$Protein3[grep(pattern = "^CH\\? \\(2D\\)", x = ciat2$Protein3)] <- "CH"

ciat_landraces3 <- ciat_landraces3 %>% dplyr::select(Accession, Common.names, Synonyms, Elevation, Longitude, Latitude, Growth.habit, Seed.color, Seed.shape, Seed.brightness, Seed.weight, Protein)

c(ciat2$Protein %>% as.character %>% unique,
  ciat2$Protein2 %>% as.character %>% unique,
  ciat2$Protein3 %>% as.character %>% unique,
  ciat2$Protein4 %>% as.character %>% unique,
  ciat2$Protein5 %>% as.character %>% unique) %>% unique %>% sort

###################################################################################
# Extract climate information on Linux servers
###################################################################################
bioList <- list.files("/mnt/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2", full.names = T)
bioList <- bioList[grep(pattern = "bio", x = bioList)]
bioList <- bioList[grep(pattern = "tif", x = bioList)] %>% mixedsort
grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
bioList <- raster::stack(bioList)

climate_data <- raster::extract(x = bioList, y = ciat_landraces3[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- rownames(ciat_landraces3[,c("Longitude", "Latitude")] %>% as.data.frame %>% na.omit)
row_id <- row_id %>% as.character() %>% as.numeric()

climate_data <- climate_data %>% as.data.frame()
climate_data$Accession <- ciat_landraces3$Accession[row_id]; rm(row_id, grep2)

genotypic_climate <- inner_join(x = ciat_landraces3, y = climate_data, by = "Accession")
saveRDS(genotypic_climate, paste0(root, "/gap_analysis_landraces/Results/_occurrences_datasets/ciat_climate.RDS"))
rm(bioList, climate_data, ciat_landraces3)

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
