## María Victoria Diaz L.
## CIAT 2019

library(readr)
library(jsonlite)

########## Load occurrence data from GBIF #####

setwd(paste0(base_dir, "/input_data/by_crop/", crop, "/databases"))

gbif_occurrence <- read.csv("LUNATUS_GBIF.csv")

#gbif_occurrence <- gbf_db_no_singer_no_wild
gbif_occurrence$status<-NA


########## DB of germplasm and herbarium institutions #####

germplasm <- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/institution_names/G_institutions.txt")
url <- "http://sweetgum.nybg.org/science/api/v1/institutions"; herbarium <- fromJSON(url)$data



cat("Figuring out GBIF  accessions from germplasm institutions", "\n")
cat("..........", "\n")

germplasm$INSTCODE <- as.character(germplasm$INSTCODE);germplasm$ACRONYM <- as.character(germplasm$ACRONYM);germplasm$FULL_NAME <- as.character(germplasm$FULL_NAME)

G <- which(gbif_occurrence$institutionCode %in% germplasm$INSTCODE | 
            gbif_occurrence$institutionID  %in% germplasm$INSTCODE |
            gbif_occurrence$collectionCode  %in% germplasm$INSTCODE |
            gbif_occurrence$institutionCode  %in% germplasm$ACRONYM |
            gbif_occurrence$institutionID  %in% germplasm$ACRONYM |
            gbif_occurrence$collectionCode  %in% germplasm$ACRONYM |
            gbif_occurrence$institutionCode  %in% germplasm$FULL_NAME |
            gbif_occurrence$institutionID  %in% germplasm$FULL_NAME |
            gbif_occurrence$collectionCode  %in% germplasm$FULL_NAME)

gbif_occurrence$status[G] <- "G"



cat("Figuring out GBIF  accessions from herbarium institutions", "\n")
cat("..........", "\n")

herbarium$organization <- as.character(herbarium$organization);herbarium$code <- as.character(herbarium$code)

H <- which(gbif_occurrence$institutionCode %in% herbarium$organization |
           gbif_occurrence$institutionCode %in% herbarium$code | 
           gbif_occurrence$collectionCode %in% herbarium$organization |
           gbif_occurrence$collectionCode %in% herbarium$code |
           gbif_occurrence$institutionID %in% herbarium$organization |
           gbif_occurrence$institutionID %in% herbarium$code)

gbif_occurrence$status[H] <- "H"



cat("Determining G and H accessions according to basisOfRecord variable from GBIF", "\n")
cat("..........", "\n")

gbif_occurrence$basisOfRecord<- as.character(gbif_occurrence$basisOfRecord)

g_living_specimen<- which(gbif_occurrence$basisOfRecord == "LIVING_SPECIMEN" & is.na(gbif_occurrence$status)); gbif_occurrence$status[g_living_specimen] <- "G"

h_no_living_specimen<- which(gbif_occurrence$basisOfRecord != "LIVING_SPECIMEN" & is.na(gbif_occurrence$status)); gbif_occurrence$status[h_no_living_specimen] <- "H"


write.csv(gbif_occurrence,"gbif_completed.csv" )

