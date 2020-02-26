#*****************************************************************************
########  script to classify germplasm and herbarium institutes from GBIF data
######## Autor: ANDRES CAMILO MENDEZ
####### This script comes with absolutely no warranty, feel free to redistribute it

pacman::p_load(stringr, stringi,tidyverse, purrr, stringdist)

match_herbairums <- function(text, h_inst){
 
  if(!all(c("organization", "code") %in% names(h_inst))){
    stop("colnames not found in h_inst")
  }
if(!is.na(text)){ 
  if(grepl("herba", text)){
    vec <- TRUE
  } else if(nchar(text) <= 6){
    vec <- text %in% h_inst$organization | text %in% h_inst$code
  }else if(grepl("(\\|)", text)){
    check <- str_split(text, "\\|") %>%  unlist()
    if(any(nchar(check) > 6)){
      h_code <-  sub("(.{3}).", "\\1", check) 
      vec <- h_code %in% h_inst$code | h_code %in% h_inst$organization
    }else{
      vec <- check %in% h_inst$code | check %in% h_inst$organization
    }
    
  }else if(grepl("\\s", text)){
    simd_1 <- na.omit(stringsim(a = text, b = h_inst$organization))  >= 0.85
    simd_2 <- na.omit(stringsim(a = text, b = h_inst$code)) >= 0.85
    vec <- any(simd_1) | any(simd_2)
  }else{
    vec <- text %in% h_inst$organization | text %in% h_inst$code
  }
}else{
  vec <- FALSE
}
  
  
  return(any(vec))
  
}

match_germplasm <- function(text, g_inst){
  
  if(!all(c("ACRONYM", "FULL_NAME", "INSTCODE") %in% names(g_inst))){
    stop("colnames not found in g_inst")
  }
if(!is.na(text)){  
  if(nchar(text) <= 6){
    vec <- text %in% g_inst$INSTCODE | text %in% g_inst$ACRONYM |text %in% g_inst$FULL_NAME
  }else if(grepl("(\\|)", text)){
    check <- str_split(text, "\\|") %>%  unlist()
    if(any(nchar(check) > 6)){
      h_code <-  sub("(.{3}).", "\\1", check) 
      vec <- any(h_code %in% g_inst$INSTCODE | h_code %in% g_inst$ACRONYM | h_code %in% g_inst$FULL_NAME)
    }else{
      vec <- any(check %in% g_inst$INSTCODE | check %in% g_inst$ACRONYM | check %in% g_inst$FULL_NAME)
    }
    
  }else if(grepl("\\s", text)){
    simd_1 <- na.omit(stringsim(a = text, b = g_inst$ACRONYM))  >= 0.85
    simd_2 <- na.omit(stringsim(a = text, b = g_inst$FULL_NAME)) >= 0.85
    vec <- any(simd_1) | any(simd_2)
  }else{
    vec <- text %in% g_inst$INSTCODE | text %in% g_inst$FULL_NAME | text %in% g_inst$FULL_NAME
  }
}else{
  vec <- FALSE
}  
  return(any(vec))
  
}
#match_herbairums(text = "8", h_inst = h_inst)

#match_germplasm(text = "institut de recherche gronomique", g_inst = g_inst)


h_inst <- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/institution_names/H_institutions.csv",header = T) %>% 
  dplyr::select(organization, code) %>% 
  dplyr::mutate_all(., .funs = iconv, to = "ASCII//TRANSLIT")%>% 
  dplyr::mutate_all(., tolower) %>% 
  dplyr::mutate_all(., .funs = function(i)ifelse(i=="", NA, as.character(i))) %>% 
  as_tibble() 


g_inst <- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/runs/input_data/institution_names/G_institutions.csv", header = T) %>% 
  dplyr::select(INSTCODE, ACRONYM, FULL_NAME) %>% 
  dplyr::mutate_all(., .funs = iconv, to= "ASCII//TRANSLIT") %>%
  dplyr::mutate_all(., tolower) %>%
  dplyr::mutate_all(., .funs = function(i)ifelse(i=="", NA, as.character(i))) %>% 
  as_tibble()

pth <- ""
gbif_data <- read_csv(pth)

to_exclude <- c("africarice",
                "singer",
                "wild", 
                "USDA", 
                "united states department of agriculture",  
                "system wide information network on genetic resources",
                "the system-wide information network for genetic resources") %>%
  paste0(., collapse = "|")


gbif_data_cleaned <- gbif_data %>% 
  mutate(database_id = paste0("gbif_", 1:nrow(.))) %>% 
  dplyr::filter(!basisOfRecord %in% c("MATERIAL_SAMPLE", "FOSSIL_SPECIMEN") ) %>%
  dplyr::mutate_if(., is.character, tolower) %>% 
  dplyr::mutate_all(.funs = function(i){ifelse(grepl(to_exclude, i), "to_remove", i)}) %>% 
  dplyr::filter_all(all_vars(!grepl("to_remove", .))) %>% 
  dplyr::filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%  
  dplyr::filter((decimalLatitude %% 1) != 0, (decimalLatitude*10) %% 1 != 0) %>% 
  dplyr::filter((decimalLongitude %% 1) != 0, ((decimalLongitude*10) %% 1) != 0) 


status <- gbif_data_cleaned %>%
  dplyr::select(institutionID, institutionCode, collectionCode, basisOfRecord) %>% 
  dplyr::mutate_all(.funs = iconv, to= "ASCII//TRANSLIT" ) %>% 
  dplyr::mutate_at(c("institutionID", "institutionCode", "collectionCode"), .funs = tolower ) %>%
  dplyr::mutate(status_h = NA, status_g  = NA) %>% 
  dplyr::mutate(status_h = purrr::pmap(.l = list(a = institutionID, b = institutionCode, c = collectionCode), 
                                       function(a,b,c){
                                         res <- match_herbairums(text = a, h_inst = h_inst)|match_herbairums(text = b, h_inst = h_inst)| match_herbairums(text = c, h_inst = h_inst)
                                         return(res)
                                       } ) %>% unlist(),
                status_g = purrr::pmap(.l = list(a = institutionID, b = institutionCode, c = collectionCode), 
                                       function(a,b,c){
                                         res <- match_germplasm(text = a, g_inst = g_inst)|match_germplasm(text = b, g_inst = g_inst)| match_germplasm(text = c, g_inst = g_inst)
                                         return(res)
                                       } ) %>% unlist()) %>% 
  dplyr::mutate(status_h = ifelse(status_h, "H", NA_character_),
                status_g = ifelse(status_g, "G", NA_character_),
                status   = case_when(
                  !is.na(status_h) &  is.na(status_g) ~ "H",
                  is.na(status_h) &  !is.na(status_g) ~ "G",
                  !is.na(status_h) & !is.na(status_g) ~ "G" ,
                  is.na(status_h) &  is.na(status_g) ~ NA_character_
                )) %>% pull(status)



gbif_data_cleaned$status <- status
  
output_path <- ""
write.csv(x = gbif_data_cleaned, file = output_path, row.names = F)  



  