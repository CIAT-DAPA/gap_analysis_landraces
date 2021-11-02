# Shiny APP for Gap analysis landraces project 
# Dashboar to show results for all crops
#######$$$$$$$$$$$$------$$$$$$$$$$$$$$$$######
#Author: Andres Camilo Mendez Alzate
#######$$$$$$$$$$$$------$$$$$$$$$$$$$$$$######
#Date : March 2020

#suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos");library(rgeos)}else{library(rgeos)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rlang)){install.packages("rlang");library(rlang)}else{library(rlang)})
suppressMessages(if(!require(shinycssloaders)){install.packages("shinycssloaders");library(shinycssloaders)}else{library(shinycssloaders)})
suppressMessages(if(!require(pROC)){install.packages("pROC");library(pROC)}else{library(pROC)})
suppressMessages(if(!require(googleVis)){install.packages("googleVis");library(googleVis)}else{library(googleVis)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(RColorBrewer)){install.packages("RColorBrewer");library(RColorBrewer)}else{library(RColorBrewer)})
suppressMessages(if(!require(collapsibleTree)){install.packages("collapsibleTree");library(collapsibleTree)}else{library(collapsibleTree)})
suppressMessages(if(!require(shinydashboardPlus)){install.packages("shinydashboardPlus");library(shinydashboardPlus)}else{library(shinydashboardPlus)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(shinyWidgets)){install.packages("shinyWidgets");library(shinyWidgets)}else{library(shinyWidgets)})
suppressMessages(if(!require(lubridate)){install.packages("lubridate");library(lubridate)}else{library(lubridate)})
suppressMessages(if(!require(bsplus)){install.packages("bsplus");library(bsplus)}else{library(bsplus)})
suppressMessages(if(!require(stringr)){install.packages("stringr");library(stringr)}else{library(stringr)})
suppressMessages(if(!require(stringi)){install.packages("stringi");library(stringi)}else{library(stringi)})
suppressMessages(if(!require(shinyjs)){install.packages("shinyjs");library(shinyjs)}else{library(shinyjs)})
suppressMessages(if(!require(htmltools)){install.packages("htmltools");library(htmltools)}else{library(htmltools)})
suppressMessages(if(!require(devtools)){install.packages("devtools");library(devtools)}else{library(devtools)})
suppressMessages(if(!require(googleway)){devtools::install_github("SymbolixAU/googleway");library(googleway)}else{library(googleway)})
suppressMessages(if(!require(colorspace)){install.packages("colorspace");library(colorspace)}else{library(colorspace)})
suppressMessages(if(!require(zip)){install.packages("zip");library(zip)}else{library(zip)})
suppressMessages(if(!require(readr)){install.packages("readr");library(readr)}else{library(readr)})
suppressMessages(if(!require(googlePolylines)){install.packages("googlePolylines");library(googlePolylines)}else{library(googlePolylines)})
suppressMessages(if(!require(curl)){install.packages("curl", type="source");library(curl)}else{library(curl)})
suppressMessages(if(!require(RCurl)){install.packages("RCurl", type="source");library(RCurl)}else{library(RCurl)})
suppressMessages(if(!require(RColorBrewer)){install.packages("RColorBrewer");library(RColorBrewer)}else{library(RColorBrewer)})



short_info <- function(input, title, place){
  
  input %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = title, placement = place))
}

source("www/helpers.R")
source("www/google_directions_custom.R")
load("www/results_all_crops.RData")
app_credentials <- readRDS("www/credentials.rds")
shp <- shapefile("www/world_shape_simplified/all_countries_simplified.shp")
ctrs <-   shp@data %>% 
  dplyr::select(ADMIN, ISO_A2, FIPS_10_) %>% 
  dplyr::mutate(ISO_A2 = ifelse(ISO_A2 == "-99", NA_character_, ISO_A2),
                FIPS_10_ = ifelse(FIPS_10_ == "-99", NA_character_, FIPS_10_),
                A2 = case_when(
                  !is.na(ISO_A2)& !is.na(FIPS_10_) ~ ISO_A2,
                  is.na(ISO_A2) & !is.na(FIPS_10_) ~ FIPS_10_,
                  !is.na(ISO_A2)& is.na(FIPS_10_) ~ ISO_A2,
                  TRUE ~ NA_character_
                ) ) %>%
  dplyr::arrange(., ADMIN) %>%
  dplyr::filter(!is.na(A2))

map_key <- app_credentials$map_key  
hc_add_event_series2 <- function (hc, series = "series", event = "click") {
   fun <- paste0("function(e){\n  var seriesinfo = {name: e.point.name }\n  console.log(seriesinfo);\n  window.x = this;\n  if (typeof Shiny != 'undefined') { Shiny.onInputChange(this.chart.renderTo.id + '_' + '", 
                 event, "', seriesinfo); }\n\n}")
 
  
  fun <- JS(fun)
  eventobj <- structure(list(structure(list(structure(list(fun), 
                                                      .Names = event)), .Names = "events")), .Names = series)
  hc$x$hc_opts$plotOptions <- rlist::list.merge(hc$x$hc_opts$plotOptions, 
                                                eventobj)
  hc
}

### Retrieve data from FTP server 

download_sftp_files  <- function(crop_name, user, pass){
  
  ftp_url <- paste0("sftp://", user, ":", pass, "@ftp.ciat.cgiar.org/srv/ftp/acmendez/spatial_lga")
  
  curl_download("https://eu.httpbin.org/get?bar=456", tmp)
  
  file_name  <- paste0("gap_richness_", crop_name, ".rds")
  
  occ_name   <- paste0(crop_name, "_used_occ_shp.rds")
  
  occ_url    <-  paste(ftp_url, crop_name, "databases", occ_name, sep = "/")
  
  file_url <- paste(ftp_url, crop_name, "results", file_name, sep =  "/")   
  
  tmp_gp_file <- paste(tempdir(), file_name, sep = "/")
  gp_rich <- RCurl::getBinaryURL(url = file_url)
  writeBin(object = gp_rich, con = tmp_gp_file )
  
  tmp_occ_file <-  paste(tempdir(), occ_name, sep = "/")
  occ <-  getBinaryURL(url = occ_url)
  writeBin(object = occ, con = tmp_occ_file)
  
  
  r <- readRDS(tmp_gp_file)
  occ_df <- readRDS(tmp_occ_file)
  
  ret <- list(shp = r, occ_shp = occ_df)
  
  return(ret)
}



## defining crops names and their respective icon
icons_names <- list.files("www/crops_icons", pattern = ".png$")
icons_names <- stringr::str_extract(icons_names, "[a-zA-Z]+")
icons_paths <- data.frame(icons_names, paths = list.files("www/crops_icons", pattern = ".png$", full.names = T))

crops_names <- tibble(name = list.dirs("www/crops_data/", full.names = F, recursive = F)) %>% 
  dplyr::mutate(base_name = str_extract(name, "[a-zA-Z]+"),
                base_name = case_when(
                  name == "african_maize" ~ "maize",
                  #name == "common_bean" ~ "common_bean",
                  #name == "pearl_millet" ~  "pearl_millet",
                  #name == "sweet_potato"  ~ "sweet_potato",
                  TRUE ~ base_name
                )) %>%
  dplyr::left_join(., icons_paths, by = c("base_name" = "icons_names") ) %>%  
  dplyr::mutate(paths = stringr::str_replace(paths, "^www/", ""))

crops_names_ck <- read.csv("www/crops_names_CK.csv")
crops_names_group <- crops_names_ck %>% 
  dplyr::select(name, Further.app.needs) %>% 
  drop_na %>% 
  pull(name)
  

crops_names <- crops_names %>% 
  left_join(., crops_names_ck %>% dplyr::select(name, App.name)) %>% 
  dplyr::arrange(App.name) 

# Define server logic required 
server<- function(input, output, session) {
  
  ## definir valores Reactivos
  rv_list     <- shiny::reactiveValues()
  rv_list$occ <- data.frame()
  rv_list$groups_vis <- FALSE
  rv_list$flags <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", ctrs %>% pull(A2) %>% tolower())
  rv_list$countries <- ctrs %>% pull(ADMIN) %>% stringr::str_sort(., decreasing  = F)
  #rv_list$gap_richness_shp <- list()
  
  # reactive list to store collecting mission routes
  route <- reactiveValues()
  route$status <- c()
  route$places <- list()
  route$status_icon <- lapply(1:5, function(i){
    icon("fas fa-times-circle")
  })
  route$ties <- list()
  route$elevation <- list()
  route$Incoords <- list()
  route$url_route <- list()
  route$restore_session <- list(points_inf = list(), crops_inf = list(), places_inf= list(), valid_file = "valid")
  #route$path <- list()
  ##### calculate values for main dashoard page
  
  #- mean coverage for all crops
  avg_coverage <- do.call(rbind,consolidado@crop$crops_coverage) %>% 
    dplyr::mutate(mean = (lower+ upper)/2) %>% 
    dplyr:::select(lower, upper, mean) %>% 
    colMeans(., na.rm = T) %>% 
    round(., 1)
    
  
  #- Total number of occ in each country
   occ_counts <- purrr::reduce(consolidado@crop$accessions_count_per_country, left_join, by = c("ISO2", "country")) %>%
     dplyr::mutate(occ_count = rowSums(select_if(.,is.numeric), na.rm = T)) %>% 
     dplyr::select(country, ISO2, occ_count) 
 #- mean gap area for each country
   area_mean <- purrr::reduce(consolidado@crop$gap_area_country_crop, left_join, by = c("ISO2", "country")) %>% 
     dplyr::select(!matches("quantile")) %>% 
     dplyr::mutate_if(is.numeric, .funs =  function(i){ifelse(i == 0 , NA, i)}) %>% 
     dplyr::mutate(area_mean = rowMeans(select_if(.,is.numeric), na.rm = T),
                   area_mean = round(area_mean)) %>% 
     dplyr::select(country, ISO2, area_mean) 
   
   # consolidated data frame to use in interactive maps
   final_data <- consolidado@combined_results %>% 
      left_join(., occ_counts, by = c("ISO2", "country")) %>% 
      left_join(., area_mean,   by = c("ISO2", "country")) %>% 
      left_join(., consolidado@country_area,   by = c("ISO2", "country")) %>% 
      left_join(., consolidado@coverage_totals,   by = c("ISO2", "country")) %>% 
     dplyr::mutate(area_perc = round((area_mean/area_tot)*100,  2),
                   score = Total/max(Total),
                   covg_total_min = round(1-gap_total_min/sdm_total, 3),
                   covg_total_max = round(1-gap_total_max/sdm_total, 3),
                   covg_total     = round((covg_total_min+covg_total_max/2), 3)) %>% 
     dplyr::filter(Total != 0) %>% 
     drop_na() %>% 
     dplyr::rename("iso-a2"  = ISO2, "gap_count" = Total)
 
##### infoboxes
    
   occ_counts <- tibble(crop  = unlist(consolidado@crop$name),
                        n_occ = consolidado@crop$accessions_count %>% 
                          lapply(., function(i){
                            if(!any(is.na(i))){
                              x <- i %>% pluck(., "source_db_freq") %>% 
                                dplyr::pull(n) %>% 
                                sum(., narm = T)
                            }else{
                              x <- NA
                            }
                            return(x)}) %>% 
                          do.call(rbind, .) ) %>% 
     tidyr::drop_na() 
   
  total_occ <- occ_counts %>%
    dplyr::pull(n_occ) %>% 
    sum(., na.rm = T) %>% 
    prettyNum(., format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ",")
  
  total_country <- consolidado@crop$accessions_count_per_country %>% 
    do.call(rbind, .) %>% 
    dplyr::filter(!is.na(occ_count)) %>% 
    pull(country) %>% 
    unique %>% 
    length()
  
    output$infobox_total_crops <- renderInfoBox({
        infoBox(
            title = "Major crops",value =  paste0("\n",22), icon = icon("fab fa-pagelines"),
            color = "green"
        )
    })
    output$infobox_finished <- renderInfoBox({
        infoBox(
            title = "World Coverage", value = paste0(round(avg_coverage[3]), "%"), 
            subtitle = paste0("(", avg_coverage[1], "-", avg_coverage[2], ")" ),
            icon =  icon("fas fa-warehouse"),
            color = "yellow"
        )
    })
    
    output$infobox_countires <- renderInfoBox({
        infoBox(
            title = "Countries",value =  paste0(total_country), subtitle = " ", icon =  icon("fas fa-globe"),
            color = "red"
        )
    })
    output$infobox_tot_occ <- renderInfoBox({
        infoBox(
            title = "Occurrences",value =  total_occ, subtitle = " ", icon =  icon("fas fa-map-pin"),
            color = "blue"
        )
    })
    
    #mapa de total de gaps por pais
    
    output$hcontainer <- renderHighchart({
        
      #read.csv("D:/OneDrive - CGIAR/Attachments/Desktop/gaps_counts_per_countries.csv", stringsAsFactors = F, header = T)
       

        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = brewer.pal(n+1, "YlOrRd"),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        
        if(!input$change_tree){
          #mapdata <- get_data_from_map(download_map_data("custom/world-palestine-highres"))
          
          hc <- hcmap( "custom/world-palestine-highres", data = final_data,
                      joinBy = c("iso-a2"), 
                      value = "score", 
                      name= "Score",
                      showInLegend = FALSE,
                      nullColor = "#DADADA",
                      states = list(hover = list(color = "#a4edba")),
                      borderColor = "#FAFAFA", borderWidth = 0.1,
                      #dataLabels = list(enabled = F, format = '{point.name}'),
                      tooltip = list(valueDecimals = 2, valuePrefix = " ", valueSuffix = " Pts"),
                      download_map_data = F) %>% 
            hc_colorAxis( stops = stops, min = 0 , max = 1, labels = list(format = "{value}")) 
          # hc_colorAxis( minColor = "#FFFFFF", maxColor = "#434348")
          hc %>% 
            hc_add_event_series2( series = "series", event = "click")
          
        }else{
          final_data %>% 
            dplyr::select(country, score) %>% 
            dplyr::filter(score != 0) %>% 
            hchart(., "treemap", hcaes(x = country, value = score, color = score)) %>% 
            hc_add_event_series2( series = "series",event = "click")
          
        }
     
    })
    
    #create deafalt valueBoxes to  be uppdated after
    output$vBox_1 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Occurrences" , icon = icon("fas fa-map-marker-alt"), color = "purple", width = 3 ) 
    })
    output$vBox_2 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Gap area" , icon = icon("fas fa-chart-area"), color = "blue", width = 3 ) 
    })
    output$vBox_3 <- renderValueBox({
      valueBox(value = paste0(0), subtitle = "Coverage" , icon = icon("fas fa-globe-africa"), color = "green", width = 3 ) 
    })
    
    ##### Event to chaange valus of ValueBboxes due to MAP clicks
    observeEvent(input$hcontainer_click,{
      
      #print(input$hcontainer_click)
      country_name <- as.character(input$hcontainer_click$name)
      
      c_info <- final_data %>% 
        dplyr::mutate(country = tolower(country)) %>% 
        dplyr::filter(country == tolower(country_name))
      
      
      output$c_name <- renderText(country_name)
      output$vBox_1 <- renderValueBox({
        valueBox(value = prettyNum(c_info$occ_count, format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ","),
                 subtitle = "Occurrences",
                 icon = icon("fas fa-map-marker-alt"), 
                 color = "purple", width = 3) 
      })
      output$vBox_2 <- renderValueBox({
        valueBox(value =  paste0(prettyNum(c_info$gap_total_max, format = "fg", big.mark = ".", small.mark  = ".", decimal.mark = ","), " Km²"),
                 subtitle = "Gap area", 
                 icon = icon("fas fa-chart-area"), color = "blue", width = 3 ) 
      })
      output$vBox_3 <- renderValueBox({
        valueBox(value = paste0(c_info$covg_total*100, "%") ,
                 subtitle = "Country coverage" , 
                 icon = icon("fas fa-globe-africa"), color = "green", width = 3) 
      })
      
      
      output$descBlock_txt_1 <-  renderText(paste(c_info$gap_count, "Crops"))
      output$descBlock_txt_2 <-  renderText(paste(round(c_info$score, 2), "Pts") )
      
    })
    
    ####segundo panel de box para mostrar mas resultados
    
    output$hcontainer2 <- renderHighchart({
      
      ## occurrence number per data base source
      df <-  consolidado@crop$accessions_count %>% 
        lapply(., function(i){pluck(i, "source_db_freq")}) %>% 
        do.call(rbind, .) %>% 
        dplyr::mutate(source_db = dplyr::recode(source_db, "VIEWS" = "WIEWS"),
                      source_db = ifelse(grepl("GBIF", source_db), "GBIF", source_db),
                      source_lab = ifelse(!grepl("GBIF|WIEWS|USDA|GENESYS", source_db), "CGIAR", source_db)) %>%  
        dplyr::group_by(source_lab) %>% 
        dplyr::summarise(suma = sum(n, na.rm =T)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(total = sum(suma),
                      freq  = round(suma/total*100, 1),
                      total = NULL,
                      suma = NULL) %>% 
        dplyr::rename(name = source_lab, y = freq)
    #make the graph  
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "pie",
                 polar = TRUE) %>% 
       hc_xAxis(categories = df$name) %>%
        hc_add_series(data = df, name = "Percentage", showInLegend = TRUE,showInLegend = TRUE,innerSize =  '60%',
                      tooltip = list(valueDecimals = 1, valuePrefix = " ", valueSuffix = " %")) %>% 
        hc_plotOptions(pie = list(dataLabels = list(enabled = FALSE)))
      
      
    })
    
    output$hcontainer3 <- renderHighchart({
      
        df <- occ_counts %>% 
          arrange(desc(n_occ)) %>% 
          slice(1:20)
      
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "bar",
                 polar = FALSE) %>% 
        hc_xAxis(categories = df$crop) %>%
        hc_add_series(data = df$n_occ, name = "Occurrences", showInLegend = FALSE,color ="#1DBB50", 
                      tooltip = list(valueDecimals = 0, valuePrefix = " ", valueSuffix = ""),pointWidth = "13")
      
      
    })
    
    
    output$hcontainer4 <- renderHighchart({
      
      df <- do.call(rbind,consolidado@crop$crops_coverage) %>% 
        dplyr::mutate(mean = (lower+ upper)/2,
                      mean = round(mean)) %>% 
        dplyr:::select(mean) %>% 
        tibble::add_column(crop_name = consolidado@crop$name) %>% 
        tidyr::drop_na() %>% 
        dplyr::arrange(desc(mean)) %>% 
        dplyr::slice(1:20)
      
      
      highchart() %>%
        hc_title(text = "",
                 style = list(fontSize = "15px")) %>% 
        hc_chart(type = "bar",
                 polar = FALSE) %>% 
        hc_xAxis(categories = df$crop_name) %>%
        hc_add_series(data = df$mean, name = "Coverage", showInLegend = FALSE,color ="#848483", 
                      tooltip = list(valueDecimals = 0, valuePrefix = " ", valueSuffix = "%"),pointWidth = "13")
      
      
    })
    
    ### diversity tree
    output$dv_tree1 <- renderCollapsibleTree({

      dv_data <- read.csv("www/diversity_tree_template.csv") %>% 
        dplyr::add_row(start = NA, ends = "P. Vulgaris", .before = 1 ) %>% 
        dplyr::mutate(collected = runif(nrow(.), 50, 3000)) 
        
      
      collapsibleTree::collapsibleTreeNetwork(dv_data, attribute = "collected")
      

    })

    ##########################
    ##### google map ########
    ########################
    
    #print coordinates when click on map
    
    observeEvent(input$Gmap_map_click,{
      ev <- input$Gmap_map_click
      df <- paste("Latitude = ", ev$lat, ";", "Longitude = ",  ev$lon)
      output$map_LatLng <- renderText(({ unlist(df)}))
      print(df)
    })
    
    #define values to track number of new inputs created
   
    counter <- reactiveValues(n = 0)
    
    #Track the number of input boxes previously
    prevcount <-reactiveValues(n = 0)
    
    observeEvent(input$add_field, {
      Sys.sleep(1)
      
      google_map_update("Gmap") %>%  
        clear_polylines(layer_id = "route") 
      
      counter$n <- counter$n + 1
      prevcount$n <- counter$n - 1
      })
    
    observeEvent(input$rm_field, {
      Sys.sleep(1.5)
      
      if (counter$n > 0) {
        counter$n <- counter$n - 1 
        prevcount$n <- counter$n + 1
      }
      
      try({
        
        marks <- lapply(1:(counter$n+5), function(i){
          ret <- route$Incoords[[i]]
          return(ret)
        }) %>% 
          bind_rows() %>% 
          mutate(id_c = 1:nrow(.),
                 id_c = as.character(id_c),
                 col = case_when(id_c == as.character(counter$n+5) ~ "red", 
                                 TRUE ~ "blue"
                 ))
        
        google_map_update("Gmap") %>% 
          clear_markers(layer_id = "location") %>% 
          clear_markers(layer_id = "places") %>%
          clear_polylines(layer_id = "route") %>% 
          clear_markers(layer_id = "markers_route") %>% 
          googleway::add_markers(data = marks,
                                 lat = "lat",
                                 lon = "lng",
                                 title = "id_c",
                                 label = "id_c",
                                 colour = "col",
                                 update_map_view = F, 
                                 load_interval = 100,
                                 layer_id = "location")
      })
      
      
      

    })
    
    #crear campos fijos para entrada de datos
    output$fields_fijos <- renderUI({
      lapply(1:5, function(i){
        tagList(
          fluidRow(
            column(1, renderText({paste0(i,".")})),
            column(2, textInput(paste0("Lat", i), label = "Latitude")),
            column(2, textInput(paste0("Lng",i), label = "Longitude")),
            column(3, textInput(paste0("addres",i), label = "Address")),
            column(2, pickerInput(
              inputId = paste0("pick_plac",i),
              label = "Near places",
              options = list(style = "btn-primary"),
              choices = c("Restaurants", "Gas station", "ATM", "Hotels"),
              choicesOpt  = list(icon = c("glyphicon glyphicon-cutlery", "glyphicon glyphicon-scale", "glyphicon glyphicon-usd", "glyphicon glyphicon-bed")),
              multiple = TRUE
            ) ),
            column(1, tags$div(style = "height:23px"),
                   actionButton(paste0("zoom", i), 
                                label = "", 
                                icon = icon("fas fa-search-location"), 
                                class = "zoom_cord" )%>% 
                     bs_embed_tooltip(title = "Zoom coord or address in Map", placement = "right")),
            
            column(1, tags$div(style = "height:30px"), icon("fas fa-times-circle") %>% 
                     bs_embed_tooltip(title = "Latitude and Longitude or address must be provided", 
                                      placement = "right") %>% htmltools::tagAppendAttributes(id = paste0("field_", i) ) )
          )
        )
      })
    })
    
   ### add and remove field recursively
    textboxes <- reactive({
      
      n <- counter$n
      
      if (n > 0) {
        # If the no. of textboxes previously where more than zero, then 
        #save the text inputs in those text boxes 
        if(prevcount$n > 0){
          
          vals_lat <- c()
          vals_lng <- c()
          vals_addres <- c()
          if(prevcount$n > n){
            lesscnt <- n
            isInc <- FALSE
          }else{
            lesscnt <- prevcount$n
            isInc <- TRUE
          }
          for(i in 1:lesscnt){
            i <- i+5
            vals_lat[i] = input[[ paste0("Lat",i)]]
            vals_lng[i] = input[[ paste0("Lng",i)]]
            vals_addres[i] = input[[paste0("addres",i)]]
          }
          if(isInc){
            
            vals_lat <- c(vals_lat, "")
            vals_lng <- c(vals_lng, "")
            vals_addres <- c(vals_addres, "")
          }
          
          lapply(seq_len(n), function(i) {
            i <- i+5
            #route$status[[i]] <- 
            route$status_icon[[i]] <-  icon("fas fa-times-circle")
          tagList(
            fluidRow( 
              column(1, renderText({paste0(i,".")})),
              column(2, textInput(inputId = paste0("Lat", i),label = "Latitude",  value = vals_lat[i])),
              column(2, textInput(inputId = paste0("Lng", i),label = "Longitude", value = vals_lng[i])),
              column(3, textInput(paste0("addres",i), label = "Address", value = vals_addres[i])),
              column(2, pickerInput(
                inputId = paste0("pick_plac",i),
                label = "Near places", 
                options = list(style = "btn-primary"),
                choices = c("Restaurants", "Gas station", "ATM", "Hotels"),
                choicesOpt  = list(icon = c("glyphicon glyphicon-cutlery", "glyphicon glyphicon-scale", "glyphicon glyphicon-usd", "glyphicon glyphicon-bed")),
                multiple = TRUE
              )   ),
              column(1, tags$div(style = "height:23px"),
                     actionButton(paste0("zoom", i), 
                                  label = "", 
                                  icon = icon("fas fa-search-location"), 
                                  class = "zoom_cord" )%>% 
                       bs_embed_tooltip(title = "Zoom coord or address in Map", placement = "right")),
              
              column(1, tags$div(style = "height:30px"), icon("fas fa-times-circle") %>% 
                       bs_embed_tooltip(title = "Latitude and Longitude or address must be provided", 
                                        placement = "right") %>% htmltools::tagAppendAttributes(id = paste0("field_", i) ))
            )
           ) 
          })
          
        }else{
          lapply(seq_len(n), function(i) {
            i <- i+5
            route$status_icon[[i]] <-  icon("fas fa-times-circle")
            fluidRow(
            tagList(
              column(1, renderText({paste0(i,".")})),
              column(2, textInput(inputId = paste0("Lat", i),label = "Latitude",  value = "")),
              column(2, textInput(inputId = paste0("Lng", i),label = "Longitude", value = "")),
              column(3, textInput(paste0("addres",i), label = "Address", value = "")),
              column(2, pickerInput(
                inputId = paste0("pick_plac",i),
                label = "Near places", 
                options = list(style = "btn-primary"),
                choices = c("Restaurants", "Gas station", "ATM", "Hotels"),
                choicesOpt  = list(icon = c("glyphicon glyphicon-cutlery", "glyphicon glyphicon-scale", "glyphicon glyphicon-usd", "glyphicon glyphicon-bed")),
                multiple = TRUE
              )   ),
              column(1, tags$div(style = "height:23px"),
                     actionButton(paste0("zoom", i), 
                                  label = "", 
                                  icon = icon("fas fa-search-location"), 
                                  class = "zoom_cord" )%>% 
                       bs_embed_tooltip(title = "Zoom coord or address in Map", placement = "right")),
              
              column(1, tags$div(style = "height:30px"), icon("fas fa-times-circle") %>% 
                       bs_embed_tooltip(title = "Latitude and Longitude or address must be provided", 
                                        placement = "right") %>% htmltools::tagAppendAttributes(id = paste0("field_", i) ))
            )
          )
          }) 
        }
        
      }
      
    })
    ### render input textboxes when it are being created
    output$fields_more <- renderUI({
      textboxes()
    })
    
  
    observeEvent(input$last_btn,{
      
      ch <- input$last_btn
      pos <- stringr::str_locate(ch, "[0-9]+_")
      id <- substring(ch, pos[1], pos[2]-1)
      id <- as.numeric(id)
     
      if(route$status[id] == "Ok" & !is.na(id)){
       
        route$places[[id]] <- NA
        
        lat <-  as.numeric(input[[paste0("Lat",id)]])
        lng <- as.numeric(input[[paste0("Lng", id)]])
        add <- as.character(input[[paste0("addres", id)]])
    
        places <- input[[paste0("pick_plac", id)]]
       
        if(grepl("#|'|_", add)){
          sendSweetAlert(
            session = session,
            title = "Error...",
            text = "Address field cannot have special characters.",
            type = "error"
          )
          loc <- data.frame(lat = NA, lng = NA)
        }
        
        if(any(is.na(c(lat, lng))) & add != "" & !grepl("#|'|_", add) ){
          
          Sys.sleep(1)
        
          ret <- googleway::google_geocode(address = add,
                               key = map_key,
                               simplify = TRUE)
          
          if(ret$status != "OK"){
            sendSweetAlert(
              session = session,
              title = "Error...",
              text = "Addres cannot be geolocated.",
              type = "error"
            )
            loc <- data.frame(lat = NA, lng = NA)
           
          }else{
            loc <- geocode_coordinates(ret)
            if(nrow(loc) > 1){
              loc <- loc[1, ]
            }
            updateTextInput(session, inputId = paste0("Lat", id), label = "Latitude", value = loc$lat)
            updateTextInput(session, inputId = paste0("Lng", id), label = "Longitude", value = loc$lng)
            
          
          }
          
          # map_url <-  "https://maps.googleapis.com/maps/api/geocode/json?"
          # urlArgs <- c(address = address,  key = map_key)
          # address <- URLencode(add)
          # http<- utils::URLencode(paste0(map_url, "address=",address,"&key=", map_key ), 
          #                  reserved = F)
          # response <- httr::GET(http)
          # text <- httr::content(response, as = "text", encoding = "UTF-8")
          # coords <- jsonlite::fromJSON(text, flatten =T , simplifyMatrix = T)
          
        }else if(!any(is.na(c(lat, lng))) & add == ""){
          loc <- data.frame(lat = lat, 
                            lng = lng)
        }else if(!any(is.na(c(lat, lng))) & add != "" & !grepl("#|'|_", add) ){
          Sys.sleep(1)
          
          ret <- googleway::google_geocode(address = add,
                                           key = map_key,
                                           simplify = TRUE)
          loc <- geocode_coordinates(ret)
          if(nrow(loc) > 1){
            loc <- loc[1, ]
          }
          updateTextInput(session, inputId = paste0("Lat", id), label = "Latitude", value = loc$lat)
          updateTextInput(session, inputId = paste0("Lng", id), label = "Longitude", value = loc$lng)
         
        }
        
        if(!any(is.na(loc))){
          
         
          cords <- data.frame(lat = loc$lat,
                              lon = loc$lng)
          elev <- google_elevation(df_locations = cords,
                                   key = map_key,
                                   simplify = TRUE)
          
          if(elev$status =="OK"){
            route$elevation[[id]] <- data.frame(cords, elevation = elev$results$elevation)
           
          }else{
            route$elevation[[id]] <- data.frame(lat = NA, lon = NA, elevation = NA)
          }
          
       
        }
        route$Incoords[[id]] <- data.frame(loc, address = add)
        
        if(!is.null(places) & !any(is.na(loc))){
         
          
          lugares <- lapply( places , function(i){
            Sys.sleep(1)
            plac <- google_places(location = as.numeric(loc[1,]),
                                  keyword = i,
                                  radius = 2000,
                                  key = map_key)
            
            if(plac$status != "OK"){
              sendSweetAlert(
                session = session,
                title = "Error...",
                text = "No near places were found.",
                type = "error"
              )
              
              res <- data.frame(name = NA,  lat =NA, lng = NA , type = i)
              
            }else{
              res <- data.frame(name = plac$results$name,  geocode_coordinates(plac), type = i)
            }
           
            
          } )
          
          lugares<- bind_rows( lugares)
         
          
          origin <- data.frame(name = 'Stop', lat = loc[,1], lng = loc[,2], type = "Stop")
          to_plot <- bind_rows(origin, lugares)  %>% 
            dplyr::mutate(marker_color = ifelse(type == "Stop", "red", "blue"),
                          over = paste(type, " :", name),
                          marker_icon  = case_when(
                            type == "Restaurants" ~ "http://maps.google.com/mapfiles/ms/icons/coffeehouse.png"  ,
                            type == "Gas station" ~ "http://maps.google.com/mapfiles/ms/icons/gas.png", 
                            type == "ATM" ~ "http://maps.google.com/mapfiles/ms/icons/dollar.png" , 
                            type == "Hotels" ~ "http://maps.google.com/mapfiles/ms/icons/lodging.png",
                            TRUE ~ "http://maps.google.com/mapfiles/ms/icons/red-pushpin.png",
                          )  ) %>% 
            drop_na()
          
          route$places[[id]] <- to_plot %>% 
            dplyr::mutate(id = id)
          
          if(nrow(to_plot) > 1)
          google_map_update("Gmap") %>%  
            clear_markers(layer_id = "location") %>% 
            clear_markers(layer_id = "places") %>%
            clear_polylines(layer_id = "route") %>% 
            clear_markers(layer_id = "markers_route") %>% 
            googleway::add_markers(data = to_plot, 
                                   title = 'type',
                                   mouse_over = "over",
                                   marker_icon = "marker_icon",
                                   update_map_view = F, 
                                   load_interval = 100,
                                   layer_id = "places")
          
          
        }else if(!any(is.na(loc))){
          
          marks <- lapply(1:id, function(i){
            ret <- route$Incoords[[i]]
            return(ret)
          }) %>% 
            bind_rows() %>% 
            mutate(id_c = 1:nrow(.),
                   id_c = as.character(id_c),
                   col = case_when(id_c == as.character(id) ~ "red", 
                                   TRUE ~ "blue"
                                   ))
          
          google_map_update("Gmap") %>% 
            clear_markers(layer_id = "location") %>% 
            clear_markers(layer_id = "places") %>%
            clear_polylines(layer_id = "route") %>% 
            clear_markers(layer_id = "markers_route") %>% 
            googleway::add_markers(data = marks,
                                   lat = "lat",
                                   lon = "lng",
                                   title = "id_c",
                                   label = "id_c",
                                   colour = "col",
                                   update_map_view = F, 
                                   load_interval = 100,
                                   layer_id = "location")
          
         
        }
        
        
       
      }else{
        google_map_update("Gmap") %>% 
          clear_markers() 
      }
      
    })
    
    
    ### dibujar ruta de colecta en base a los puntos ingresados y mostrar estadísticas del viaje
    
    observeEvent(input$get_route,{
      
      k <- counter$n + 5
      points <- bind_rows(route$ties)
      points <- points[1:k, ]
      
      status <- unlist(route$status)
      status <- status[1:k]
      df_route <- data.frame(points, status) %>% 
        dplyr::mutate(status = ifelse((is.na(lat)|is.na(lng)) & status == "Ok", 
                                      "Invalid Latitude and Longitude", 
                                      status))
    
      if(any(df_route$status != "Ok")){
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Lat/Lng misspecified.",
          type = "error"
        )
        
      }else{
        start <-  df_route[1, c("lat", "lng")]
        end  <-   df_route[nrow(df_route), c("lat", "lng")]
        way_pts <- df_route[-c(1, nrow(df_route)),  c("lat", "lng")]
        
        
        polyWaypoints <- encode_pl(way_pts$lat, way_pts$lng)
        polyWaypoints <- list( via = paste0("enc:", polyWaypoints, ":"))
        
        
        
        df <-  google_directions(origin = start,
                          destination = end,
                          mode = "driving",
                          simplify = FALSE, 
                          waypoints = polyWaypoints,
                          #optimise_waypoints = TRUE,
                          key = map_key)
    
       df <-  jsonlite::fromJSON(df, simplifyVector = F)
       
       if(df$status == "OK"){
         
         base_url <- "https://www.google.com/maps/dir/?api=1&"
         url_wpts <- apply(way_pts,1, paste, collapse=",") %>% paste(., collapse = "|") 
         url_route <- paste0(base_url, 
                "origin=", 
                paste(start,collapse = ","), 
                "&destination=",
                paste(end, collapse = ","),
                "&waypoints=",
                url_wpts,
                "&travelmode=driving"
                ) %>% URLencode()
         
         route$url_route <- url_route
         
         res <- get_directions_custom(coords = df)
          
         route$path  <-  res
     
        pl <- data.frame(polyline = res$polyline)
        
        df_route$id <- as.character(1:nrow(df_route) )
        
        google_map_update("Gmap") %>% 
         #google_map(key = map_key) %>% 
          clear_markers(layer_id = "location") %>% 
          clear_markers(layer_id = "places") %>% 
          clear_polylines(layer_id = 'route') %>% 
          clear_markers(layer_id = 'markers_route') %>% 
           add_markers(data = df_route,
                       lat = "lat",
                       lon = "lng",
                       title = "id",
                       label = "id",
                       layer_id = 'markers_route') %>% 
           add_polylines(data = pl, 
                         polyline = "polyline", 
                         stroke_weight = 2,
                         layer_id = "route") 
        
        
       }else{
         sendSweetAlert(
           session = session,
           title = "Error...",
           text = "route cannot be established.",
           type = "error"
         )
       }
    
      }
     
     
      
      output$route_stats <- renderUI({
        tagList(
          span(h3(textOutput("total_distance"), align = "right"), style = "color:#000000"),
          span(h4(textOutput("route_covg"), align = "right"), style = "color:#000000"),
          span(h5(textOutput("total_time"), align = "right"), style = "color:#000000"),
          span(h6(textOutput("mean_altitude"), align = "right"), style = "color:#000000"),
          tags$hr(),
          highchartOutput("distance_graph", height="400px", width="100%"),
          highchartOutput("altitude_graph", height="400px", width="100%"),
          tags$hr(),
          fluidRow(
            verbatimTextOutput("url_text")
            # actionBttn(inputId = "copy_url", style = "bordered",
            #            color = "primary",
            #            label = "Copy URL") %>% 
            #   bs_embed_tooltip(title = "Copy URL to clipboard", placement = "right")
          )
          
        )
      })
   
     
      
    })
    
    ##### graficos de resumen de la ruta
    output$total_distance <- renderText({
      res <-  route$path
      paste(prettyNum( round(sum(res$data$total_dist)/1000), big.mark=",",scientific=FALSE), " km-total distance")
    })
    
    output$total_time <- renderText({
     res <-  route$path
    
     x <-  res$data %>%
        mutate(duration = gsub("\\W", "", duration ),
               duration = ifelse(grepl("hour", duration),
                                 paste0(gsub("[a-zA-Z]+", ":" ,duration),"00" ), 
                                 ifelse(grepl("min|mins", duration),paste0("00:",  gsub("[a-zA-Z]+", ":" ,duration),"00" ), duration)),
               duration = lubridate::hms(duration)) 
    
      suma <- purrr::reduce(x$duration, `+`) %>% 
        lubridate::period_to_seconds(.) %>% 
        lubridate::seconds_to_period(.)
      splitted <- stringr::str_split(suma,pattern = '[A-Z]' ) %>% unlist
      horas <- gsub("\\W", "", splitted[1]) %>% as.numeric
      minutos <- gsub("\\W", "", splitted[2]) %>% as.numeric
      
      if(minutos > 59){
        horas_min <- minutos/60
        horas <- horas+ as.integer(horas_min)
        minutos <- round(horas_min%%1*60)
      }
      
      paste(horas," hours ",minutos, " minutes", " total travel time")
    })
    
    output$mean_altitude <- renderText({
      
      res <- bind_rows(route$elevation) %>% 
        drop_na()
      paste(prettyNum( round(mean(res$elevation,1)), big.mark=",",scientific=FALSE), " mts altitude on average")
      
    })
    
    output$route_covg <- renderText({
      
      req(route$path, rv_list$gap_richness_shp)
      
      
      resul <- route$path
      
      pr <- resul$data %>% 
        dplyr::mutate(id_rt = 1:nrow(.),
                      lat_lng_sf = purrr::map2(.x = lat_lng, .y = id_rt, .f = function(.x, .y){
                        .x %>% 
                          mutate(id_rt = .y )
                        
                      }))
      
      lns <- bind_rows(pr$lat_lng_sf)
      
      route_sf <- lns 
      
      coordinates(route_sf) <- ~lon+lat
      
      pol <- rv_list$gap_richness_shp
      
  
      route_sf <- route_sf %>% 
        sf::st_as_sf()
      
      st_crs(route_sf)  <- st_crs(pol)
      
      print(class(route_sf))
      print(class(pol))
      
      ov <- sf::st_join(route_sf, pol)
      
      
     
      ov_num <- ov %>% 
       dplyr::filter(!is.na(Gaps_layer)) %>% 
       nrow()
      
      tot <- ov %>% 
        nrow()
      
      to_print <- round(ov_num/tot*100,3)
      
     
     
      paste0(format(to_print, nsmall = 2), "% of route are covered by gap areas")
     
      
    })
    
    output$distance_graph <- renderHighchart({
     
      Incoords <- bind_rows(route$Incoords) %>% 
        dplyr::mutate(id = 1:nrow(.)) %>% 
        dplyr::select(id, everything())
      
      df_cords <- Incoords %>% 
        dplyr::select(lat, lng)
      
      res <- google_distance(origins = df_cords  ,
                      destinations =  df_cords , mode = "driving",key = app_credentials$map_key  )

      vals <- res$rows
     
      
      dist_m <- lapply(vals$elements, function(i){
        (round(i[[1]][,2] /1000))
      }) %>% 
        do.call(rbind,.)
      
      dists <- c()
      labs <- c()
      for(i in 1:(nrow(dist_m)-1)){
        dists [i] <- print(dist_m[i, i+1])
        labs[i] <- paste0(i, "-", i+1)
      }
      
      dists <- c( dists)
      labs  <- c( labs)
     
      res_t <- data.frame(seq = 1:(nrow(df_cords)-1),
                          total_dist = dists,
                          c_sum = cumsum(dists),
                          label = labs)
      
      highchart() %>% 
        hc_xAxis(
          categories = res_t$label,
          title = list(text = "Points ID"),
          opposite = FALSE
        ) %>% 
        hc_add_series(
          res_t,
          type = "line",
          hcaes(x = label, 
                y = c_sum),
          name = "Cumulative distance",
          tooltip = list(pointFormat = "Distance btw points ID <b> {point.key} </b>: <b> {point.y} </b>",valueDecimals = 0, valuePrefix = "", valueSuffix = "Km"),
          color = "green"
        ) %>%
        hc_add_series(res_t,
                      type = "line",
                      hcaes(x = label, 
                            y = total_dist),
                      name = "Travel dist bwt. points",
                      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "Km"),
                      color = "orange") %>% 
        hc_title(text = "Travel Distance Graph",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_yAxis(
          title = list(text = "Distance in Km"),
          opposite = FALSE,
          labels = list(format= '{value} km')
        ) %>%  
        hc_add_theme(hc_theme_google ())
    })
    
    
    output$altitude_graph <- renderHighchart({
      
      
      Incoords <- bind_rows(route$Incoords) %>% 
        dplyr::mutate(id = 1:nrow(.)) %>% 
        dplyr::select(id, everything())
      
      df_cords <- Incoords %>% 
        dplyr::select(lat, lon = lng)
      
      elevs <- lapply(1:(nrow(df_cords)-1), function(i){
        df_in <- df_cords[i:(i+1),]
        res <- google_elevation(df_locations = df_in,
                                location_type = "path",
                                samples = 10,
                                simplify = TRUE,
                                key = app_credentials$map_key  )
        res <- res$results %>% 
          mutate(ID = i)
        return(res)
        Sys.sleep(0.2)
      }) %>% 
        dplyr::bind_rows()
     
      df_cords$pos <-c(seq(1, (nrow(df_cords)-1)*10,  by = 10), (nrow(df_cords)-1)*10)
    
      res_t <- elevs %>% 
        dplyr::select(elevation, ID) %>% 
        dplyr::mutate(seq = 1:nrow(.))
     
      highchart() %>% 
      hc_add_series(res_t, 
             type = "line", 
             hcaes(x = seq, 
                   y = elevation),
             name = "Elevation",
             tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "Mts"),
             color = "blue") %>% 
        hc_title(text = "Travel Elevation Graph",
                 style = list(fontWeight = "bold", fontSize = "30px"),
                 align = "center") %>% 
        hc_yAxis(
          title = list(text = "elevation in Mts"),
          opposite = FALSE,
          labels = list(format= '{value} Mts')
         
        ) %>% 
        hc_xAxis(
          title = list(text = "Route travel points"),
          opposite = FALSE,
          plotLines = lapply(1:nrow(df_cords), function(i){
            
            list(label = list(text = paste("Point ID", i)),
                 color = "#FF0000",
                 width =2,
                 value = df_cords$pos[i])
          })
        ) %>%  
        hc_add_theme(hc_theme_google ())
      
      
    })
    
    output$url_text      <- renderText({route$url_route})
    
  #   observeEvent(input$copy_url,{
  #     req(route$url_route)
  #     write.table(route$url_route, "clipboard", row.names = F, col.names = F)
  # }) 
    
    ### monitorear y validar que los campos ingresados esten correctos
   
    
    observe({
     
      n <- counter$n + 5

      lat_vals <- c()
      lng_vals <- c()
      addres_vals <- c()
      icn <- c()
      
     
      for(i in 1:n){

        #req(input[[paste0("Lat", i)]], input[[paste0("Lng", i)]], input[[paste0("addres", i)]])
        if(!any(is.null(c(input[[paste0("Lat", i)]], input[[paste0("Lng", i)]], input[[paste0("addres", i)]])))){

         

          lat_vals[i] <-  input[[paste0("Lat", i)]] %>% as.numeric()
          lng_vals[i] <-  input[[paste0("Lng", i)]] %>% as.numeric()
          addres_vals[i] <-  input[[paste0("addres", i)]] %>% as.character()
          
          route$ties[[i]]<- data.frame(lat = lat_vals[i], lng = lng_vals[i])
          
          route$restore_session$places_inf[[i]] <- data.frame(places = ifelse(is.null(input[[paste0("pick_plac", i)]]), NA, input[[paste0("pick_plac", i)]]), id = i)
          
          route$restore_session$points_inf[[i]] <- data.frame(lat = lat_vals[i], lng = lng_vals[i], add = addres_vals[i], id = i )
          #prepare error messages
          if(any(is.na(c(lat_vals[i], lng_vals[i]))) & addres_vals[i] == "" ){
            route$status[i]<-  "Invalid Latitude and Longitude or address"
            icn[i] <- "fas fa-times-circle"
           
          }else if(!any(is.na(c(lat_vals[i], lng_vals[i]))) & (lat_vals[i] > 80 | lat_vals[i] < -80 | lng_vals[i] > 170 | lng_vals[i] < -170)){
            route$status[i] <-  "Latitude or Longitude out of range [-80 ; 80] or [-170 ; 170]"
            icn[i] <- "fas fa-times-circle"
           
          }else if( !any(is.na(c(lat_vals[i], lng_vals[i]))) & addres_vals[i] != ""){
            route$status[i] <- "Ok" #"Both Lat/Lng and address are specified, only Lat/Lng will be used"
            icn[i] <- "fas fa-check-circle"
           
             # js$change_tit(id = paste0("field_",i), 
             #               new_title =  status[i])
          }else if(!any(is.na(c(lat_vals[i], lng_vals[i]))) & addres_vals[i] == ""){
            route$status[i] <- "Ok"
            icn[i] <- "fas fa-check-circle"
            
           
          }else if(any(is.na(c(lat_vals[i], lng_vals[i]))) & nchar(addres_vals[i]) > 0){
            route$status[i] <- "Ok" 
            icn[i] <- "fas fa-times-circle"
          }
          
          js$change_icon(id = paste0("field_",i), 
                         new_icon =icn[i], 
                         new_title = route$status[i])
         
          #print(paste("field = ", i , "Latitude = ",  lat_vals[i], "Longitude = ", lng_vals[i], "addres = ",  addres_vals[i], "status = ", status[i] ))
          
        }

      }#end for

     if( length(na.omit(lat_vals)) == n & length(na.omit(lng_vals)) == n  ){
       shinyjs::enable("get_route")
     }else{
       shinyjs::disable("get_route")
     }

    })
    
    
    observe(({
      req(input$select_crops2, input$country_picker )
      route$restore_session$crops_inf <- data.frame(selected_crops = input$select_crops2, selected_country = input$country_picker)
      route$restore_session$groups_in <- data.frame(selected_groups = input$select_groups)
    }))
    
    #guardar resultados
    
   output$downl_route <- downloadHandler(
      
      filename = function(){
        paste0("LGA_route_", Sys.Date(), ".zip")
      }, 
      content = function(con){
        
        shp_fpath <- paste0(tempdir(), "\\", "route_polyline.shp")
        Incoords_fpath <- paste0(tempdir(), "/", "Input_coords.csv")
        plac_fpath  <- paste0(tempdir(), "/", "near_places.csv")
        rds_fpath  <- paste0(tempdir(), "/", "restore_session.rds")
        route_url <- paste0(tempdir(), "/", "link_to_google_maps.txt")
        
        url_route <- route$url_route
        
        writeLines(text = url_route, sep = "\t", con = route_url)
        
        resul <- route$path
        
        pr <- resul$data %>% 
          dplyr::mutate(id_rt = 1:nrow(.),
                        lat_lng_sf = purrr::map2(.x = lat_lng, .y = id_rt, .f = function(.x, .y){
                          .x %>% 
                            mutate(id_rt = .y )
                          
                        }))
        
        lns <- bind_rows(pr$lat_lng_sf)
        
        fn <- lns %>% 
          sf::st_as_sf(coords = c("lon","lat")) %>% 
          sf::st_set_crs(., value = '+proj=longlat +datum=WGS84 +no_defs') %>%
          group_by(id_rt) %>%
          summarise(geometry = sf::st_union(geometry)) %>%
          ungroup()
        
        st_write(fn, shp_fpath, delete_dsn = TRUE)
        
        shp_files <- list.files(tempdir(),
                                "route_polyline",
                                full.names = TRUE)
        
     
       
        
        Incoords <- bind_rows(route$Incoords) %>% 
          dplyr::mutate(id = 1:nrow(.)) %>% 
          dplyr::select(id, everything())
        
        elev <-  bind_rows(route$elevation)
        
        Incoords$elevation <- elev
        
        write.csv(Incoords, Incoords_fpath, row.names = F)
        
        plac <- route$places
        if(length(plac[!is.na(plac)]) != 0){
          plac <- plac[!is.na(plac)] %>% 
            bind_rows() %>% 
            dplyr::select(id, name, lat, lng, type, label = over)
          
         
        }else{
          plac <- data.frame(id = NA, name = NA, lat = NA, lng = NA, type = NA, label = NA)
        }
        
        write.csv(plac, plac_fpath, row.names = F)
        saveRDS(Incoords, rds_fpath)
        
        zipr(con, c(shp_files,Incoords_fpath, plac_fpath,  rds_fpath, route_url))
        
        file.remove(shp_files, Incoords_fpath, plac_fpath, rds_fpath, route_url)
        
      }

   )
   
   output$save_session <- downloadHandler(
     filename = function(){
       paste0("LGA_route_restore_file.rds")
     },
     content = function(con){
       
       saveRDS(route$restore_session , con)
     }
     
   )
   
   observeEvent(input$clear_all,{
     n <- counter$n + 5
    
     for(i in 1:n){
       
       updateTextInput(
       session = getDefaultReactiveDomain(),
       inputId = paste0("Lat", i),
       label = "Latitude",
       value = "",
       placeholder = NULL
     )
     
     updateTextInput(
       session = getDefaultReactiveDomain(),
       inputId = paste0("Lng", i),
       label = "Longitude",
       value = "",
       placeholder = NULL
     )
     
     updateTextInput(
       session = getDefaultReactiveDomain(),
       inputId = paste0("addres", i),
       label = "Addres",
       value = "",
       placeholder = NULL
     )
     
     updatePickerInput(
       session,
       inputId = paste0("pick_plac",i),
       label = "Near places",
       choices = c("Restaurants", "Gas station", "ATM", "Hotels"),
       choicesOpt  = list(icon = c("glyphicon glyphicon-cutlery", "glyphicon glyphicon-scale", "glyphicon glyphicon-usd", "glyphicon glyphicon-bed"))
      
     )
     } 
     
     route$status <- c()
     route$places <- list()
     route$status_icon <- lapply(1:5, function(i){
       icon("fas fa-times-circle")
     })
     route$ties <- list()
     route$elevation <- list()
     route$Incoords <- list()
     
     google_map_update("Gmap") %>%  
       clear_markers(layer_id = "location") %>% 
       clear_markers(layer_id = "places") %>% 
       clear_polylines(layer_id = "route") %>% 
       clear_markers(layer_id = "markers_route")
     
   })
   
    output$Gmap <- renderGoogle_map({
      google_map(key = map_key,
                 location = c(0,0),
                 zoom = 2,
                 search_box = T,
                 event_return_type = "list")
    })
    
    
    
    observeEvent(input$add_to_map ,{
      
      if(is.null(input$select_crops2)){
        shinyWidgets::sendSweetAlert(session, title = "Error", 
                                     text = "No crop has been selected from the list",
                                     type = "error")
      }else{
        
        
        updateMaterialSwitch(session, "hide_layer", value = FALSE)
        
        withBusyIndicatorServer("add_to_map",{
          
          crops <- input$select_crops2
          try(expr = {
            
            if(length(crops) == 1 & rv_list$groups_vis & !is.null(input$select_groups)){
              
              groups <- input$select_groups
              
              occ_file <- readRDS(paste0("www/crops_data/", crops, "/groups/used_occ_shp_groups.rds"))
              
              dwnloaded <- lapply(groups, function(k){
                dest_file_rich_groups <- paste0("www/crops_data/", crops, "/groups/gap_richness_group_", k, ".rds")
                
                occ_file <- occ_file %>% 
                  dplyr::filter(ensemble == k)
                
                ret <- list(shp = readRDS(dest_file_rich_groups), occ_shp = occ_file)
                return(ret)
              })
              
              rv_list$map_layer_labels <- groups
              
             
            }else{
              
              rv_list$map_layer_labels <- crops
              
              dwnloaded <- lapply(crops, function(i){
                
                crop_name <- i
                dest_file_shp <- paste0("www/crops_data/",crop_name,"/", crop_name, "_used_occ_shp.rds")
                dest_file_gp <- paste0('www/crops_data/', crop_name, "/gap_richness_", crop_name, ".rds")
                
               
                dest_file_shp <-  paste0("www/crops_data/",crop_name,"/", crop_name, "_used_occ_shp.rds")
                
                
                if(file.exists(dest_file_shp) & file.exists(dest_file_gp)){
                  
                  occ_df <- readRDS(dest_file_shp)
                  r <- readRDS(dest_file_gp)
                  
                  ret <- list(shp = r, occ_shp = occ_df)
                  
                }else{
                  safeError("Download failed !!!")
                  ret <- NULL
                }
                
                return(ret)
                # download_sftp_files(crop_name = i, 
                #                     user = app_credentials$sftp_user, 
                #                     pass = app_credentials$sftp_pass )
              })
            }
            
            
          })
          if(exists("dwnloaded") ){
            
         
            pol <- lapply(dwnloaded, function(i){
              if("Spatial" %in% is(i$shp)){
                
                pol <- i$shp
                names(pol) <- "value"
               
              }else{
                pol <- NA
              }
              return(pol)
            }) %>% do.call(rbind,.)
            
            labels_nms <- rv_list$map_layer_labels
            
            pol@data <- data.frame(value = 1:(nrow(pol@data)))
            print(labels_nms)
            pol <- sf::st_as_sf(pol) %>% 
              st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>% 
              dplyr::mutate(Gaps_layer = labels_nms)
        
          
            occ <- lapply(dwnloaded, function(i){
              #if("Spatial" %in% is(i$occ_shp)){
              pol <- i$occ_shp
              #names(pol) <- "value"
              #}else{
                #pol <- NA
              #}
              return(pol)
            }) %>% bind_rows(.) 
              #do.call(rbind,.)
             if("list" %in% class(occ)){
               occ <- occ[[1]]
             }
            
            occ_pol <- sf::st_as_sf(occ, coords = c("Longitude", "Latitude")) %>% 
              st_set_crs("+proj=longlat +datum=WGS84 +no_defs") 
            
         
            if(!is.null(input$country_picker)){
              
              country_name <- input$country_picker
              
              country_shp <- shp[shp@data$ADMIN == country_name,]
              country_shp <- sf::st_as_sf(country_shp) %>% 
                st_set_crs("+proj=longlat +datum=WGS84 +no_defs")
             
              occ_pol <- sf::st_intersection(occ_pol, country_shp)
              pol  <- sf::st_intersection(pol, country_shp)
             
              if(rv_list$groups_vis & !is.null(input$select_groups)){
                
                
                occ_marks <- data.frame(Group =  occ_pol$ensemble,
                                        Group_html = paste(tags$strong("Landracre Group: "), occ_pol$ensemble),
                                        Longitude = st_coordinates(occ_pol)[,1],
                                        Latitude = st_coordinates(occ_pol)[,2],
                                        m_icon = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
                )
              }else{
                occ_marks <- data.frame(Group =  occ_pol$org,
                                        Group_html = paste(tags$strong("Type of accession: "), occ_pol$org),
                                        Longitude = st_coordinates(occ_pol)[,1],
                                        Latitude = st_coordinates(occ_pol)[,2],
                                        m_icon = "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
                )
                
              }
              
              
            }
            

           
            rv_list$occ <- occ_marks
            rv_list$gap_richness_shp <- pol
            
          }
          
        
          
        })#end bussy indicator
        
      }#end else
      
      output$selected_crops <- renderText({ paste("Viewing:", paste(crops, collapse =  ","))})
      updateMaterialSwitch(session, "hide_layer", value = TRUE)
    })
    
    
    ##### make selectable menu to select crops for google maps 
    
    output$multiselect1 <- renderUI({
     
      shinyWidgets::multiInput(
        inputId = "select_crops2",
        label = "Select Major crops :", 
        choices = NULL,
        width = "100%",
        choiceNames = lapply(seq_along(1:nrow(crops_names)), 
                             function(i) htmltools::tagList(tags$img(src = crops_names$paths[i],
                                                          width = 30, 
                                                          height = 20), crops_names$App.name[i] )),
        choiceValues = crops_names$name)# %>%  
        #short_info(input = ., place = "top",title = "Select maximun three crops to download data.")

      
    })
    
    
    output$multiselect2 <- renderUI({
      req(input$select_crops2)
      crop <- input$select_crops2
      
      if(length(crop) == 1 ){
        
        if( crop %in% crops_names_group ){
          
          rv_list$groups_vis <- TRUE
          
          rv_list$dest_file_gp_groups <- list.files(paste0('www/crops_data/', crop, "/groups/"), pattern = "gap_richness_group", full.names = T)
          
          rv_list$groups_names <- gsub("([A-Za-z0-9_/]+)(group_)|(.rds)", "", rv_list$dest_file_gp_groups)
          
          
          shinyWidgets::multiInput(
            inputId = "select_groups",
            label = "Select Landrace Groups :", 
            choices = NULL,
            width = "100%",
            choiceNames = rv_list$groups_names,
            choiceValues = rv_list$groups_names)
        }
        
        
      }
      
      
    })
  
    
    observeEvent(input$hide_layer,{
      
      req(rv_list$gap_richness_shp)
      
      
      crops <- rv_list$map_layer_labels
      pol <- rv_list$gap_richness_shp
      occ_pol <- rv_list$occ
      # occ_pol <- occ_pol %>% 
      #   dplyr::mutate(Occurrence_layer = ifelse(value == 1, "Herbarium", "Germplasm"))
      
      
      if(length(crops) > 1){
        pal_colors <- brewer.pal(length(crops), "Set1")
        pal <- colorRampPalette(colors = pal_colors)
      }else{
        pal <- colorRampPalette(colors = c("#F90B0B"))
      }
      
      pal2 <- colorRampPalette(colors = c( "#070707", "#726E6E"))
      
    
      
      if(input$hide_layer){
        
        
        if(nrow(occ_pol) == 0 & nrow(pol) == 0){
          shinyWidgets::sendSweetAlert(session, title = "warning", 
                                       text = "There are Not occurences and gaps area in selected country",
                                       type = "warning")
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
            clear_markers(layer_id = "occ")
          
        }else if(nrow(occ_pol) == 0 & nrow(pol) != 0){
          
          shinyWidgets::sendSweetAlert(session, title = "warning", 
                                       text = "There are not occurences in selected country",
                                       type = "warning")
          
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
            clear_markers(layer_id = "occ") %>% 
            add_polygons(pol,
                         #stroke_colour = "Gaps_layer",
                         stroke_weight = "2",
                         stroke_opacity = "0",
                         fill_colour = "Gaps_layer",
                         fill_opacity = "0.3",
                         update_map_view = F, 
                         legend = T, 
                         palette = pal,
                         layer_id = "pols")
        }else if(nrow(pol) == 0 & nrow(occ_pol) != 0){
          shinyWidgets::sendSweetAlert(session, title = "warning", 
                                       text = "There are not gaps area in selected country",
                                       type = "warning")
          
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
            clear_markers(layer_id = "occ") %>%
            add_markers(
              data = occ_pol,
              lat = "Latitude",
              lon = "Longitude",
              #colour= "col",
              marker_icon = "m_icon",
              mouse_over = "Group_html",
              #info_window = "Group_html",
              close_info_window = T,
              update_map_view = F,
              #label = "Group",
              focus_layer = F,
              layer_id = "occ"
            )
            # add_polygons(occ_pol,
            #              #stroke_colour = "Occurrence_layer",
            #              stroke_weight = "2",
            #              stroke_opacity = "0",
            #              fill_colour = "Occurrence_layer",
            #              fill_opacity = "0.7",
            #              update_map_view = F, 
            #              legend = T, 
            #              pal = pal2,
            #              layer_id = "occ") 
        }else{
          
          google_map_update("Gmap") %>% 
            #google_map(key = map_key) %>%
            clear_polygons( layer_id    = "pols") %>% 
            clear_markers(layer_id = "occ") %>% 
            add_polygons(pol,
                         #stroke_colour = "Gaps_layer",
                         stroke_weight = "2",
                         stroke_opacity = "0",
                         fill_colour = "Gaps_layer",
                         fill_opacity = "0.3",
                         update_map_view = F, 
                         legend = T, 
                         palette = pal,
                         layer_id = "pols") %>%
            add_markers( data = occ_pol,
              lat = "Latitude",
              lon = "Longitude",
              #colour= "col",
              marker_icon = "m_icon",
              mouse_over = "Group_html",
              update_map_view = F,
              #info_window = "Group_html",
              close_info_window = T,
              #label = "Group",
              focus_layer = F,
              layer_id = "occ"
            )
            # add_polygons(occ_pol,
            #              #stroke_colour = "Occurrence_layer",
            #              stroke_weight = "2",
            #              stroke_opacity = "0",
            #              fill_colour = "Occurrence_layer",
            #              fill_opacity = "0.7",
            #              update_map_view = F, 
            #              legend = T, 
            #              pal = pal2,
            #              layer_id = "occ") 
          
        }
      }else{
        google_map_update("Gmap") %>% 
          #google_map(key = map_key) %>%
          clear_polygons( layer_id    = "pols") %>% 
          clear_markers( layer_id    = "occ")
      }#end else hide layer  
      
     
      #js$geocodeAddr()
    })
    
  
    output$country_selector <- renderUI({
      
     
      countries<- rv_list$countries
      flags <- rv_list$flags
      pickerInput(
        inputId = "country_picker",
        label = "Select one country:", 
        choices = countries,
        choicesOpt = list(content = lapply(seq_along(countries), 
                                           function(i) {HTML(paste(tags$img(src = flags[i],
                                                                            width = 20, 
                                                                            height = 15), countries[i]))})),
        options = list(
          `live-search` = TRUE),
        multiple = FALSE
        
      )
      
    })
     
    
   ### RESTORES SESSION 
    
     
    output$restoreSession <- renderMenu({
      msgs2 <- list(notificationItem(
        text = "Restore session",
        icon = icon("fas fa-undo-alt") 
      ) %>%
        tagAppendAttributes(., id = "restore"))
      
      dropdownMenu(type = "notifications", .list = msgs2, icon = icon("fas fa-user-cog") )
    })
   
    shinyjs::onclick("restore", expr = function(){
     
      output$modal1 <- renderUI({
        showModal(modalDialog(
          title = tags$strong("Restore Session"),
          tags$div(
            tags$div(style = "float:left;", tags$img(src = 'restore-icon.jpg', eigth = "45px", width = "45px")),
            tags$div(style = "margin-left: 50px;",tags$h5("
            Did your last Session closed unexpectedly or would you like to continue from you left? You can restore the variables and results from a previous session, or start a
            new session."))
          ),
          tags$hr(),
          fileInput("restorePath", "Select  Rsession.rds file:"),
          actionBttn(
            inputId = "accept_restore",
            label = "Restore",
            style = "jelly", 
            color = "primary"),
          easyClose = FALSE,
          footer = modalButton("Ok")
        ))
      })
    })
    
    
    
    observeEvent(input$accept_restore,{
     tryCatch({
       
       rsession_path <- input$restorePath$datapath
       
       if(nchar(rsession_path != 0)){
         
         rsd <- readRDS(rsession_path)
         print(rsd)
         #check if the rds file was generated by this app
         if(!is.null(rsd$valid_file)){
           
           points <- rsd$points_inf %>% 
             bind_rows %>% 
             filter(!(is.na(lat) & is.na(lng) & add == ""))
           
           places <- rsd$places_inf %>% 
             bind_rows() %>% 
             filter(!is.na(places))
           
           id_places <- places %>% 
             pull(id) %>% 
             unique()
           #content to restore here
           n <- nrow(points)
  
           counter$n <-  ifelse(n <= 5, 0, n-5)
           
           Sys.sleep(0.3)
           
           updatePickerInput(session, 
                             inputId = "country_picker", 
                             selected = rsd$crops_inf %>% pull(selected_country) %>% unique() )
           
           updateMultiInput(session,
             inputId =  "select_crops2",
             selected = rsd$crops_inf %>% pull(selected_crops),
             choices = NULL
           )
           
           Sys.sleep(0.5)
           
           updateMultiInput(session,
                            inputId = "select_groups",
                            selected = rsd$groups_in %>%  dplyr::pull(selected_groups),
                            choices = NULL)
          
          
           for(i in 1:n ){
             
             #input[[paste0("pick_plac", id)]]
             if(i %in% places$id){
               updatePickerInput(session, inputId = paste0("pick_plac", i), selected = places$places)
             }
             
             
             updateTextInput(session, inputId = paste0("Lat", i), value =  ifelse(is.na(points$lat[i]), "", as.character(points$lat[i])))
             updateTextInput(session, inputId = paste0("Lng", i), value =  ifelse(is.na(points$lng[i]), "", as.character(points$lng[i])) )
             updateTextInput(session, inputId = paste0("addres", i), value =  ifelse(is.na(points$add[i]), "", as.character(points$add[i])))
             
           }
           
           
         }else{
           safeError("Wrong .RDS file selected.")
         }
         
         
         print("Reactive values were uploaded.")
         
       }else{
         safeError("Not a valid file detected.")
       }
       
       
     },
              error = function(e){
                sendSweetAlert(
                  session = session,
                  title = "Error !!",
                  text = paste("Your Session could not be restored.  \n", e),
                  type = "error"
                )
              })
    })
    
    
}## END server
