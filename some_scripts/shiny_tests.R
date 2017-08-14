title: "ggplotly: various examples"
author: "Carson Sievert"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
social: menu
source_code: embed
---
  
{r setup, include=FALSE}



install.packages("plotly"):library(plotly)
install.packages("maps");library(maps)
install.packages("markdown");library(markdown)
knitr::opts_chunk$set(message = FALSE)
```

Row {data-height=600}
------------------------------------------------------------------------------
  ##double all 'a' or 'b's;  "\" must be escaped, i.e., 'doubled'
gsub("([ab])", "\\1_\\1_", "abc and ABC")

  
  
  ### Unemployment
  
  ```{r}
# This example modifies code from Hadley Wickham (https://gist.github.com/hadley/233134)
# It also uses data from Nathan Yau's flowingdata site (http://flowingdata.com/)
unemp <- read.csv("http://datasets.flowingdata.com/unemployment09.csv")
names(unemp) <- c("id", "state_fips", "county_fips", "name", "year", 
                  "?", "?", "?", "rate")
unemp$county <- tolower(gsub(" County, [A-Z]{2}", "", unemp$name))


unemp$state <- gsub("^.*([A-Z]{2}).*$", "\\1", unemp$name)

county_df <- map_data("county")

map('state', fill = TRUE, col = 1:10)


names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")

county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]

county_df$state_name <- NULL
state_df <- map_data("state")
choropleth <- merge(county_df, unemp, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]
choropleth$rate_d <- cut(choropleth$rate, breaks = c(seq(0, 10, by = 2), 35))

# provide a custom tooltip to plotly with the county name and actual rate
choropleth$text <- with(choropleth, paste0("County: ", name, "Rate: ", rate))
p <- ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rate_d, text = text), 
               colour = alpha("white", 1/2), size = 0.2) + 
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  scale_fill_brewer(palette = "PuRd") + theme_void()
# just show the text aesthetic in the tooltip
ggplotly(p, tooltip = "text")

coor<-read.delim("clipboard",header=TRUE)





library(shiny)
install.packages("leaflet");library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <-bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, actionButton("begin", "Press to start survey"),checkboxGroupInput("check_1", 
                                                        label = h3("Caribbean Countrys"), 
                                                        choices = paste(survey$Country[which(survey$selec=="Caribbean")]),
                                                        selected = 1), checkboxGroupInput("check_2", 
                                                                                          label = h3("Central America"), 
                                                                                        choices = paste(survey$Country[which(survey$selec=="Central America")]),
                                                                                          selected = 1)
  )
)
  

server <- function(input, output, session) {
  survey
  points <- eventReactive(input$recalc, {
    coor[,1:2]
  }  , ignoreNULL = FALSE)
  
   
   
   
  
   
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = points())
    
  })
}



shinyApp(ui, server)

rm(ui,server)
install.packages("RColorBrewer")
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addProviderTiles(providers$Stamen.TonerLite,
                                 options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)





ui <- fluidPage(
  checkboxGroupInput("variable", "Variables to show:",
                     c("individuo" = "2",
                       "Transmission" = "3",
                       "Gears" = "4")),
  tableOutput("data")
)

server <- function(input, output, session) {
  output$data <- renderTable({
    mtcars[c(1,), "mpg", drop = FALSE]
  }, rownames = TRUE)
}

shinyApp(ui, server)



ui <- fluidPage(
  leafletOutput("mymap")
)

server <- shinyServer(function(input, output) {
  
  # produce the basic leaflet map with single marker
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lat = 54.406486, lng = -2.925284)
    
  )
  
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$mymap_click,
               print(input$mymap_click)
  )
  
})


shinyApp(ui, server)



leaflet(countries[countries@plotOrder[1:3],]) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "yellow",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))




color = "#444444", weight = 1, smoothFactor = 0.5,
opacity = 1.0, fillOpacity = 0.5,
fillColor = colorQuantile("yellows", domain=NULL),



colorNumeric("Blues", domain = NULL)
previewColors(colorBin("Blues", domain = NULL, bins = 4), sort(rexp(16)))
previewColors(colorQuantile("Blues", domain = NULL), sort(rexp(16)))









observeEvent(input$mymap_shape_click, {
  click2 <- input$mymap_shape_click
  
  if(is.null(click2)){retrun()}
  RV$Clicks<-c(RV$Clicks,click2$id)
  print(RV$Clicks)
  
  
  proxy <- leafletProxy("mymap")
  
  selected<- countries[countries@data$NAME==click2$id,]
  
  
  proxy %>%
    setView(lng = click2$lng, lat = click2$lat, zoom = input$mymap_zoom) %>%
    addPolygons(data = selected,
                fillColor = "yellow",
                fillOpacity = .95,
                color = "orange",
                opacity = 1,
                weight = 1,
                stroke = t,
                layerId = "Selected")
  
  
})




install.packages("fields")
install.packages("spam")

library(fields)
library(spam)



ica<-read.delim("clipboard",header=T,dec=",")

colores<-colorRampPalette(c("blue","green"))
colo1<-c("blue", "green", "yellow","red","purple")
colo2<-c("purple","red","yellow", "green","blue")
inter<-c(20,28,14,16,19)
cort<-cut(1:100,breaks=c(0,19,36,51,79,100))


nf<-layout(matrix(c(1,2),1,2,byrow=TRUE),c(3,0.5),c(3,1), TRUE)
layout.show(nf)
par(mar=c(4,2,4,1),las=1)

boxplot(ica$ica~sino ica$tempo,col=NULL,ylim=c(5,95),yaxt="n") 
axis(2, at = seq(0,100,by=10), cex.axis=1)#
abline(h=98,col="blue")
text(0.6,97,"Excelente calidad",pos=1,cex=0.7)
abline(h=79,col="green")
text(0.6,79,"Buena calidad",pos=3,cex=0.7)
abline(h=51,col="yellow")
text(0.6,51,"Regular calidad",pos=3,cex=0.7)
abline(h=36,col="red")
text(0.6,36,"Mala calidad",pos=3,cex=0.7)
abline(h=19,col="purple")
text(0.6,19,"Pésima calidad",pos=3,cex=0.7)

z=matrix(seq(0,1,by=0.1),nrow=1)
y=range(ica$ica) 

image(z,y,breaks=c(0,0.2,0.4,0.6,0.8,1),col=colo1,axes=FALSE,xlab="",ylab="")

rowSums(is.na(matrix())==TRUE)










--------------------------------------------------

fName <- 'https://rawgit.com/benbalter/dc-maps/master/maps/ward-2012.geojson'


st_write(countries2,"probe.shp")
json<-geojson_read("C:/Users/acmendez/Downloads/data.geojson", what = "sp")

json2<- rgdal::readOGR(dsn = "C:/Users/acmendez/Downloads/mygeodata", layer='data')

c2<-json@polygons

c2[[c(1,1)]]

plot(json)
c2<-projectRaster(json,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sp::proj4string(countries)
extent(json)<-ert

extent(countries)
access

projectRaster(access, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot()
plot(countries)
plot(json) 
ms_simplify(countries@data[209,])#hacer mas ligera el archivo .shp


c2<- matrix(c( 
  -464.7217 ,45.42930,
  -457.1191 ,47.01023,
  -449.8242 ,43.54855,
  -445.7812, 36.77409,
  -454.8340 ,32.24997,
  -463.3594 ,36.49197,
  -474.5654, 33.87042,
  -474.7852, 41.31082,
  -464.7217, 45.42930),9,2,byrow = T)
lines(c2[,1],c2[,2],type="l",col="red")

lines(-rnorm(100,430,30),rnorm(100,0,50))

---------------

  
  output$mymap4 <- renderLeaflet({
    if( "radius" %in% names(input$mymap_drawnItems_created$properties))
    {
      lng = input$mymap_drawnItems_created$geometry$coordinates[[1]]
      lat = input$mymap_drawnItems_created$geometry$coordinates[[2]]
      radius = input$mymap_drawnItems_created$properties$radius
      myMap() %>% addCircles(lng = lng, lat = lat, radius = radius, color="green")
    }
    else{
      myMap() %>% addGeoJSON(input$mymap_drawnItems_created, color="green")
    }
    
    
  })



ui <- fluidPage(
 
 
    textOutput("text"),leafletOutput("mymap")
  
)

server <- function(input, output, session) {
  

myMap <- reactive({
  return(leaflet() %>%
           addTiles() %>%
           addDrawToolbar())
})

# Define reactive expressions, outputs, etc.
output$mymap <- renderLeaflet({
 myMap()
  
})
out<-outputOptions(output, "mymap", suspendWhenHidden = FALSE)




observeEvent(input$mymap_drawnItems_created, {
  
  
  output$mymap <- renderLeaflet({
   
     if( "radius" %in% names(input$mymap_drawnItems_created$properties))
    {
      lng = input$mymap_drawnItems_created$geometry$coordinates[[1]]
      lat = input$mymap_drawnItems_created$geometry$coordinates[[2]]
      radius = input$mymap_drawnItems_created$properties$radius
      myMap() %>% addCircles(lng = lng, lat = lat, radius = radius, color="green")
    }
    else{
      myMap() %>% addGeoJSON(input$mymap_drawnItems_created, color="green")
    }
  })
  
  print(str(input$mymap_drawnItems_created$geometry$coordinates[[1]]))
  
  })
pr<-eventReactive(input$mymap_drawnItems_created, {
  
  salida<-input$mymap_drawnItems_created
  print(input$mymap_click)
  return(salida)
})
# When the Done button is clicked, return a value
output$text<-renderText(

  #input$mymap_field_draw_features
 #unlist(input$mymap_click)
 unlist(input$mymap_draw_all_features)

  )


}

shinyApp(ui, server)




selected <- selectFeatures(countries2)
mapita<-leaflet()%>% addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                                      options = providerTileOptions(noWrap = TRUE) ) %>% editMap()

                                      
crud <- editMap(mapita)




# do this in GlobalEnv only for example purposes
deleted <- list()
ui <- leafletOutput("leafmap")
server <- function(input, output, session) {
  
  rand_lng <-function(n = 10) rnorm(n, -93.65, .01)
  rand_lat<- function(n = 10) rnorm(n, 42.0285, .01)
  m <- leaflet() %>%
    addTiles() %>%
    addPolygons(rand_lng(4), rand_lat(4), group = 'foo') %>%
    addPolygons(rand_lng(4), rand_lat(4), group = 'foo') %>%
    addDrawToolbar(targetGroup = "foo", editOptions = editToolbarOptions())
  
  output$leafmap <- renderLeaflet({m})
  
  observeEvent(input$leafmap_draw_deleted_features,{
    str(input$leafmap_draw_deleted_features, max.level=2)
    deleted <<- c(
      deleted,
      input$leafmap_draw_deleted_features
    )
  })
}
shinyApp(ui, server)




# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    dataTableOutput("responses"), tags$hr(),
    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    sliderInput("r_num_years", "Number of years using R",
                0, 25, 2, ticks = FALSE),
    bsButton("submit", label = "Click Me", block = F, style="danger")
  ),
  server = function(input, output, session) {
    fields <- c("name", "used_shiny", "r_num_years")
    saveData <- function(data) {
      data <- as.data.frame(t(data))
      if (exists("responses")) {
        responses <<- rbind(responses, data)
      } else {
        responses <<- data
      }
    }
    
    loadData <- function() {
      if (exists("responses")) {
        responses
      }
    }
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData());print("hi mon")
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- renderDataTable({
     # input$submit
      #loadData()
     
      
    })     
  }
)


runApp(list(
  ui = pageWithSidebar(
    headerPanel("Test"),
    sidebarPanel(
      tags$head(tags$style(type="text/css", "
                           #loadmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
                           }
                           ")),
      numericInput('n', 'Number of obs', 100),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),busyIndicator(text = "Calculation in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=1000)
      ),
    mainPanel(plotOutput('plot'))
      ),
  server = function(input, output) {
    output$plot <- renderPlot({ Sys.sleep(2); hist(runif(input$n)) })
  }
  ))




getwd()




busyIndicator <- function(text = "Calculation in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$style(HTML("div.shinysky-busy-indicator {
  position:fixed;
                      top: 40%;
                      left: 40%;
                      margin-top:  auto;
                      margin-left:  auto;
                      display:none;
                      background: rgba(109, 106, 105, .8);
                      text-align: center;
                      padding-top: 20px;
                      padding-left: 30px;
                      padding-bottom: 40px;
                      padding-right: 30px;
                      border-radius: 5px;
}"))
     
    ))
    ,div(class="shinysky-busy-indicator",h2(text),img(src=img,width="95px", height="50px"))
    ,tags$script(sprintf(
      "	setInterval(function(){
  		 	 if ($('html').hasClass('shiny-busy')) {
  		    setTimeout(function() {
  		      if ($('html').hasClass('shiny-busy')) {
  		        $('div.shinysky-busy-indicator').show()
  		      }
  		    }, %d)  		    
  		  } else {
  		    $('div.shinysky-busy-indicator').hide()
  		  }
  		},100)
  		",wait)
    )
  )	
}

busyIndicator(text = "Calculation in progress..",img = "C:/Users/acmendez/Music/ajaxloaderq.gif", wait=1000)



for(i in 1:10000) {
  ifelse(i%%4==0,n<-6,n<-12)
  diga<-paste("Done: ",paste0(sample(letters,n,replace = F),collapse = ""))
 
  cat(diga,"\n","realizado",(i/1000)*100,"%","\n")
  Sys.sleep(1)
}
  
mtcars %>% group_by(am) %>%summarise(.,mean=mean(disp),n=n(),var=var(disp),sd=sqrt(var(disp)))
  
  mtcars %>%select(.,current_vars()[-c(2,8:11)]) %>%summarise_all(.,funs("mean"))
 
  grep("a+", letters,value = T)
  
  grep("a", c("abc", "def", "cba a", "aa"), perl=TRUE, value=TRUE)
  
  x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
  # split x on the letter e
  strsplit(x, "f")
  
  
  
  
  install.packages("dismo")
library(dismo)  
  
file<- read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/genesys-accessions-filtered/geo.csv")
  
file<- file[,c(3,2)]  
plot(countries2)
lines(file,col="red",type="p")

mapita<-leaflet() %>%addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                                      options = providerTileOptions(noWrap = TRUE)) %>% addCircleMarkers(lng=lonzero$longitude,lat=lonzero$latitude)
                                   
mapita
acaule = gbif("solanum", "acaule*", geo=FALSE)


geo<-read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/genesys-accessions-filtered/geo.csv",header = T)
core<-read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/genesys-accessions-filtered/core.csv",header=T)
#name<-read.csv("//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/genesys-accessions-filtered/names.csv",header=T)

#####--------------------------------------------------------------------###########
#####------ DEPURAR LA BASE DE DATOS DE PRESENCIA DE LOS CULTIVOS --------############
#####-------------------------------------------------------------------############

presence<-merge(geo,core,by="genesysId")

#mirar si hay valores perdidos (NA) y eliminarlos
sum(is.na(presence$latitude))
sum(is.na(presence$longitude))
names(presence)

nrow(presence)


#hacer seguimiento a la depuración de la base de datos
mapita<-leaflet() %>%addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                                      options = providerTileOptions(noWrap = TRUE)) %>% addCircleMarkers(lng=presence$longitude,lat=presence$latitude)

mapita
#3acaule = gbif("solanum", "acaule*", geo=FALSE)


#quitar las coordenadas (0,0)
presence<-subset(presence, latitude!=0 & longitude!=0 )


#mirar si las coordenadas (0,lat) estan correctas (concuerde el pais )
dat<-subset(presence,latitude==0 | longitude==0 )[,c("longitude","latitude", "uncertainty","orgCty")]
coordinates(dat)<- ~ longitude + latitude
proj4string(dat) <- proj4string(countries)
gf<-sp::over(dat,countries)
dat$orgCty
cbind(gf,dat$orgCty)

#quitar las coordenadas que caen dentro del oceano


ui <- fluidPage(
  textAreaInput("caption", "Caption", "Data Summary", width = "1000px"),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$caption })
}
shinyApp(ui, server)



se<-data.frame(x=runif(100,0,1),y=c(runif(99,1,2),NA))

se[which(!is.na(se$y)),]
is.na(se$y)
