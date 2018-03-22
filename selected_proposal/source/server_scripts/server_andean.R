

observeEvent(input$mydata,{ 
  print(fromJSON(input$mydata) )  
  })

  

  
  observeEvent(input$intro,{
    
    newtab<-switch(input$menu,"intro"="Andean")
    updateTabItems(session,"menu",newtab)
    # updateTabsetPanel(session, "tabset1",
    #                   selected = "Crops and expert name")
    
  })
  
  
  observeEvent(input$close,{
    
    newtab<-switch(input$menu,"Andean"="Mesoamerican")
    updateTabItems(session,"menu",newtab)
    # updateTabsetPanel(session, "tabset1",
    #                   selected = "Crops and expert name")
    
  })
  
  
  observeEvent(input$keep,{
    #input$crop!="-- Please select crop --" && 
    if(nchar(input$nombre)!=0 ){
      updateTabsetPanel(session, "tabset1",
                        selected = "Occurrence")
      
    }else{showModal(modalDialog(
      title = "Oops something went wrong:",
      h3("May be you forgot to select a crop variety or  write a name"),footer = modalButton("OK"),easyClose = TRUE
    ))}
    
    
    
    
  })
  
  
  
  
  
  beanIcon <- makeIcon(
    iconUrl = "https://cdn0.iconfinder.com/data/icons/different-types-of-legumes/32/kidney-beans-512.png",
    iconWidth = 25, iconHeight = 25,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
 
  
  
  
  
  
  ####  PRIMERA PREGUNTAAA----########
  #####=-=-=-=-=-=-=###########
  
  #Create the lefletmap object and add drawToolbar to edit maps
  output$mymap5<-renderLeaflet({
    leaflet("mymap5") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE) 
                       
      )%>% addScaleBar(position="bottomleft")%>%  addSearchOSM() %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                 opacity = 0.5, fillOpacity = 0.5,
                                                                                 fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   %>%addDrawToolbar(
                                                                                   targetGroup= NULL,
                                                                                   polylineOptions = FALSE,
                                                                                   polygonOptions = drawPolygonOptions(repeatMode = TRUE),
                                                                                   circleOptions = FALSE,
                                                                                   rectangleOptions = drawRectangleOptions(repeatMode=TRUE),
                                                                                   markerOptions = FALSE,
                                                                                   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
    
  })
  
  #create a reactives list to store drawn objects in environmental obrserver (critical step)
  featurelist <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list()
  )
  
  
  recorder <- list()
  #set the names of objects where polygons coordinates are stored 
  
  EVT_DRAW <- "mymap5_draw_new_feature"
  EVT_EDIT <- "mymap5_draw_edited_features"
  EVT_DELETE <- "mymap5_draw_deleted_features"
  
  
  #call the objects whit the input[[]] function in a observer environment
  #this function store the drawn polygons in the reactives lists
  observeEvent(input[[EVT_DRAW]], {
    featurelist$drawn <- c(featurelist$drawn, list(input[[EVT_DRAW]]))
    featurelist$finished <- c(featurelist$finished, list(input[[EVT_DRAW]]))
    
  })
  #this function store edited polygons
  observeEvent(input[[EVT_EDIT]], {
    edited <- input[[EVT_EDIT]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist$finished[loc] <<- list(x)
      }
    })
    
    featurelist$edited_all <- c(featurelist$edited_all, list(edited))
    
  })
  
  #this function estore the objects deleted
  observeEvent(input[[EVT_DELETE]], {
    
    deleted <- input[[EVT_DELETE]]
    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    
    ids <- unlist(lapply(featurelist$finished, function(x){x$properties$`_leaflet_id`}))
    
    # now modify finished to match edited
    
    
    feat2delete <- unlist(lapply(deleted$features, function(x){
      return(x$properties$`_leaflet_id`)
    }))
    
    grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
    loc <- grep2(pattern = feat2delete, x = ids, fixed = T)
    
    if(length(loc) > 0) {
      featurelist$finished <<- featurelist$finished[-loc]
    }
    
    
    #print(featurelist$finished)
    # lapply(deleted$features, function(x) {
    #   # loc <- base::match(x$properties$`_leaflet_id`, ids)
    #   loc <- grep(pattern = x$properties$`_leaflet_id`, x = ids, fixed = T)
    #  
    #   if(length(loc) > 0) {
    #     featurelist$finished <<- featurelist$finished[-loc]  
    #   }
    # })
    
    
    featurelist$deleted_all <- c(featurelist$deleted_all, list(deleted))
    
  })
  
 
  
  #when the buttom "done" is clicked this function
  #Create a list containing the objectes drawn, edited and delted  from the leaflet.drawn map
  
  returnlist <- reactive({
    
    workinglist <- list(
      drawn = featurelist$drawn,
      edited = featurelist$edited_all,
      deleted = featurelist$deleted_all,
      finished = featurelist$finished
    )
    
    # convert the lists to simple features files througth functions created above
    
    workinglist <- lapply(
      workinglist,
      function(action) {
        # ignore empty action types to prevent error
        #   handle in the helper functions?
        if(length(action) == 0) { return() }
        
        # FeatureCollection requires special treatment
        #  and we need to extract features
        features <- Reduce(
          function(left,right) {
            if(right$type == "FeatureCollection") {
              right <- lapply(right$features, identity)
            } else {
              right <- list(right)
            }
            c(left,right)
          },
          action,
          init = NULL
        )
        
        
        
        combine_list_of_sf(
          lapply(features, st_as_sf.geo_list)
        )
      }
    )
    
    
    return(workinglist)
    
  })
  
  
  
  
  
  
  
  
  
  observeEvent(input$done,{
    
    
    
    
    saveData_1(returnlist()$finished,gsub(" ", "", input$nombre),gsub(" ", "",input$tabset1),(input$done), gsub(" ", "",input$menu),input$txt1)
    
    if(length(returnlist()$finished)!=0){
      
      updateButton(session, "done",label = " Save",style = "success",icon("check-circle"))
      saveData_1(returnlist()$finished,gsub(" ", "", input$nombre),gsub(" ", "",input$tabset1),(input$done), gsub(" ", "",input$menu),input$txt1)
     
      }
    
  })
  
  observeEvent(input$next1,{
    updateTabsetPanel(session, "tabset1",
                      selected = "2. Landraces")
    
  })
  
  
  observeEvent(input$back1,{
    
    updateTabsetPanel(session, "tabset1",
                      selected = "Occurrence")
    
    
  })
  
  # Then pass the token to each drop_ function
  
  
  
  
  
  
  
  
  
  output$plot1<-renderLeaflet({
    
    if(length(returnlist()$finished)!=0){
      
      leaflet("plot1") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )%>% addPolygons(data=returnlist()$finished, color = "blue", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                         opacity = 0.5, fillOpacity = 0.5) 
      
    }else{leaflet("plot1") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )}
    
    
  })
  
  
  ######SEGUNDA PREGUNTA###########
  
  output$mymap6<-renderLeaflet({
    
    isolate({
      
      if(length(returnlist()$finished)!=0){
        
        
        
        leaflet("mymap6") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stame.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          ) %>%  addPolygons(data=returnlist()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                             opacity = 0.5, fillOpacity = 0.05,
                             fillColor = colorQuantile("Blues", domain=NULL)) %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                         opacity = 0.5, fillOpacity = 0.5,
                                                                                                                                                         fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   %>% addDrawToolbar(
                                                                                                                                                           targetGroup= NULL,
                                                                                                                                                           polylineOptions = FALSE,
                                                                                                                                                           
                                                                                                                                                           polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                                                                                                                                                                                 fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
                                                                                                                                                           circleOptions = FALSE,
                                                                                                                                                           rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                                                                                                                                                                                   fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
                                                                                                                                                           markerOptions = FALSE,
                                                                                                                                                           editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
        
        
        
      }else{
        
        
        
        leaflet("mymap6") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stame.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          )%>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                      opacity = 0.5, fillOpacity = 0.5,
                                                                                      fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))  %>% addDrawToolbar(
                                                                                        targetGroup= NULL,
                                                                                        polylineOptions = FALSE,
                                                                                        
                                                                                        polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                                                                                                              fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
                                                                                        circleOptions = FALSE,
                                                                                        rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "red", weight = 1, opacity = 1,
                                                                                                                                                                                fill = TRUE, fillColor = "red", fillOpacity = 0.4)),
                                                                                        markerOptions = FALSE,
                                                                                        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
        
        
      }
      
      
    })#end isolate
    
    
    
    
  }) 
  
  #To avoid when drawing new polygon, editing a existing polygon and deleting any polygons that being affec the next map
  observeEvent(input$mymap5_draw_new_feature,{
    
    proxy<-leafletProxy("mymap6")
    if(length(returnlist()$finished)!=0){
      proxy %>%  clearShapes()%>% addScaleBar(position="bottomleft")%>%  addSearchOSM() %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                        opacity = 0.5, fillOpacity = 0.5,
                                                                                                        fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>%addPolygons(data = returnlist()$finished,
                                                                                                                                                                                                                                              color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                              opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                              fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                        )
      
    }
    
  })
  observeEvent(input$mymap5_draw_edited_features,{
    proxy<-leafletProxy("mymap6")
    if(length(returnlist()$finished)!=0){
      
      
      proxy %>%  clearShapes()%>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                         opacity = 0.5, fillOpacity = 0.5,
                                                                                                         fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addPolygons(data = returnlist()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                         )
      
    }
    
  })
  
  observeEvent(input$mymap5_draw_deleted_features,{
    proxy<-leafletProxy("mymap6")
    if(length(returnlist()$finished)!=0){
      
      
      proxy %>%  clearShapes()%>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                         opacity = 0.5, fillOpacity = 0.5,
                                                                                                         fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addPolygons(data = returnlist()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                         )
      
    }else{  proxy %>%  clearShapes()  %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                 opacity = 0.5, fillOpacity = 0.5,
                                                                                                                 fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))  }
    
  })
  featurelist2 <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list()
  )
  
  
  
  
  recorder2 <- list()
  
  EVT_DRAW2 <- "mymap6_draw_new_feature"
  EVT_EDIT2 <- "mymap6_draw_edited_features"
  EVT_DELETE2 <- "mymap6_draw_deleted_features"
  
  
  
  
  #call the objects whit the input[[]] function in a observer environment
  #this function store the drawn polygons in the reactives lists
  observeEvent(input[[EVT_DRAW2]], {
    featurelist2$drawn <- c(featurelist2$drawn, list(input[[EVT_DRAW2]]))
    featurelist2$finished <- c(featurelist2$finished, list(input[[EVT_DRAW2]]))
    
  })
  #this function store edited polygons
  observeEvent(input[[EVT_EDIT2]], {
    edited <- input[[EVT_EDIT2]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist2$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist2$finished[loc] <<- list(x)
      }
    })
    
    featurelist2$edited_all <- c(featurelist2$edited_all, list(edited))
    
  })
  
  #this function estore the objects deleted
  observeEvent(input[[EVT_DELETE2]], {
    deleted <- input[[EVT_DELETE2]]
    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist2$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify finished to match edited
    feat2delete <- unlist(lapply(deleted$features, function(x){
      return(x$properties$`_leaflet_id`)
    }))
    
    grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
    loc <- grep2(pattern = feat2delete, x = ids, fixed = T)
    
    if(length(loc) > 0) {
      featurelist2$finished <<- featurelist2$finished[-loc]
    }
    
    featurelist2$deleted_all <- c(featurelist2$deleted_all, list(deleted))
    
  })
  
  
  
  returnlist2 <- reactive({
    
    workinglist <- list(
      drawn = featurelist2$drawn,
      edited = featurelist2$edited_all,
      deleted = featurelist2$deleted_all,
      finished = featurelist2$finished
    )
    
    # convert the lists to simple features files througth functions created above
    
    workinglist <- lapply(
      workinglist,
      function(action) {
        # ignore empty action types to prevent error
        #   handle in the helper functions?
        if(length(action) == 0) { return() }
        
        # FeatureCollection requires special treatment
        #  and we need to extract features
        features <- Reduce(
          function(left,right) {
            if(right$type == "FeatureCollection") {
              right <- lapply(right$features, identity)
            } else {
              right <- list(right)
            }
            c(left,right)
          },
          action,
          init = NULL
        )
        
        
        
        combine_list_of_sf(
          lapply(features, st_as_sf.geo_list)
        )
      }
    )
    
    
    return(workinglist)
    
  })
  
  
  
  
  
  observeEvent(input$done2,{
    
    
    saveData_1(returnlist2()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done2), gsub(" ", "",input$menu),input$txt2)
    
    if(length(returnlist2()$finished)!=0){
      updateButton(session, "done2",label = " Save",style = "success",icon("check-circle"))
      
    }
    
    
  })
  
  observeEvent(input$next2,{
    updateTabsetPanel(session, "tabset1",
                      selected = "3. Collecting")
    
  })
  observeEvent(input$back2,{
    
    updateTabsetPanel(session, "tabset1",
                      selected = "1. Cultivars")
    
  })
  
  
  output$plot2<-renderLeaflet({
    
    if(length(returnlist2()$finished)!=0){
      
      leaflet("plot2") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )%>% addPolygons(data=returnlist2()$finished, color = "red", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                         opacity = 0.5, fillOpacity = 0.5) 
      
    }else{leaflet("plot2") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )}
    
    
  })
  
  ####-----TERCERA PREGUNTA ---------####
  ##-----------------------------------##
  ###                                 ###
  ##                                   ## 
  #-------------------------------------#
  
  output$mymap7<-renderLeaflet({
    isolate({
      if(length(returnlist2()$finished)!=0){
        
        leaflet("mymap7") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stamen.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          )%>% addPolygons(data=returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                           opacity = 0.5, fillOpacity = 0.05,
                           fillColor = colorQuantile("Blues", domain=NULL)) %>% addScaleBar(position="bottomleft")%>%  addSearchOSM() %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                      opacity = 0.5, fillOpacity = 0.5,
                                                                                                                                                      fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   %>% 
          
          
          
          addDrawToolbar(
            targetGroup= NULL,
            polylineOptions = FALSE,
            
            polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                                  fill = TRUE, fillColor = "orange", fillOpacity = 0.4)),
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                                    fill = TRUE, fillColor = "orange", fillOpacity = 0.4)),
            markerOptions = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
        
      }else{leaflet("mymap7") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stamen.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          )%>% addScaleBar(position="bottomleft")%>%  addSearchOSM() %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                     opacity = 0.5, fillOpacity = 0.5,
                                                                                     fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))  %>% 
          addDrawToolbar(
            targetGroup= NULL,
            polylineOptions = FALSE,
            
            polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                                  fill = TRUE, fillColor = "orange", fillOpacity = 0.5)),
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "orange", weight = 1, opacity = 1,
                                                                                                    fill = TRUE, fillColor = "orange", fillOpacity = 0.5)),
            markerOptions = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)} 
    })
    
  })
  
  observeEvent(input$mymap6_draw_new_feature,{
    
    proxy<-leafletProxy("mymap7")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                          opacity = 0.5, fillOpacity = 0.5,
                                                                                                          fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   %>% addPolygons(data = returnlist2()$finished,
                                                                                                                                                                                                                                                   color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                   opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                   fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                          )
      
    }
    
  })
  observeEvent(input$mymap6_draw_edited_features,{
    
    proxy<-leafletProxy("mymap7")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                          opacity = 0.5, fillOpacity = 0.5,
                                                                                                          fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))  %>% addPolygons(data = returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                  opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                  fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                          )
      
    }
    
  })
  
  observeEvent(input$mymap6_draw_deleted_features,{
    
    proxy<-leafletProxy("mymap7")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes()  %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                           opacity = 0.5, fillOpacity = 0.5,
                                                                                                           fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addPolygons(data = returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                  opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                  fillColor = colorQuantile("Blues", domain=NULL))
      
    }else{  proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                opacity = 0.5, fillOpacity = 0.5,
                                                                                                                fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))    }
  })
  
  
  
  
  featurelist3 <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list()
  )
  
  
  recorder3 <- list()
  
  EVT_DRAW3 <- "mymap7_draw_new_feature"
  EVT_EDIT3 <- "mymap7_draw_edited_features"
  EVT_DELETE3 <- "mymap7_draw_deleted_features"
  
  
  #call the objects whit the input[[]] function in a observer environment
  #this function store the drawn polygons in the reactives lists
  observeEvent(input[[EVT_DRAW3]], {
    featurelist3$drawn <- c(featurelist3$drawn, list(input[[EVT_DRAW3]]))
    featurelist3$finished <- c(featurelist3$finished, list(input[[EVT_DRAW3]]))
    
  })
  #this function store edited polygons
  observeEvent(input[[EVT_EDIT3]], {
    edited <- input[[EVT_EDIT3]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist3$finished[loc] <<- list(x)
      }
    })
    
    featurelist3$edited_all <- c(featurelist3$edited_all, list(edited))
    
  })
  
  #this function estore the objects deleted
  observeEvent(input[[EVT_DELETE3]], {
    deleted <- input[[EVT_DELETE3]]
    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify finished to match edited
    feat2delete <- unlist(lapply(deleted$features, function(x){
      return(x$properties$`_leaflet_id`)
    }))
    
    grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
    loc <- grep2(pattern = feat2delete, x = ids, fixed = T)
    
    if(length(loc) > 0) {
      featurelist3$finished <<- featurelist3$finished[-loc]
    }
    
    featurelist3$deleted_all <- c(featurelist3$deleted_all, list(deleted))
    
  })
  
  
  
  returnlist3 <- reactive({
    
    workinglist <- list(
      drawn = featurelist3$drawn,
      edited = featurelist3$edited_all,
      deleted = featurelist3$deleted_all,
      finished = featurelist3$finished
    )
    
    # convert the lists to simple features files througth functions created above
    
    workinglist <- lapply(
      workinglist,
      function(action) {
        # ignore empty action types to prevent error
        #   handle in the helper functions?
        if(length(action) == 0) { return() }
        
        # FeatureCollection requires special treatment
        #  and we need to extract features
        features <- Reduce(
          function(left,right) {
            if(right$type == "FeatureCollection") {
              right <- lapply(right$features, identity)
            } else {
              right <- list(right)
            }
            c(left,right)
          },
          action,
          init = NULL
        )
        
        
        
        combine_list_of_sf(
          lapply(features, st_as_sf.geo_list)
        )
      }
    )
    
    
    return(workinglist)
    
  })
  
  
  
  
  
  
  observeEvent(input$done3,{
    
    
    saveData_1(returnlist3()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done3), gsub(" ", "",input$menu),input$txt3)
    if(length(returnlist3()$finished)!=0){
      updateButton(session, "done3",label = " Save",style = "success",icon("check-circle"))
      
      
    }
    
    
  })
  
  
  observeEvent(input$next3,{
    updateTabsetPanel(session, "tabset1",
                      selected = "4. Already conserved")
    
  })
  observeEvent(input$back3,{
    
    updateTabsetPanel(session, "tabset1",
                      selected = "2. Landraces")
    
  })
  
  
  output$plot3<-renderLeaflet({
    
    if(length(returnlist3()$finished)!=0){
      
      leaflet("plot3") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )%>% addPolygons(data=returnlist3()$finished, color = "orange", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                         opacity = 0.5, fillOpacity = 0.5) 
      
    }else{leaflet("plot3") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )}
    
    
  })
  
  #####-------PREGUNTA 4-----------######
  ####--------##########-----------######
  ###_________#########____________######
  
  output$mymap8<-renderLeaflet({
    isolate({
      
      if(length(returnlist2()$finished)!=0){
        
        leaflet("mymap8") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stamen.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          )%>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                      opacity = 0.5, fillOpacity = 0.5,
                                                                                      fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   %>%  addPolygons(data=returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                fillColor = colorQuantile("Blues", domain=NULL))    %>% 
          
          addDrawToolbar(
            targetGroup= NULL,
            polylineOptions = FALSE,
            
            polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                  fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
            circleOptions = FALSE,
            rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                    fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
            markerOptions = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
        
        
        
      }else{
        
        
        leaflet("mymap8") %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite, #map type or map theme. -default($Stamen.TonerLite)
                           options = providerTileOptions(noWrap = TRUE) 
                           
          )  %>% addScaleBar(position="bottomleft")%>%  addSearchOSM() %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                       opacity = 0.5, fillOpacity = 0.5,
                                                                                       fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addDrawToolbar(
                                                                                         targetGroup= NULL,
                                                                                         polylineOptions = FALSE,
                                                                                         
                                                                                         polygonOptions = drawPolygonOptions(repeatMode = TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                                                                                               fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
                                                                                         circleOptions = FALSE,
                                                                                         rectangleOptions = drawRectangleOptions(repeatMode=TRUE,shapeOptions = drawShapeOptions(color = "green", weight = 1, opacity = 1,
                                                                                                                                                                                 fill = TRUE, fillColor = "green", fillOpacity = 0.4)),
                                                                                         markerOptions = FALSE,
                                                                                         editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>% setView(10,10,zoom=2)
        
        
        
        
        
      }
      
      
      
    })
    
  })
  
  
  observeEvent(input$mymap6_draw_new_feature,{
    
    proxy<-leafletProxy("mymap8")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                          opacity = 0.5, fillOpacity = 0.5,
                                                                                                          fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>%addPolygons(data = returnlist2()$finished,
                                                                                                                                                                                                                                                color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                          )
      
    }
    
  })
  observeEvent(input$mymap6_draw_edited_features,{
    
    proxy<-leafletProxy("mymap8")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                          opacity = 0.5, fillOpacity = 0.5,
                                                                                                          fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addPolygons(data = returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                 opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                 fillColor = colorQuantile("Blues", domain=NULL)
                                                                                                          )
      
    }
    
  })
  
  observeEvent(input$mymap6_draw_deleted_features,{
    proxy<-leafletProxy("mymap8")
    if(length(returnlist2()$finished)!=0){
      
      
      proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                          opacity = 0.5, fillOpacity = 0.5,
                                                                                                          fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F)) %>% addPolygons(data = returnlist2()$finished,color = "darkgray", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                                                                                                                                                 opacity = 0.5, fillOpacity = 0.05,
                                                                                                                                                                                                                                                 fillColor = colorQuantile("Blues", domain=NULL))
    }else{  proxy %>%  clearShapes() %>% addScaleBar(position="bottomleft")%>%  addSearchOSM( ) %>% addPolygons(data=harvest, group="Harvested area" ,stroke = T,color = "#f7ba94", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                                                                                                                opacity = 0.5, fillOpacity = 0.5,
                                                                                                                fillColor = "#dd9e87") %>%  addLayersControl(overlayGroups="Harvested area",options=layersControlOptions(collapsed=F))   }
  })
  
  
  
  
  
  
  
  
  
  featurelist4 <- reactiveValues(
    drawn = list(),
    edited_all = list(),
    deleted_all = list(),
    finished = list()
  )
  
  
  recorder4 <- list()
  
  EVT_DRAW4 <- "mymap8_draw_new_feature"
  EVT_EDIT4 <- "mymap8_draw_edited_features"
  EVT_DELETE4 <- "mymap8_draw_deleted_features"
  
  
  #call the objects whit the input[[]] function in a observer environment
  #this function store the drawn polygons in the reactives lists
  observeEvent(input[[EVT_DRAW4]], {
    featurelist4$drawn <- c(featurelist4$drawn, list(input[[EVT_DRAW4]]))
    featurelist4$finished <- c(featurelist4$finished, list(input[[EVT_DRAW4]]))
    
  })
  #this function store edited polygons
  observeEvent(input[[EVT_EDIT4]], {
    edited <- input[[EVT_EDIT4]]
    # find the edited features and update drawn
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist3$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify drawn to match edited
    lapply(edited$features, function(x) {
      loc <- match(x$properties$`_leaflet_id`, ids)
      if(length(loc) > 0) {
        featurelist4$finished[loc] <<- list(x)
      }
    })
    
    featurelist4$edited_all <- c(featurelist4$edited_all, list(edited))
    
  })
  
  #this function estore the objects deleted
  observeEvent(input[[EVT_DELETE4]], {
    deleted <- input[[EVT_DELETE4]]
    # find the deleted features and update finished
    # start by getting the leaflet ids to do the match
    ids <- unlist(lapply(featurelist4$finished, function(x){x$properties$`_leaflet_id`}))
    # now modify finished to match edited
    feat2delete <- unlist(lapply(deleted$features, function(x){
      return(x$properties$`_leaflet_id`)
    }))
    
    grep2 <- Vectorize(FUN = grep, vectorize.args = "pattern")
    loc <- grep2(pattern = feat2delete, x = ids, fixed = T)
    
    if(length(loc) > 0) {
      featurelist4$finished <<- featurelist4$finished[-loc]
    }
    
    featurelist4$deleted_all <- c(featurelist4$deleted_all, list(deleted))
    
  })
  
  
  
  returnlist4 <- reactive({
    
    workinglist <- list(
      drawn = featurelist4$drawn,
      edited = featurelist4$edited_all,
      deleted = featurelist4$deleted_all,
      finished = featurelist4$finished
    )
    
    # convert the lists to simple features files througth functions created above
    
    workinglist <- lapply(
      workinglist,
      function(action) {
        # ignore empty action types to prevent error
        #   handle in the helper functions?
        if(length(action) == 0) { return() }
        
        # FeatureCollection requires special treatment
        #  and we need to extract features
        features <- Reduce(
          function(left,right) {
            if(right$type == "FeatureCollection") {
              right <- lapply(right$features, identity)
            } else {
              right <- list(right)
            }
            c(left,right)
          },
          action,
          init = NULL
        )
        
        
        
        combine_list_of_sf(
          lapply(features, st_as_sf.geo_list)
        )
      }
    )
    
    
    return(workinglist)
    
  })
  
  
  
  
  
  
  observeEvent(input$done4,{
    
    
    saveData_1(returnlist4()$finished,gsub(" ", "", input$nombre),gsub(" ", "", input$tabset1),(input$done4), gsub(" ", "",input$menu),input$txt4)
    if(length(returnlist4()$finished)!=0){
      updateButton(session, "done4",label = " Save",style = "success",icon("check-circle"))
      
    }
    
    
  })
  
  
  observeEvent(input$next4,{
    updateTabsetPanel(session, "tabset1",
                      selected = "Results")
    
  })
  observeEvent(input$back4,{
    
    updateTabsetPanel(session, "tabset1",
                      selected = "3. Collecting")
    
  })
  
  
  
  observeEvent(input$back5,{
    
    updateTabsetPanel(session, "tabset1",
                      selected = "4. Already conserved")
    
  })
  
  
  # observeEvent(input$close,{
  #   
  #   
  #   # if(length(returnlist()$finished)!=0) savePlots(returnlist()$finished,gsub(" ","",input$nombre),countries2,col="blue",n.quest=1,gsub(" ", "",input$menu))
  #   # if(length(returnlist2()$finished)!=0)savePlots(returnlist2()$finished,gsub(" ","",input$nombre),countries2,col="red",n.quest=2,gsub(" ", "",input$menu))
  #   # if(length(returnlist3()$finished)!=0) savePlots(returnlist3()$finished,gsub(" ","",input$nombre),countries2,col="green",n.quest=3,gsub(" ", "",input$menu))
  #   # if(length(returnlist4()$finished)!=0)savePlots(returnlist4()$finished,gsub(" ","",input$nombre),countries2,col="orange",n.quest=4,gsub(" ", "",input$menu))
  #   
  #   
  #   
  #   
  #   Sys.sleep(2)
  #   showModal(modalDialog(
  #     title = "Important message:",
  #     h3("Thank you very much for your time!"),h3("Your work has already been saved, now you can close the app."),footer = NULL,easyClose = TRUE
  #   ))
  #   Sys.sleep(4)
  #   
  #   session$close()
  #   
  # })
  
  output$plot4<-renderLeaflet({
    
    if(length(returnlist4()$finished)!=0){
      
      leaflet("plot4") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )%>% addPolygons(data=returnlist4()$finished, color = "green", weight = 1, smoothFactor = 0.2,  #adicionar el archivo .shp al mapa y hacer que brillen cuadno son seleccionados
                         opacity = 0.5, fillOpacity = 0.5) 
      
    }else{leaflet("plot4") %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, #map type or map theme. -default($Stamen.TonerLite)
                         options = providerTileOptions(noWrap = TRUE) 
                         
        )}
    
    
  })
  
  
  
  

  


