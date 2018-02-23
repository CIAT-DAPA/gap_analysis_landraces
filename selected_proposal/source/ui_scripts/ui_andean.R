busyIndicator <- function(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=1000) {
  
  
  tagList(
    singleton(tags$head(
      tags$style(HTML("div.shinysky-busy-indicator {
                      background-color: rgba(28, 25, 25, .8);
                      position:fixed;
                      top: 40%;
                      left: 20%;
                      margin-top:  auto;
                      margin-left:  auto;
                      display:none;
                      text-align: center;
                      padding-top: 20px;
                      padding-left: 40px;
                      padding-bottom: 40px;
                      padding-right: 30px;
                      border-radius: 5px;
                      h2{color:red};
}"))
      
      ))
    ,div(class="shinysky-busy-indicator",h2(HTML("<font color=white>Wait a moment please...</font>")),img(src=img,width="95px", height="50px"))
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



fluidRow( 
            tabBox(
              title = tagList(shiny::icon("bug"), "Andean Bean"),
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1",width = "10px", height = "250px"
           
              
              
              ,tabPanel("Crops and expert name"  #,selectInput("crop","Select crop variety:", "Andean" ,selected = "Andean")
                                                ,textInput('nombre',"Please type your full name")
                       ,bsButton("keep",label = " Save",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep", title = "Save name", placement = "right", trigger = "hover"))
              
              ,tabPanel("Occurrence",   box(solidHeader = TRUE, height = 600,width = 12, tagList(tags$head( 
                            HTML( '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.2/leaflet.css" />' ) 
                          
                          , HTML( '<script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.2/leaflet-src.js" ></script> ' )
                          , HTML('<script src = "https://rawgit.com/Wildhoney/Leaflet.FreeDraw/master/dist/leaflet-freedraw.web.js" ></script>')
                          ,HTML('<style> * {
  padding: 0;
  margin: 0;
  box-sizing: border-box;
}

.map {
  width: 100vw;
  height: 100vh;
}

.map.mode-create {
    cursor: crosshair;
}

.leaflet-edge {
    background-color: #95bc59;
    box-shadow: 0 0 0 2px white, 0 0 10px rgba(0, 0, 0, .35);
    border-radius: 50%;
    cursor: move;
    outline: none;
    transition: background-color .25s;
}

.leaflet-polygon {
    fill: #b4cd8a;
    stroke: #50622b;
    stroke-width: 2;
    fill-opacity: .75;
} </style>')
                          ) 
                          , tags$body( 

                                        div( id="map",style="width:95%; height:600px; color:#0000FF; background-color: black" ,tags$script( type="text/javascript", charset="UTF-8", '  

 var map;

map = new L.Map("map");
                                                                                                                     
var osmUrl="https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}@2x.png";
                                                                                                                               
var osm = new L.TileLayer(osmUrl);		
                                                                                                                             
map.setView(new L.LatLng(51.3, 0.7),9);
map.addLayer(osm);
                                                                                                                                
                                                                                                              
const freeDraw = new FreeDraw({mode: FreeDraw.ALL});
map.addLayer(freeDraw);


 freeDraw.on("markers", function(event){
        
      var str = JSON.stringify(event.latLngs);
       Shiny.onInputChange("mydata", str);
   
}  );
                                                                                                                                           
                                                                                                                                            
                                                                                                                                            
                                               ' ) ) 
                                       
                                       
                                       ,tags$button(id="freedraw",
                                                    type="button",
                                                    class="btn action-button btn-large btn-primary",
                                                    HTML('<i class="icon-star"></i>Large button')
                                                    )
                                       )
                                                             
                                                                    )     )
                       , box(solidHeader = F,width = 4, tagList(div( tags$h3(HTML("<b>1. Current distribution of landraces </b>"))),
                                                                tags$h5(HTML("<p align=justify>  This map plots the distribution of accessions labled as landraces within the CIAT collection. 
                                                                               In the background of this map, the pink color shows the area where common bean is produced, according to Mapspam. 
                                                                               Please revise this map, and help us to identify any points that seem wrong. Draw a polygon around any dubious points.
                                                                                <br/> <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                    <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                    B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                    rather than purchasing from the formal seed sector).
                                                                             </p>")),
                                                                textAreaInput("txt0","Write a note or comment:",height = "150px")
                                                                ,div(style="position:relative;width: 100%",bsButton("done0", size="large",label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                     bsTooltip(id = "done0", title = "Save your work", placement = "right", trigger = "hover")
                                                                     
                                                                     ,div(style="float:right;width:95px",bsButton("next0", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                          ,bsTooltip(id = "next0", title = "Go to next question", placement = "left", trigger = "hover")
                                                                          
                                                                          
                                                                          )
                                                                )
                                                                
                                                                
                                                                ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                                
                                                                )   )
                       
              )
              
              
              ,tabPanel("1. Cultivars",box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap5",height = 500,width = "100%")
                                           
                                           
              )
              , box(solidHeader = F,width = 4, tagList(div( tags$h3(HTML("<b>1. High yielding cultivars</b>"))),
                                                       tags$h5(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas where only modern varieties 
                                                                    (no landraces) of the crop are cultivated. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                    <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                    B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                    rather than purchasing from the formal seed sector).</p>")),
                                                       textAreaInput("txt1","Write a note or comment:",height = "150px")
                                                       ,div(style="position:relative;width: 100%",bsButton("done", size="large",label = "Save", block = F, style="primary",icon("cloud-download")),
                                                            bsTooltip(id = "done", title = "Save your work", placement = "right", trigger = "hover")
                                                            
                                                            ,div(style="float:right;width:95px",bsButton("next1", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                 ,bsTooltip(id = "next1", title = "Go to next question", placement = "left", trigger = "hover")
                                                                 ,bsButton("back1", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                 ,bsTooltip(id = "back1", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                 
                                                                 )
                                                       )
                                                       
                                                       
                                                       ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                       
              )
              
              )
              
              ),
              
              tabPanel("2. Landraces", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap6", height = 500,width = "100%")
              ), box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>2. Landraces</b>"))),
                                                           
                                                           
                                                           tags$h5(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop are likely to still be cultivated. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                        <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                        B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                        rather than purchasing from the formal seed sector).</p></p>")),
                                                           textAreaInput("txt2","Write a note or comment:",height = "150px")
                                                           ,div(style="position:relative",bsButton("done2",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                bsTooltip(id = "done2", title = "Save your work", placement = "right", trigger = "hover")
                                                                
                                                                
                                                                
                                                                
                                                                ,div(style="float:right;width:95px"
                                                                     ,bsButton("next2", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                     ,bsTooltip(id = "next2", title = "Go to next question", placement = "left", trigger = "hover")
                                                                     
                                                                     ,bsButton("back2", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                     ,bsTooltip(id = "back2", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                     
                                                                )
                                                                
                                                                
                                                                
                                                           )  
                                                           
                                                           ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
              )   
              
              )
              # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.    
              
              ),
              tabPanel("3. Collecting", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap7", height = 500,width = "100%")
                                            
              ),
              
              
              box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>3. Collecting missions for crop landraces</b>"))),
                                                        
                                                        
                                                        
                                                        tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where you would prioritize collecting trips for landraces of the crop, 
                                                                      in order to improve their representation in publicly available genebanks. </p>")),
                                                        textAreaInput("txt3","Write a note or comment:",height = "150px")                                                      
                                                        ,div(style="position:relative",bsButton("done3",size="large", label = "Save", block = F, style="primary",icon("cloud-download")),
                                                             bsTooltip(id = "done3", title = "Save your work", placement = "right", trigger = "hover")
                                                             
                                                             ,div(style="float:right;width:95px"
                                                                  ,bsButton("next3", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                  ,bsTooltip(id = "next3", title = "Go to next question", placement = "left", trigger = "hover")
                                                                  
                                                                  ,bsButton("back3", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                  ,bsTooltip(id = "back3", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                  
                                                             )
                                                             
                                                             
                                                             
                                                             
                                                        )
                                                        ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
              )   
              
              )
              ),
              
              tabPanel("4. Already conserved", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap8", height = 500,width = "100%"),div(style = "float:right")),
                       
                       box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>4. Areas sufficiently represented</b>"))),
                                                                 
                                                                 tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop have already been sufficiently collected 
                                                                              and where germplasm is conserved in publicly available genebanks.</p>")),
                                                                 textAreaInput("txt4","Write a note or comment:",height = "150px")
                                                                 ,div(style="position:relative",bsButton("done4",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                      bsTooltip(id = "done4", title = "Save your work", placement = "right", trigger = "hover")
                                                                      
                                                                      ,div(style="float:right;width:95px"
                                                                           ,bsButton("next4", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                           ,bsTooltip(id = "next4", title = "Go to next question", placement = "left", trigger = "hover")
                                                                           
                                                                           ,bsButton("back4", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                           ,bsTooltip(id = "back4", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                           
                                                                      )
                                                                      
                                                                      
                                                                      ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                                      
                                                                 )                                                     
                                                                 
                                                                 
                       )   
                       
                       
                       )
                       
                       
              ),
              
              
              tabPanel("Results",div(tags$h3("Please review your work and return to relevant pages to revise.")),
                       
                       box(title = "Plot Cultivars",status = "primary",solidHeader = TRUE, collapsible = T, collapsed =T, leafletOutput("plot1", height = 500,width = "100%")),
                       
                       box(title = "Plot Landraces",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot2", height = 500,width = "100%")),
                       box(title = "Plot Collecting",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot3", height = 500,width = "100%")),
                       box(title = "Plot Already conserved",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot4", height = 500,width = "100%"))
                       
            
                       ,div(style="float:right;width:95px",bsButton("close",size="large",label="Next",style = "danger",icon("sign-out")),
                            
                            bsTooltip(id = "close", title = "Go to next survey", placement = "bottom", trigger = "hover")   )
                       ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                       
                       ,bsButton("back5", size="large",label = "Back", block = F, style="warning",icon("backward"))
                       ,bsTooltip(id = "back5", title = "Return to previous question", placement = "left", trigger = "hover")
                       
                       
                       
                       #,box(title = "debuggin",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,tableOutput("debug"))
                       
                       
                       
                       
                       
              )                                                                                                           
            )
            
            
            
            
            
  )

