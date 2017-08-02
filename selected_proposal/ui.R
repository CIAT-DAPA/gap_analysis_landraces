#Shiny application for collecting experts infromation
#Created and edited by Andres camilo mendez Alzate
#All rigth reserved
#We reserve the right of admission
#
#
#
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(shinyBS)){inastall.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})




busyIndicator <- function(text = "Calculation in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=1000) {
  
 
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
                      padding-left: 30px;
                      padding-bottom: 40px;
                      padding-right: 30px;
                      border-radius: 5px;
                      h2{color:red};
}"))
      
      ))
    ,div(class="shinysky-busy-indicator",h2(HTML("<font color=white>Saving data in progress...</font>")),img(src=img,width="95px", height="50px"))
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



header <- dashboardHeader(
  title = "Survey - Crop Landraces Gap Analysis",titleWidth = 400

)

body <- dashboardBody(
  fluidRow( 
    tabBox(
      title = tagList(shiny::icon("bug"), "Questions"),
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",width = "10px", height = "250px",
      tabPanel("Introduction",tags$html( tags$body(h1(HTML("<b>Introduction</b>")),
                                                   p("Here is a little introduction created by julian"),
                                                   h2(HTML("<b>How to conduct the survey</b> ")),
                                                   p(style="align=justify","Below you will find a short tutorial to handle drawing and editing tools of the polygons."),
                                                   h3(HTML("<b> 1.Selec the crop variety from the drop down list and type your full name, finally press the save button to advance. </b>")),
                                                  
                                                   div(style="text-align:center",img(src="name_and_croptype.gif",width="800px", height="400px",aling="left")),
                                                   h3(HTML("<b>2. work area of the questions</b>")),
                                                   h3(HTML("<p align=justify>As you can see the environment of first (or all) question  has three objects the first one is a map in which you must draw the polygons,
                                                      the second object is the statement of the questions and the third object is a text box where you can put notes or comments.</p>")),
                                                   
                                                   
                                                   h3(HTML("<b> 3. Drawing tools. </b>")),
                                                   h3(" , 
                                                      on this map you will find a bar tools located at top left."),
                                                   div(style="text-align:center",img(src="maps_tools.PNG",width="800px", height="400px",aling="left")),
                                                   h3(HTML("<p align=justify>You can zoom in the map with + or - buttons in the  draw toolbar or you can use the mouse wheel.</P>")),
                                                   h3("The draw polygons tool allows you d")
                                                   
                                                   
                                       
                                                   
                                                   )
                                         
                                         
                                         )
               
               
               ),
      
      tabPanel("Crops variety and expert name",selectInput("crop","Select crop variety:",c("NULL" ,"Potato" ,       "Barley"  ,      "Banana"  ,      "Rice"  ,        "Chickpea" ,     "Common bean" ,  "Wheat (bread)" ,"Wheat (durum)" ,"Cassava"   ,    "Maize" ,       
                                                                       "Yam" ,          "Grass pea" ,    "Lentil"  ,      "Sweet potato" , "Sorghum" ,      "Groundnut" ,    "Cowpea"    ,    "Pea"    ,       "Faba bean"   ,  "Pigeonpea" ,   
                                                                        "Finger millet", "Pearl millet" , "Forages"   ),selected = NULL),textInput('nombre',"Please type your full name"),bsButton("keep",label = " Continue",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep", title = "Save name", placement = "right", trigger = "hover")),
      tabPanel("1. Cultivars",box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap5",height = 500,width = "100%")
                                  
                                  
                                  )
      , box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>1. High yielding cultivars</b>"))),
                                                       tags$h4(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas where only modern varieities (no landraces) of the crop are cultivated.</p>")),
                                                       textAreaInput("txt1","Write a note or comment:",height = "200px")
                                                  ,div(style="float:middle",bsButton("done", size="large",label = "Save", block = F, style="primary",icon("cloud-download")),
                                                       bsTooltip(id = "done", title = "Save your work", placement = "right", trigger = "hover"))
                                                  ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                  )
      
             )
      
      ),
      
      tabPanel("2. Landraces", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap6", height = 500,width = "100%")
             ), box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>2. Landraces</b>"))),
                     
                
                       tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop are likely to still be cultivated</p>")),
                       textAreaInput("txt2","Write a note or comment:",height = "200px")
                       ,div(style="float:center",bsButton("done2",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                            bsTooltip(id = "done2", title = "Save your work", placement = "right", trigger = "hover"))
                       ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
             )   
               
      )
     # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.    
               
               ),
      tabPanel("3. Collecting", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap7", height = 500,width = "100%")
               
                                 ),
                                  
     
     box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>3. Collecting missions for crop landraces</b>"))),
                                                                                                  
                                               
                                               
                                               tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where you would prioritize collecting trips for landraces of the crop, in order to improve their representation in publicly available genebanks</p>")),
                                               textAreaInput("txt3","Write a note or comment:",height = "200px")                                                      
                                               ,div(style="float:middle",bsButton("done3",size="large", label = "Save", block = F, style="primary",icon("cloud-download")),
                                                    bsTooltip(id = "done3", title = "Save your work", placement = "right", trigger = "hover"))
                                               ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                )   

         )
      ),
     
      tabPanel("4. Already conserved", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap8", height = 500,width = "100%"),div(style = "float:right")),
               
                      box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>4. Areas sufficiently represented</b>"))),
                                                                                                                     
                                                                tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop have already been sufficiently collected and where germplasm is conserved in publicly available genebanks</p>")),
                                                                textAreaInput("txt4","Write a note or comment:",height = "200px")
                                                                ,div(style="float:middle",bsButton("done4",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                     bsTooltip(id = "done4", title = "Save your work", placement = "right", trigger = "hover"))                                                     
                                                                ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                                
                                                                )   
                      
                        
                      )
               
               
               ),
      

      tabPanel("Results",div(tags$h3("Please review your work and return to relevant pages to revise.")),
               
               box(title = "Plot Cultivars",status = "primary",solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,plotOutput("plot1")),
               
               box(title = "Plot Landraces",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot2")),
               box(title = "Plot Collecting",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot3")),
               box(title = "Plot Already conserved",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot4")),
               bsButton("close",size="large",label="Finish",style = "danger",icon("sign-out")),
               
               bsTooltip(id = "close", title = "Close all", placement = "bottom", trigger = "hover"),
               busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
               
               #,box(title = "debuggin",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,tableOutput("debug"))
               
               
               
               
               
               )                                                                                                           
    )
    
 
  
  
  
  )
  
  
 
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)




