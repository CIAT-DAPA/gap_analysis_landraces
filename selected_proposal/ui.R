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
      tabPanel("Introduction",tags$html( tags$body(h1(HTML("<b>How to complete our survey</b> ")),

                                                   #p(style="align=justify","Below you will find a short tutorial to handle drawing and editing tools of the polygons."),
                                                   h3(HTML("<b> 1. Selec your crop from the drop down list and type your full name. Press the save button to advance. </b>")),
                                                  
                                                   div(style="text-align:center",img(src="name_and_croptype.gif",width="800px", height="400px",aling="left")),
                                                  
                                                   h3(HTML("<b>2. Questions</b>")),
                                                   h3(HTML("<p align=justify> This survey has four pages of questions, related to the current distribution of modern crop cultivars, of landraces, 
                                                           of gaps in germplasm collections for landraces, and of areas that are already well represented in genebanks. On each page, please draw your 
                                                           best understanding of the requested information, and add any notes or comments helpful in interpretation.</p> ")),
                                                   
                                                   h3(HTML("<b>2. Drawing tools</b>")),
                                                   h3(HTML("<p align=justify>On  each map the tools are located at top left. You can zoom in the map with + or - buttons in the  draw toolbar or you can use the mouse wheel. </p>")),
                                                   div(style="text-align:center",img(src="maps_tools.PNG",width="800px", height="400px",aling="left")),

                                                   h3(HTML("<p align=justify>Please draw a shape, being sure to complete the shape by reconnecting to the starting point. 
                                                           </P>")),
                                                   div(style="text-align:center",img(src="drawing_polygon.gif",width="800px", height="400px",aling="left")),
                                                   
                                                   h3(HTML("<p align=justify>  You can undo your work as needed. Save your work when you are done.       </p>")),
                                                   
                                                   div(style="text-align:center",img(src="edit_polygon.gif",width="800px", height="400px",aling="left")),
                                                   
                                                   h3(HTML("<b> 4. Final revision </b>")),
                                                   h3(HTML("<p align=justify> On the final page of the survey, you have the opportunity to review your work before finalizing.
                                                           Feel free to return to any section needing further editing </p>")),
                                                   div(style="text-align:center",img(src="review_polygon.gif",width="800px", height="400px",aling="left"))
                                                   
                                                   
                                       
                                                   
                                                   )
                                         
                                         
                                         )
               
               
               ),
      
      tabPanel("Crops and expert name",selectInput("crop","Select crop variety:",c("-- Please select crop --" ,"Potato" ,       "Barley"  ,      "Banana"  ,      "Rice"  ,        "Chickpea" ,     "Common bean" ,  "Wheat (bread)" ,"Wheat (durum)" ,"Cassava"   ,    "Maize" ,       
                                                                       "Yam" ,          "Grass pea" ,    "Lentil"  ,      "Sweet potato" , "Sorghum" ,      "Groundnut" ,    "Cowpea"    ,    "Pea"    ,       "Faba bean"   ,  "Pigeonpea" ,   
                                                                        "Finger millet", "Pearl millet" , "Forages"   ),selected = NULL),textInput('nombre',"Please type your full name"),bsButton("keep",label = " Save",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep", title = "Save name", placement = "right", trigger = "hover")),
      tabPanel("1. Cultivars",box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap5",height = 500,width = "100%")
                                  
                                  
                                  )
      , box(solidHeader = F,width = 4, tagList(div( tags$h3(HTML("<b>1. High yielding cultivars</b>"))),
                                                       tags$h5(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas where only modern varieties 
                                                                    (no landraces) of the crop are cultivated. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                    <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                   B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                    rather than purchasing from the formal seed sector).</p>")),
                                                       textAreaInput("txt1","Write a note or comment:",height = "150px")
                                                  ,div(style="float:middle",bsButton("done", size="large",label = "Save", block = F, style="primary",icon("cloud-download")),
                                                       bsTooltip(id = "done", title = "Save your work", placement = "right", trigger = "hover"))
                                                  ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
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
                                               textAreaInput("txt3","Write a note or comment:",height = "150px")                                                      
                                               ,div(style="float:middle",bsButton("done3",size="large", label = "Save", block = F, style="primary",icon("cloud-download")),
                                                    bsTooltip(id = "done3", title = "Save your work", placement = "right", trigger = "hover"))
                                               ,busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                )   

         )
      ),
     
      tabPanel("4. Already conserved", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap8", height = 500,width = "100%"),div(style = "float:right")),
               
                      box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>4. Areas sufficiently represented</b>"))),
                                                                                                                     
                                                                tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop have already been sufficiently collected and where germplasm is conserved in publicly available genebanks</p>")),
                                                                textAreaInput("txt4","Write a note or comment:",height = "150px")
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




