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
                      position:fixed;
                      top: 40%;
                      left: 20%;
                      margin-top:  auto;
                      margin-left:  auto;
                      display:none;
                      background: rgba(28, 25, 25, .8);
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




header <- dashboardHeader(
  title = "Survey - Crop Landraces Gap Analysis",titleWidth = 400

)

body <- dashboardBody(
  fluidRow( 
    tabBox(
      title = tagList(shiny::icon("bug"), "Questions"),
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",width = "10px", height = "250px",
      tabPanel("Type Name",textInput('nombre',"Please type your full name"),bsButton("keep",label = "Done",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep", title = "Save name", placement = "right", trigger = "hover")),
      tabPanel("1. Cultivars",box(background ="black",solidHeader = TRUE,width = 8,div(style="float:right",bsButton("done", size="large",label = "Done", block = F, style="primary",icon("cloud-download")),
                                                                                                               bsTooltip(id = "done", title = "Save your work", placement = "right", trigger = "hover")),leafletOutput("mymap5",height = 500,width = "100%"),busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200))
      , box(solidHeader = TRUE,width = 4, tagList(div( tags$h1("1. High yielding cultivars")),
                                                                                                          tags$h3(" Draw one or more polygons (of any size) over all areas where only modern varieities (no landraces) of the crop are cultivated")
                        )   
      
             )
      
      ),
      
      tabPanel("2. Landraces", box(background ="black",solidHeader = TRUE,width = 8,div(style="float:right",bsButton("done2",size="large" ,label = "Done", block = F, style="primary",icon("cloud-download")),
                                                                                                               bsTooltip(id = "done2", title = "Save your work", placement = "right", trigger = "hover")),leafletOutput("mymap6", height = 500,width = "100%"),busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
             ), box(solidHeader = TRUE,width = 4, tagList(div( tags$h1("2. Landraces")),
                     
                
                       tags$h3("Draw one or more polygons (of any size) over all areas where landraces of the crop are likely to still be cultivated")
             )   
               
      )
     # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.    
               
               ),
      tabPanel("3. Collecting", box(background ="black",solidHeader = TRUE,width = 8,div(style="float:right",bsButton("done3",size="large", label = "Done", block = F, style="primary",icon("cloud-download")),
                                                                                                               bsTooltip(id = "done3", title = "Save your work", placement = "right", trigger = "hover")),leafletOutput("mymap7", height = 500,width = "100%"),busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
               
                                 ),
                                  
     
     box(solidHeader = TRUE,width = 4, tagList(div( tags$h1("3. Collecting missions for crop landraces")),
                                                                                                  tags$h3("Draw one or more polygons (of any size) over all areas where you would prioritize collecting trips for landraces of the crop, in order to improve their representation in publicly available genebanks")
                                                                                                      )   

         )
      ),
     
      tabPanel("4. Already conserved", box(background ="black",solidHeader = TRUE,width = 8,div(style="float:right",bsButton("done4",size="large" ,label = "Done", block = F, style="primary",icon("cloud-download")),
                                                                                                               bsTooltip(id = "done4", title = "Save your work", placement = "right", trigger = "hover")),leafletOutput("mymap8", height = 500,width = "100%"),busyIndicator(text = "Saving data in progress..",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200),div(style = "float:right")),
               
                      box(solidHeader = TRUE,width = 4, tagList(div( tags$h1("4. Areas sufficiently represented")),
                                                                                                                     tags$h3("Draw one or more polygons (of any size) over all areas where landraces of the crop have already been sufficiently collected and where germplasm is conserved in publicly available genebanks")
                                                                                                                     )   
                      
                        
                      )
               
               
               ),
      

      tabPanel("Results",div(tags$h3("Please review your work and return to relevant pages to revise.")),box(title = "Plot Question 1",status = "primary",solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,plotOutput("plot1")),
               
               box(title = "Plot Question 2",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot2")),
               box(title = "Plot Question 3",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot3")),
               box(title = "Plot Question 4",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,plotOutput("plot4")),
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




