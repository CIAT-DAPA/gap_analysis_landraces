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
suppressMessages(if(!require(shinyBS)){install.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
#suppressMessages(if(!require(plotly)){install.packages('plotly'); library(plotly)} else {library(plotly)})




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
                      padding-left: 30px;
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




header <- dashboardHeader(
  title = "Survey - Crop Landraces Gap Analysis",titleWidth = 400

)
sidebar<-dashboardSidebar(
  sidebarMenu(id="menu",
    menuItem("Bean genepools",tabName = "geneBeans", icon = icon("dashboard"), startExpanded = TRUE,selected=TRUE,badgeColor = "yellow",
    menuSubItem("Introduction",tabName="intro"),
    menuSubItem("Andean beans", tabName = "Andean"),
    menuSubItem("Mesoamerican beans", tabName = "Mesoamerican")
    
    )#End menu item beans genepool
    
  )
  
  
)

body <- dashboardBody( 
  tabItems(
   tabItem(tabName="intro",  
           
           tags$html( tags$body(h1(HTML("<b>How to complete our survey</b>")),
                                
                                h3( HTML( " <p align=justify > The aim of this survey is ask about your knowledge in spatial distribution
                                            of landraces crops in this case common Beans. Then:   </p>" )),
                                
                                #p(style="align=justify","Below you will find a short tutorial to handle drawing and editing tools of the polygons."),
                                h3(HTML("<b> 1.  Type your full name then press the save button to advance. </b>"), 
                                        h3(HTML( "<p align= justify>       The survey begin asking for the Bean genepool Andean. </p>" ))  ),
                                
                                div(style="text-align:center",img(src="name_and_croptype.gif",width="1000px", height="300px",aling="left")),
                                
                                h3(HTML("<b>2. Questions</b>")),
                                h3(HTML("<p align=justify> This survey has five pages of questions, related to:

                                         <br></br> A) Current distribution of landraces in CIAT collection.
                                         <br></br> B) The current distribution of modern crop cultivars. 
                                         <br></br> C) The current distribution of landraces.

                                        <br></br> D) The current distribution of gaps in germplasm collections for landraces.
                                        <br></br> E) The current distribution of areas that are already well represented in genebanks.
                                        <br></br> On each page, please draw your
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
                                                                   Feel free to return to any section needing further editing. </p>")),
                                                           div(style="text-align:center",img(src="review_polygon.gif",width="800px", height="400px",aling="left")),
                                                           
                                                           div(style="position: relative;width:100%",h3(HTML("<font color=white >nothing to see here</font>"))),
                                                           div(style=" border:3px solid #ff0000;align:center",bsButton("intro", size="large",label = HTML("<b><font color=black> Go to survey</font></b>"), block = T, style="primary",icon("hand-o-right")))
                                
                                
                                
                                
                                )
                      
                      
                                )
           
           
           
           ),
    
  tabItem(tabName = "Andean",
 
          source("source/ui_scripts/ui_andean.R", local = TRUE)
  
 
  ),#end tabItem for the first survey
  
  tabItem(tabName = "Mesoamerican",
          
          
          source("source/ui_scripts/ui_meso.R", local = TRUE)
          
  )
  

)#end tabItems



)#end dashboard Body
dashboardPage(
  header,
  sidebar,#dashboardSidebar(disable = F),
  body
)




