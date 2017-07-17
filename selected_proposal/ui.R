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
suppressMessages(if(!require(shinysky)){devtools::install_github("AnalytixWare/ShinySky",force=TRUE);library(shinysky)}else{library(shinysky)})





header <- dashboardHeader(
  title = "Landraces Survey"

)

body <- dashboardBody(
  fluidRow( 
    tabBox(
      title = tagList(shiny::icon("bug"), "Questions"),
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",width = "10px", height = "250px",
      tabPanel("Type Name",textInput('nombre',"Please type your full name")),
      tabPanel("1. Question ",leafletOutput("mymap5",height = 500,width = "100%"),bsButton("done", label = "Done", block = F, style="primary",icon("cloud-download")),
      bsTooltip(id = "done", title = "Save drawns objects", placement = "right", trigger = "hover"),busyIndicator(text = "Saving data in progress..",img = "shinysky/busyIndicator/ajaxloaderq.gif", wait=500)),
      
      #actionButton('done',"Done",icon("cloud-download"),width=200,class="btn action-button btn-primary btn-lg ",style = "primary")
      #bsActionButton("done", label = "Click Me", block = F, style="danger")
      
      tabPanel("2. Question", leafletOutput("mymap6", height = 500,width = "100%"),bsButton("done2", label = "Done", block = F, style="primary",icon("cloud-download")),
               bsTooltip(id = "done2", title = "Save drawns objects", placement = "right", trigger = "hover"),busyIndicator(text = "Saving data in progress..",img = "shinysky/busyIndicator/ajaxloaderq.gif", wait=500)),
      tabPanel("3. Question", leafletOutput("mymap7", height = 500,width = "100%"),bsButton("done3", label = "Done", block = F, style="primary",icon("cloud-download")),
               bsTooltip(id = "done3", title = "Save drawns objects", placement = "right", trigger = "hover"),busyIndicator(text = "Saving data in progress..",img = "shinysky/busyIndicator/ajaxloaderq.gif", wait=500)),
      tabPanel("4. Question", leafletOutput("mymap8", height = 500,width = "100%"),bsButton("done4", label = "Done", block = F, style="primary",icon("cloud-download")),
               bsTooltip(id = "done4", title = "Save drawns objects", placement = "right", trigger = "hover"),busyIndicator(text = "Saving data in progress..",img = "shinysky/busyIndicator/ajaxloaderq.gif", wait=500),div(style = "float:right",bsButton("close",label="Go out",style = "warning",icon("sign-out")),
                                                                                                                  
                                                                                                                  bsTooltip(id = "close", title = "Close all", placement = "bottom", trigger = "hover"))) 
                                                                                                                 
    )
    
 
  
  
  
  )
  
  
 
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)