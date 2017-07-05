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
#library(curl)






header <- dashboardHeader(
  title = "Landraces Survey"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mymap", height = 500)
           )
    ),
    fixedRow(column(width = 10, p(
      class = "text-muted",
      paste("Note: a route number can have several different trips, each",
            "with a different path. Only the most commonly-used path will",
            "be displayed on the map."
      )
    ) ,fixedRow(
      column(2,
             box(width = NULL, status = "warning", 
                  checkboxGroupInput("check_1", 
                                        label = h3("Caribbean Countrys"), 
                                    choices = paste(survey$Country[which(survey$selec=="Caribbean")]),
                                      # choiceNames = lapply(seq_along(survey$Country[which(survey$selec=="Caribbean")]),function(i){cdf<-survey$Country[which(survey$selec=="Caribbean")];return(as.character(cdf[i]))})
                                      
                                       #  ,choiceValues= lapply(seq_along(survey$Country[which(survey$selec=="Caribbean")]),function(i){cdf<-seq_along(survey$Country[which(survey$selec=="Caribbean")]);return(as.numeric(cdf[i]))})
                                    
                                    selected = 1),actionButton('submit',"Submit")
      )),
      column(2,
             box(width = NULL, status = "warning",checkboxGroupInput("check_2", 
                                label = h3("Central America"), 
                                choices = paste(survey$Country[which(survey$selec=="Central America")]),
                                selected = 1))
      )
    ,  column(2, box(width = NULL, status = "warning",checkboxGroupInput("check_3", 
                         label = h3("South Africa"), 
                         choices = paste(survey$Country[which(survey$selec=="South Africa")]),
                         selected = 1))),
    
    column(2,box(width = NULL, status = "warning",checkboxGroupInput("check_4", 
                                label = h3("Eastern Africa"), 
                                choices = paste(survey$Country[which(survey$selec=="Eastern Africa")]),
                                selected = 1))),
    column(2,box(width = NULL,status="warning",tableOutput("text")))
           
              
               
               
           )
    ))
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)