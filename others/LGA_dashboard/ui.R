#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://rinterface.com/shiny/shinydashboardPlus/
#https://adminlte.io/themes/AdminLTE/index2.html

#suppressMessages(if(!require(pacman)){install.packages("pacman");library(pacman)}else{library(pacman)})
suppressMessages(if(!require(raster)){install.packages("raster");library(raster)}else{library(raster)})
suppressMessages(if(!require(rgdal)){install.packages("rgdal");library(rgdal)}else{library(rgdal)})
suppressMessages(if(!require(rgeos)){install.packages("rgeos");library(rgeos)}else{library(rgeos)})
suppressMessages(if(!require(sp)){install.packages("sp");library(sp)}else{library(sp)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}else{library(tidyverse)})
suppressMessages(if(!require(rlang)){install.packages("rlang");library(rlang)}else{library(rlang)})
suppressMessages(if(!require(sf)){install.packages("sf");library(sf)}else{library(sf)})
suppressMessages(if(!require(shinycssloaders)){install.packages("shinycssloaders");library(shinycssloaders)}else{library(shinycssloaders)})
suppressMessages(if(!require(googleVis)){install.packages("googleVis");library(googleVis)}else{library(googleVis)})
suppressMessages(if(!require(XML)){install.packages("XML");library(XML)}else{library(XML)})
suppressMessages(if(!require(highcharter)){install.packages("highcharter");library(highcharter)}else{library(highcharter)})
suppressMessages(if(!require(RColorBrewer)){install.packages("RColorBrewer");library(RColorBrewer)}else{library(RColorBrewer)})
suppressMessages(if(!require(collapsibleTree)){install.packages("collapsibleTree");library(collapsibleTree)}else{library(collapsibleTree)})
suppressMessages(if(!require(shinydashboardPlus)){install.packages("shinydashboardPlus");library(shinydashboardPlus)}else{library(shinydashboardPlus)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(shinyWidgets)){install.packages("shinyWidgets");library(shinyWidgets)}else{library(shinyWidgets)})
suppressMessages(if(!require(googleway)){install.packages("googleway");library(googleway)}else{library(googleway)})
suppressMessages(if(!require(bsplus)){install.packages("bsplus");library(bsplus)}else{library(bsplus)})
suppressMessages(if(!require(shinyjs)){install.packages("shinyjs");library(shinyjs)}else{library(shinyjs)})


short_info <- function(input, title, place){
    
    input %>% shinyInput_label_embed(shiny_iconlink() %>% bs_embed_tooltip( title = title, placement = place))
}

source("www/helpers.R", local = TRUE)

jschange_tit <- "
shinyjs.change_tit = function(params) {
var defaultParams = {
        id : null,
        new_title : null
      };
      
      params = shinyjs.getParams(params, defaultParams);
      
  document.getElementById(params.id).title = params.new_title;
 
}

shinyjs.change_icon = function(params) {
var defaultParams = {
        id : null,
        new_icon : null,
        new_title : null
      };
      
      params = shinyjs.getParams(params, defaultParams);
      

    
   document.getElementById(params.id).setAttribute('data-original-title',  params.new_title);
   document.getElementById(params.id).className = params.new_icon;
   document.getElementById(params.id).title = params.new_title;
  
 
}


"
header <- dashboardHeaderPlus(
   
    title = tagList(
        span(class = "logo-lg", "LGA dashboard"), 
        img(src = "https://image.flaticon.com/icons/svg/861/861083.svg"),
       ),
    dropdownMenuOutput("messageMenu"),
    dropdownMenuOutput("restoreSession")

)





sidebar <- dashboardSidebar(
    
    sidebarMenu(id = "menu", 
                menuItem(" Home", tabName = "tab1", icon = icon("fars fa-home"), selected = T),
                menuItem(" Crops summary", tabName = "tab2", icon = icon("fas fa-seedling")),
                menuItem(" Map tools", tabName = "tab3", icon = icon("fas fa-globe-africa")),
                menuItem(" Diversitity Tree", tabName = "diver_tree ", icon = icon("fas fa-project-diagram")),
                menuItem(" About", tabName = "tab4", icon = icon("fas fa-info-circle")),
                HTML("
                 <footer style='position:absolute; align: center; bottom:0px; width:100%; height:60px; color: white; padding: 5px;'>
                 <table style='margin-left:auto; margin-right:auto;'>
                    <tr>
                    <td style='padding: 5px;'><a href='https://www.facebook.com/BiovIntCIAT.esp/' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>
                    <td style='padding: 5px;'><a href='https://www.youtube.com/user/ciatweb' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>
                    <td style='padding: 5px;'><a href='https://twitter.com/BiovIntCIAT_esp' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>
                    <td style='padding: 5px;'><a href='https://github.com/CIAT-DAPA' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>
                    </tr>
                    </table>
                 <p style='text-align:center; align: center; padding: 0px; margin: 0px;'>
                 <small>
                 <em>Search us in</em>
                 </small>
                 </p>
                 </footer> ")
                
    ) 
    
)





body <- dashboardBody(
    
   
    
    tabItems(
        tabItem(tabName = "tab1",
                tags$script(src = "https://code.highcharts.com/mapdata/custom/world-palestine-highres.js"),
                useShinyjs(),
                tags$head(HTML(
                  '<style> 
                             .card {
                             box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                             padding: 8px;
                             border: 1px solid #ccc;
                             border-radius: 5px; /* 5px rounded corners */
                             max-width: 800px;
                             margin: 0 auto;
                             background-color:white;
                             }
                            
                             
                             .fa-pagelines:hover{
                                font-size:2em;
                                transition: 1s ease-out;
                             }
                                
                            .thumbs-up:hover{
                                font-size:2em;
                                transition: 1s ease-out;
                            }
                             .fa-globe:hover{
                                font-size:2em;
                                transition: 1s ease-out;
                            }
                             .fa-map-pin:hover{
                                font-size:2em;
                                transition: 1s ease-out;
                             }
                             .header-wrap {
                                  display: flex;                   /* 1 */
                                  align-items: flex-start;         /* 2 */
                                  justify-content: space-between;  /* 3 */
                                  text-align: center;
                                  padding: 1rem 0;
                                }
                                
                                
                                .header-left   {  width: 100px; }
                                .header-right  { width: 100px; }
                                .header-center {  width: 400px; }
                             
                           
                            
                             </style>'
                )),
                tags$h4("Landraces Diversity Conservation Satus", tags$small("(Version 0.0)")),
                textOutput("txt_clusters"),
                        tags$ol(),
                        fluidRow(
                            infoBoxOutput("infobox_tot_occ", width = 3),
                            infoBoxOutput("infobox_countires", width = 3),
                            infoBoxOutput("infobox_total_crops", width = 3),
                            infoBoxOutput("infobox_finished", width = 3)),
              
                boxPlus(solidHeader = FALSE, title = "Multi-crop Joint Priorities", background = NULL,
                    width = NULL, status = "danger", collapsible = F, closable = F,
                    column(width = 8,
                           materialSwitch(
                               inputId = "change_tree",
                               label = "Tree chart", 
                               value = FALSE,
                               status = "success"
                           ),
                           tags$script(src = "https://code.highcharts.com/mapdata/custom/world-palestine-highres.js"),
                           highchartOutput("hcontainer",height = "80%", width = "95%") %>% 
                               withSpinner(color = "green")
                           
                    ),
                    column(width = 4,
                           style = "background-color:#F9F9F9;border-style: solid;border-color:#EFEFEF;border-radius: 7px;",
                           tags$h3(tags$strong("Overview of: ", textOutput('c_name', inline = T)), align="center"),
                           valueBoxOutput("vBox_1", width = 12),
                           valueBoxOutput("vBox_2", width = 12),
                           valueBoxOutput("vBox_3", width = 12),
                           
                    ), 
                    footer = fluidRow(
                        column(
                            width = 6,
                            descriptionBlock(
                                number = textOutput("descBlock_txt_2", inline = T), 
                                numberColor = "green", 
                                numberIcon = "fa fa-caret-up",
                                header =  textOutput("descBlock_txt_1", inline = T), 
                                text = "Priorities", 
                                rightBorder = TRUE,
                                marginBottom = FALSE
                            )
                        ),
                        column(
                            width = 6,
                            descriptionBlock(
                                number = "18%", 
                                numberColor = "red", 
                                numberIcon = "fa fa-caret-down",
                                header = "1200", 
                                text = "GOAL COMPLETION", 
                                rightBorder = FALSE,
                                marginBottom = FALSE
                            )
                        )
                    )
                ),#end boxplus, 
                fluidRow(
                    column(width = 4,
                           box(title = "Coverage ranking (Top 20)", width = NULL,status = "success",
                               highchartOutput("hcontainer4",height = "70%")%>% 
                                   withSpinner(color = "green")
                           )
                           
                    ),
                  column(width = 4,
                         box(title = "Data source",width = NULL,status = "success",
                             highchartOutput("hcontainer2",height = "90%")%>% 
                                 withSpinner(color = "green")
                             )
                         ),
                  column(width = 4, 
                         box(title = "Number of Occurrences (Top 20)",width = NULL,status = "success",
                             highchartOutput("hcontainer3",height = "70%")%>% 
                                 withSpinner(color = "green")
                             )
                         )
                  
                )
                       
                           ),
        
        ###### GOOGLE SECTION
        tabItem(tabName = "tab3",
                tags$head(tags$script(HTML("
                 var click = 0;
                              $(document).on('click', '.zoom_cord', function () {
                             
                              click += 1; 
                              
                                Shiny.onInputChange('last_btn',this.id+'_'+click);
                             });  "))),
                extendShinyjs(text = jschange_tit, functions = c("change_tit", "change_icon")),
                boxPlus(title = tags$h4("Collecting Mission Planning Tool", tags$small("(Version 0.0)")),
                        status = "success", 
                        width = NULL,
                        collapsible = TRUE,
                        enable_sidebar = TRUE,
                        sidebar_width = 40,
                        sidebar_start_open = FALSE,
                        sidebar_icon = "info",
                        sidebar_background = "#EEEEEE",
                        closable = FALSE,
                        fluidRow(
                          column(4,
                                 uiOutput("country_selector")
                          )
                        ),
                        tags$section(
                          tags$div(class = "header-wrap",
                                   tags$div(class = "header-left", 
                                            shinyWidgets::dropdownButton( circle = TRUE,
                                                                          inputId = "dropbtn",
                                                                          status = "primary",
                                                                          size  = "sm",
                                                                          icon = icon("gear"), 
                                                                          width = "600px",
                                                                          tooltip = tooltipOptions(title = "Download gap maps"),
                                                                          column(width = 12,
                                                                                 
                                                                                 uiOutput("multiselect1"),
                                                                                 fluidRow(
                                                                                   column(6,
                                                                                          withBusyIndicatorUI(
                                                                                            actionBttn(
                                                                                              inputId = "add_to_map",
                                                                                              label = "Add to map",
                                                                                              style = "jelly", 
                                                                                              color = "success")
                                                                                          )))))
                                            ),
                                   tags$div(class = "header-center", 
                                            textOutput("selected_crops")
                                            ),
                                   tags$div(class = "header-right", materialSwitch(
                                     inputId = "hide_layer",
                                     label = "Hide layer", 
                                     value = TRUE,
                                     status = "primary"
                                   ))
                          )
                        ),
                    
                    google_mapOutput(outputId = "Gmap", height = "600px"),
                    verbatimTextOutput("map_LatLng"),
                    tags$div(style = "height:20px"),
                    column(12,
                           panel(
                        uiOutput("fields_fijos") ,
                        uiOutput("fields_more"),
                        style = "height:20vh; overflow-x:auto; max-height:400px")),
                    fluidRow(
                        column(1,actionBttn(inputId = "add_field", style = "bordered",
                                            color = "primary",
                                            label = "", icon = icon("fas fa-plus")) %>% 
                                   bs_embed_tooltip(title = "Add field", placement = "right")),
                        column(1,actionBttn(inputId = "rm_field", style = "bordered",
                                            color = "primary",
                                            label = "", icon = icon("fas fa-minus"))%>% 
                                   bs_embed_tooltip(title = "Remove field", placement = "right")),
                        column(9, span( actionBttn(inputId = 'get_route', 
                                                   label ='Get route',
                                                   style = "simple",
                                                   color = "primary"),
                                        downloadBttn(
                                          outputId = 'downl_route',
                                          label    = '',
                                          style = "simple",
                                          color = "primary")%>% 
                                          bs_embed_tooltip(title = "Download route", placement = "right"),
                                        style = "position:absolute;right:1px"),
                               actionBttn(inputId = 'clear_all', 
                                          label ='clear',
                                          style = "simple",
                                          color = "primary"))
                               
                    ),
            
                    sidebar_content = tagList(span(tags$i(h4(tags$strong("Route summary."))), style="color:#000000"),
                                              tags$hr(),
                                              uiOutput("route_stats"))
                    
                    
                    )#end boxplus
                ), #end TabItem
        tabItem(tabName = "diver_tree",
                
                box(title = "diversity tree", status = "success",width = NULL,
                    collapsibleTreeOutput("dv_tree1", width = "100%", height = "400px") %>% 
                        withSpinner(color = "green")
                    )
                ),
        tabItem(tabName = "tab4",
                fluidRow(  
                    box(title = "Developers", status = "success",width = 3,
                                boxProfile(
                                    src = "https://avatars2.githubusercontent.com/u/29578684?s=460&v=4",
                                    title = "Andres Camilo Mendez",
                                    subtitle = "Statistician",
                                    boxProfileItemList(
                                        bordered = TRUE,
                                        boxProfileItem(
                                            title = "Followers",
                                            description = 79
                                        ),
                                        boxProfileItem(
                                            title = "Following",
                                            description = 102
                                        ),
                                        boxProfileItem(
                                            title = "Friends",
                                            description = 181
                                        )
                                    )
                                )
                ),
                
                box(title = "Powered by", status = "success",width = 7,
                    userList(
                        tags$a( href = "https://ciat.cgiar.org", target = "_blank",
                        userListItem(
                            src = "https://ciat.cgiar.org/wp-content/uploads/CIAT-Logo-light.jpg", 
                            user_name = "CIAT", 
                            description = "Palmira, Colombia"
                        )),
                        tags$a( href = "https://www.icarda.org", target = "_blank",
                        userListItem(
                            src = "https://www.icarda.org/themes/custom/icarda/logo.svg", 
                            user_name = "ICARDA", 
                            description = "Rabat, Morocco"
                        )),
                        tags$a( href = "https://www.croptrust.org", target = "_blank",
                        userListItem(
                            src = "data:image/jpeg;base64,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", 
                            user_name = "Crop Trust", 
                            description = "Bonn, Germany"
                        ))
                    ) 
                )
                
                )#end fluidrow
                
                
        )
    )
)


footer = dashboardFooter(
          left_text = "By Divad Nojnarg",
          right_text = "Zurich, 2019"
         )

dashboardPagePlus(skin = "green",
                  header,
                  sidebar,#dashboardSidebar(disable = F),
                  body
                  #footer
)


