busyIndicator <- function(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=1000) {
  
  
  tagList(
    singleton(tags$head(
      tags$style(HTML("div.shinysky-busy-indicator {
                      background-color: rgba(28, 25, 25, .8);
                      position:fixed;
                      top: 40%;
                      left: 30%;
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
              title = tagList(shiny::icon("bug"), "Mesoamerican Bean"),
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1.m",width = "10px", height = "250px",
           
              
              
              # tabPanel("Crops and expert name"#,selectInput("crop","Select crop variety:",c("-- Please select crop --" ,"Potato" ,       "Barley"  ,      "Banana"  ,      "Rice"  ,        "Chickpea" ,     "Common bean" ,  "Wheat (bread)" ,"Wheat (durum)" ,"Cassava"   ,    "Maize" ,       
              #                                 #                                             "Yam" ,          "Grass pea" ,    "Lentil"  ,      "Sweet potato" , "Sorghum" ,      "Groundnut" ,    "Cowpea"    ,    "Pea"    ,       "Faba bean"   ,  "Pigeonpea" ,   
              #                                 #                                             "Finger millet", "Pearl millet" , "Forages"   ),selected = NULL)
              #                                   #,textInput('nombre',"Please type your full name")
              #          ,bsButton("keep.m",label = " Save",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep.m", title = "Save name", placement = "right", trigger = "hover"))
              #          
              
              tabPanel("Occurrence",   box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap4.m",height = 500,width = "100%") ) 
                       
                       
                       , box(solidHeader = F,width = 4, tagList(div( tags$h3(HTML("<b>1. Truly occurrence of crops </b>"))),
                                                                tags$h5(HTML("<p align=justify> Based on occurence coordinates from CIAT landraces beans crops shown in the map. 
                                                                                                Draw one or more polygons (of any size) over all areas where  you  consider  
                                                                             that presence points are wrong. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                             <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                             B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                             rather than purchasing from the formal seed sector).
                                                                             </p>")),
                                                                textAreaInput("txt0","Write a note or comment:",height = "150px")
                                                                ,div(style="position:relative;width: 100%",bsButton("done0_m", size="large",label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                     bsTooltip(id = "done0_m", title = "Save your work", placement = "right", trigger = "hover")
                                                                     
                                                                     ,div(style="float:right;width:95px",bsButton("next0.m", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                          ,bsTooltip(id = "next0.m", title = "Go to next question", placement = "left", trigger = "hover")
                                                                          
                                                                          
                                                                          )
                                                                )
                                                                
                                                                
                                                                ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                                
                                                                )   )
                       
              )
              
              
              ,tabPanel("1. Cultivars",box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap5.m",height = 500,width = "100%")
                                           
                                           
              )
              , box(solidHeader = F,width = 4, tagList(div( tags$h3(HTML("<b>1. High yielding cultivars</b>"))),
                                                       tags$h5(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas where only modern varieties 
                                                                    (no landraces) of the crop are cultivated. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                    <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                    B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                    rather than purchasing from the formal seed sector).</p>")),
                                                       textAreaInput("txt1","Write a note or comment:",height = "150px")
                                                       ,div(style="position:relative;width: 100%"
                                                            
                                                           ,bsButton("done_m", size="large",label = "Save", block = F, style="primary",icon("cloud-download"))
                                                           ,bsTooltip(id = "done_m", title = "Save your work", placement = "right", trigger = "hover")
                                                            
                                                            ,div(style="float:right;width:95px",bsButton("next1.m", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                 ,bsTooltip(id = "next1.m", title = "Go to next question", placement = "left", trigger = "hover")
                                                                 ,bsButton("back1.m", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                 ,bsTooltip(id = "back1.m", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                 
                                                                 )
                                                       )
                                                       
                                                       
                                                       ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                       
              )
              
              )
              
              ),
              
              tabPanel("2. Landraces", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap6.m", height = 500,width = "100%")
              ), box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>2. Landraces</b>"))),
                                                           
                                                           
                                                           tags$h5(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop are likely to still be cultivated. <br/> Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                                                        <br/> <br/> A). Being tied to a place (undergoing natural selection in a place for a significant period of time). <br/> <br/>
                                                                        B). Farmer reproduced (farmers save the seed/propagules and replant the landrace,
                                                                        rather than purchasing from the formal seed sector).</p></p>")),
                                                           textAreaInput("txt2","Write a note or comment:",height = "150px")
                                                           ,div(style="position:relative",bsButton("done2_m",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                bsTooltip(id = "done2_m", title = "Save your work", placement = "right", trigger = "hover")
                                                                
                                                                
                                                                
                                                                
                                                                ,div(style="float:right;width:95px"
                                                                     ,bsButton("next2.m", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                     ,bsTooltip(id = "next2.m", title = "Go to next question", placement = "left", trigger = "hover")
                                                                     
                                                                     ,bsButton("back2.m", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                     ,bsTooltip(id = "back2.m", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                     
                                                                )
                                                                
                                                                
                                                                
                                                           )  
                                                           
                                                           ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
              )   
              
              )
              # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.    
              
              ),
              tabPanel("3. Collecting", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap7.m", height = 500,width = "100%")
                                            
              ),
              
              
              box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>3. Collecting missions for crop landraces</b>"))),
                                                        
                                                        
                                                        
                                                        tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where you would prioritize collecting trips for landraces of
                                                                     the crop, in order to improve their representation in publicly available genebanks. </p>")),
                                                        textAreaInput("txt3","Write a note or comment:",height = "150px")                                                      
                                                        ,div(style="position:relative"
                                                             ,bsButton("done3_m",size="large", label = "Save", block = F, style="primary",icon("cloud-download")),
                                                             bsTooltip(id = "done3_m", title = "Save your work", placement = "right", trigger = "hover")
                                                             
                                                             ,div(style="float:right;width:95px"
                                                                  ,bsButton("next3.m", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                  ,bsTooltip(id = "next3.m", title = "Go to next question", placement = "left", trigger = "hover")
                                                                  
                                                                  ,bsButton("back3.m", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                  ,bsTooltip(id = "back3.m", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                  
                                                             )
                                                             
                                                             
                                                             
                                                             
                                                        )
                                                        ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
              )   
              
              )
              ),
              
              tabPanel("4. Already conserved", box(background ="black",solidHeader = TRUE,width = 8,leafletOutput("mymap8.m", height = 500,width = "100%"),div(style = "float:right")),
                       
                       box(solidHeader = TRUE,width = 4, tagList(div( tags$h3(HTML("<b>4. Areas sufficiently represented</b>"))),
                                                                 
                                                                 tags$h4(HTML("<p align=justify>Draw one or more polygons (of any size) over all areas where landraces of the crop have already
                                                                              been sufficiently collected and where germplasm is conserved in publicly available genebanks.</p>")),
                                                                 textAreaInput("txt4","Write a note or comment:",height = "150px")
                                                                 ,div(style="position:relative"
                                                                      ,bsButton("done4_m",size="large" ,label = "Save", block = F, style="primary",icon("cloud-download")),
                                                                      bsTooltip(id = "done4_m", title = "Save your work", placement = "right", trigger = "hover")
                                                                      
                                                                      ,div(style="float:right;width:95px"
                                                                           ,bsButton("next4.m", size="large",label = "Next", block = F, style="primary",icon("forward"))
                                                                           ,bsTooltip(id = "next4.m", title = "Go to next question", placement = "left", trigger = "hover")
                                                                           
                                                                           ,bsButton("back4.m", size="large",label = "Back", block = F, style="primary",icon("backward"))
                                                                           ,bsTooltip(id = "back4.m", title = "Return to previous question", placement = "left", trigger = "hover")
                                                                           
                                                                      )
                                                                      
                                                                      
                                                                      ,busyIndicator(text = "Wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                                                      
                                                                 )                                                     
                                                                 
                                                                 
                       )   
                       
                       
                       )
                       
                       
              ),
              
              
              tabPanel("Results",div(tags$h3("Please review your work and return to relevant pages to revise.")),
                       
                       box(title = "Plot Cultivars",status = "primary",solidHeader = TRUE, collapsible = T, collapsed =T, leafletOutput("plot1.m", height = 500,width = "100%")),
                       
                       box(title = "Plot Landraces",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot2.m", height = 500,width = "100%")),
                       box(title = "Plot Collecting",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot3.m", height = 500,width = "100%")),
                       box(title = "Plot Already conserved",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,leafletOutput("plot4.m", height = 500,width = "100%"))
                       
                       
                       ,bsButton("back5.m", size="large",label = "Back", block = F, style="warning",icon("backward"))
                       ,bsTooltip(id = "back5.m", title = "Return to previous question", placement = "left", trigger = "hover")
                       
                       
                       
                       ,div(style="float:right;width:95px",bsButton("close.m",size="large",label="Finish",style = "danger",icon("sign-out")),
                            
                            bsTooltip(id = "close.m", title = "Close all", placement = "bottom", trigger = "hover"))
                       ,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                       
                       
                       #,box(title = "debuggin",status = "primary", collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,tableOutput("debug"))
                       
                       
                       
                       
                       
              )                                                                                                           
            )
            
            
            
            
            
  )