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
suppressMessages(if(!require(yaml)){install.packages('yaml'); library(yaml)} else {library(yaml)})
suppressMessages(if(!require(shiny)){install.packages("shiny");library(shiny)}else{library(shiny)})
suppressMessages(if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}else{library(shinydashboard)})
suppressMessages(if(!require(leaflet)){install.packages("leaflet");library(leaflet)}else{library(leaflet)})
suppressMessages(if(!require(shinyBS)){install.packages("shinyBS");library(shinyBS)}else{library(shinyBS)})
#suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
#suppressMessages(if(!require(plotly)){install.packages('plotly'); library(plotly)} else {library(plotly)})







header <- dashboardHeader(
  title = HTML("<h4><b> survey - </br>  Crop Landraces Gap Analysis</b></h4>"),titleWidth = 260

)

sidebar<-dashboardSidebar( width = 260,
  
 sidebarMenu( id = "menu",
    #menuItem("Beans Geenepools",tabName = "geneBeans", icon = icon("dashboard"), startExpanded = TRUE, ),
    menuItem("Introduction",tabName="introm", icon = icon( "fa fa-angle-double-right")),
    menuItem("Questionaire", tabName = "Andean" ,icon = icon("far fa-angle-double-right")  )
    
    #End menu item beans genepool
    
  )
  
  
)

body <- dashboardBody( 
  

      tabItems(
        
      tabItem(tabName = "introm", 
              
              source("www/introduction.txt", local = TRUE, encoding ="UTF-8")
              
              )
     
      ,tabItem(tabName = "Andean"
    
               ,tabBox( title = tagList(shiny::icon("bug"), "Questions"),id="tabset1", width = 12, height = "500px"
      
                 ,tabPanel("Crops and expert name"
                   
                     ,selectInput("crop","Select crop variety:",c("-- Please select crop --" ,"Potato" ,       "Barley"  ,      "Banana"  ,      "Rice"  ,        "Chickpea" ,     "Common bean" ,  "Wheat (bread)" ,"Wheat (durum)" ,"Cassava"   ,    "Maize" ,       
                                                                       "Yam" ,          "Grass pea" ,    "Lentil"  ,      "Sweet potato" , "Sorghum" ,      "Groundnut" ,    "Cowpea"    ,    "Pea"    ,       "Faba bean"   ,  "Pigeonpea" ,   
            
                                                                                                                                   "Finger millet", "Pearl millet" , "Forages"   )),textInput('nombre',"Please type your full name")
                    ,bsButton("keep",label = " Save",size = "large",block=F,style="primary",icon("hand-o-right")),bsTooltip(id = "keep", title = "Save name", placement = "right", trigger = "hover")
               
#                ,HTML('  
#                      <button id="keep" type="button" class="btn action-button btn-primary btn-lg">
#   <i class="fa fa-hand-o-right"></i>
#    Save
# <script>$(document).ready(function() {setTimeout(function() {shinyBS.addTooltip(\'keep\', \'tooltip\', {\'placement\': \'right\', \'trigger\': \'hover\', \'title\': \'Save name\'})}, 500)});</script>
# </button>
#                      ')
            
          )
        

        
          ,tabPanel("Landrace",
                    
               tagList(
                 tags$head(
            
HTML( '
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.2/leaflet.css" />
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"/>

        <script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.2/leaflet-src.js" ></script> 
        <script src = "https://rawgit.com/Wildhoney/Leaflet.FreeDraw/master/dist/leaflet-freedraw.web.js" ></script>
        
        <script src = "L_Control_Zoominfo.js" ></script>
        <link rel = "stylesheet" href = "L_Control_Zoominfo.css"/>

        <script type="text/javascript" src="leaflet-control-credits.js"></script>
        <link rel="stylesheet" href="leaflet-control-credits.css" />


       <link rel="stylesheet" href="https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.css" />
<script src="https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.js"></script>
        
<style> 



.shinysky-busy-indicator {
                      background-color: rgba(28, 25, 25, .8);
      position:fixed;
      top: 40%;
      left: 30%;
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
      z-index:1100 ;
      }
              * {
              padding: 0;
              margin: 0;
              box-sizing: border-box;
              }

              .map {
               z-index: 0;
              width:1000px; 
              height:450px; 
              color:#0000FF; 
              background-color: black; 
              overflow: hidden;
              }
              
              
              .mode-create {
              cursor: crosshair;
              }
              
              .leaflet-edge {
              background-color: #95bc59;
              box-shadow: 0 0 0 2px white, 0 0 10px rgba(0, 0, 0, .35);
              border-radius: 10%;
              cursor: move;
              outline: none;
              transition: background-color .25s;
              }
              
               div.leaflet-edge.disabled {
              pointer-events: none;
              background-color: #bbb;
              border-radius: 50%;
              }              

              .leaflet-polygon {
              fill: #b4cd8a;
              stroke: #50622b;
              stroke-width: 2;
              fill-opacity: .75;
              }  

              .messages {
              background: #4999E8;
              border: 3px solid #50310a;
              border-radius: 6px;
              left: 50%;
              margin-left: -233px;
              margin-top: 0.3%   ;
              position: absolute;
              top: 11%;
              width: 460px;
              padding: 0.4cm 1cm ;
              font-family: Arial,Helvetica,sans-serif;
              color:#000000 ; 
              }

              .map-container {
              margin: auto;
              border: 3px solid #151515;
             
              }


              .maps-tools {
               float: left;
              list-style: none;
              margin: 0;
              padding: 0;
              position: relative;
              }

              .maps-tools-btn {
                 background: #eee;
                 border: 3px solid #ccc;
                 border-radius: 6px;
                 color	#1E90FF;
                 display: block;
                 float: left;
                 font-family: Tahoma;
                 font-size: 14px;
                 font-weight: 700;
                 margin: 6px 0 6px 6px;
                 padding: 8px 6px 8px 4px;
                 text-decoration: none!important;
                }
            .buttonsEffects:hover {
                background-color:#1E90FF;
                color: #FFFFFF ;
                border: 3px solid #1E90FF;
            }
           
            .buttonDisabled{
                   cursor: no-drop ;
                background: #D5CBCC;
                 border: 3px solid #ccc;
                 border-radius: 6px;
                 color: #8B8989;
                 display: block;
                 float: left;
                 font-family: Arial,Helvetica,sans-serif!important;
                 font-size: 14px;
                 font-weight: 700;
                 margin: 6px 0 6px 6px;
                 padding: 8px 6px 8px 4px;
                 text-decoration: none!important;
                  }
 
            .buttonpressed{
                 background-color:#1E90FF;
                 color: #FFFFFF ;
                 border: 3px solid #1E90FF;
            }

             .buttoncancel{
                 background-color:#E2BC24;
                 color: #FFFFFF ;
                 border: 3px solid #E2BC24;
              }

            .buttonsuccess{
                 background-color:#5CBD3E;
                color: #FFFFFF ;
                border: 3px solid #5CBD3E;
               }

                input[type=reset], button, html input[type=button], input[type=submit] {
                -webkit-appearance: button;
                cursor: pointer;
                }


              .unselectable {
              -moz-user-select: -moz-none;
              -khtml-user-select: none;
              -webkit-user-select: none;
              -o-user-select: none;
              user-select: none;
              }

              .createbtn{
              background: 0 0;
              color: #50310a;
              text-decoration: underline;
              float: right;
              }

             .maps-tools li {
              display: block;
              float: left;
              margin: 0;
             }
            .leaflet-top .leaflet-control {
              margin-top: 60px;
            }


           .maps-mini-tools {
            border-radius: 0px;
            float: right;
            list-style: none;
            margin: 0;
            padding-right: 10px;
           }

          .maps-mini-tools li {
              display: block;
              float: right;
              margin: 0;
           }

           .leaflet-control-geocoder{
           margin-top: 10px;
           }
              </style>
              
<script>

$(document).ready(function(){

 $("#tabset1 a[data-toggle=\'tab\']").click(function () {
   
     setTimeout(function(){ map.invalidateSize();}, 1000);
     setTimeout(function(){ map2.invalidateSize();}, 1000);
     L.polygon(latln, {color: "red"}).addTo(map2);
  });

});

</script>
              
              
              ')
                          ) 
        , tags$body( 
          
          box(width=12, title=HTML("<b>Statement Of The Question 1</b>"),status="warning",solidHeader=TRUE, collapsible = TRUE
              
              
              ,tagList(div( tags$h3(HTML("<b>1. Landraces presence </b>"))),
                      tags$h4(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas landraces are likely to still be cultivated. <br/> 
                                   Landrace is a complicated term. Here we define a landrace as having two essential features: 
                                   <br/> <br/> A). Being the product of human and natural selection in a place for a significant period of time. <br/> <br/>
                                   B). Being farmer reproduced (farmers save the seed/germplasm and replant, rather than purchasing from the formal seed sector).</p>")),
                      textAreaInput("txt1","Write a note or comment:",height = "80px")
                      #,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                      
              )#end tagList
              
              
              )#END BOX STATEMENT OF THE QUESTION
          
          
       ,div(style="margin: 20px 0 0;"
            
            
            
                   
                         , div(id = "map", class= "map map-container"
                               
                               
                               
                               ,div( style = "z-index: 1000; position: absolute; left: 0px; top: 0px; width: 100%"  
                                    
                                     ,HTML(   '<div class="shinysky-busy-indicator">
                                               <h2><font color=white>Wait a moment please...</font></h2>
                                              <img src="http://www.cameronbarnes.com/images/loading.gif" width="95px" height="50px"/>
                                              </div>')
                                     
                                     
                                      
                                     ,div(id = "tool-bar" , style = "z-index: 0; background:rgb(0, 25, 51);background:rgba(0, 25, 51,.6);width:100%  ;height:50px"  
                                          , tags$ul( class="maps-tools"
                                                     ,tags$li(  tags$button( id = "createA", type = "button", class = "maps-tools-btn buttonsEffects", title = "This help you draw a polygon" ,HTML('<i class="fa fa-paint-brush"></i> Create Feature ')   )  )
                                                     ,tags$li(  HTML( '<button  id="editA" type="button" class="maps-tools-btn  buttonDisabled" title="This help you edit a drawn polygon" disabled><i class="fa fa-edit"></i> Edit Feature</button>   ' )  )
                                                     ,tags$li(  HTML( '<button  id="delA" type="button" class="maps-tools-btn  buttonDisabled" title="This help you delete a drawn polygon" disabled><i class="fa fa-minus-circle"></i> Delete Feature</button>   ' )  )
                                                     
                                          )
                                          ,tags$ul(class="maps-mini-tools"
                                                   ,tags$li( HTML( '<button  id="nextA" type="button" class="maps-tools-btn  buttonDisabled" title="Go to next question" disabled><i class="fa fa-chevron-circle-right"></i> Next </button>   ' )  )
                                                   ,tags$li( HTML( '<button  id="saveA" type="button" class="maps-tools-btn  buttonDisabled" title="Save your work" disabled><i class="fa fa-save"></i> Save Features</button>   ' )  )
                                                   
                                          )
                                     )
                               ) 
                               
                               ,div(id="createMess", class="messages unselectable", style=" display: none; z-index: 1100; width:500px;height:50px;"
                                    , tags$span( id="actmess")
                                    , tags$img(id = "imgmess",src = "edit.png", height="17", width="71"  )
                                    ,HTML('  <button id="cancelact" class="createbtn"> <span id="cancelstr">Cancel</span> </button> ')
                                    ,tags$script(  type="text/javascript", charset="UTF-8", '
                                                   
                                                   cancelact.addEventListener("click",  function () {
                                                   
                                                   freeDraw.mode(FreeDraw.NONE);
                                                   createMess.style.display = "none";
                                                   document.getElementById("createA").className = "maps-tools-btn  buttonsEffects";
                                                   });
                                                   
                                                   ')
                                    )    

,HTML(   '<script  type="text/javascript">  
                                                                                                                        
                                        var map;
                                        
                                        map = new L.Map("map", { 
                                        zoominfoControl: true,
                                        zoomControl: false
                                        
                                        });
                                        
                                        var osmUrl="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png";
                                        
                                        
                                        
                                        var osm = L.tileLayer(\'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}\', {
                                        attribution: \'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ\',
                                        maxZoom: 12,
                                        noWrap: true,
                                        minZoom:2
                                        
                                        });		
                                        
                                        
                                        map.setView(new L.LatLng(40, 40),2);
                                        map.addLayer(osm);
                                        
                                        map.setMaxBounds([
                                        [-90, -190],
                                        [90, 190]
                                        ]);
                                        
                                        
                                        var credctrl = L.controlCredits({
                                        image: "ciat-logo.png",
                                        link: "https://ciatnet.ciat.cgiar.org/SitePages/CIATnet_Home.aspx",
                                        text: "<u>Internacional center of <br/> tropical agriculture</u>",
                                        width: 47,
                                        height: 47
                                        }).addTo(map);
                                        
                                        
                                        L.Control.geocoder({ position:"topleft" }).addTo(map);
                                        
                                        const freeDraw = new FreeDraw({mode: FreeDraw.NONE , leaveModeAfterCreate: true });
                                        map.addLayer(freeDraw);
                                        
                                        
                                        
                                        
                                        
                                        
                                        $("#createA").on("click", function() {
                                        
                                        freeDraw.mode(FreeDraw.CREATE);
                                        
                                        var x = document.getElementById("createMess");
                                        if (x.style.display === "none") {
                                        x.style.display = "block";
                                        } else {
                                        x.style.display = "none";
                                        }
                                        
                                        document.getElementById("actmess").innerHTML = "Press left Click to start drawing ".bold();
                                        document.getElementById("imgmess").src = "edit.png" ;
                                        
                                        
                                        document.getElementById("createA").className = "maps-tools-btn buttonpressed";
                                        
                                        $("#map").on("mouseup", function(){
                                        x.style.display = "none";
                                        document.getElementById("createA").className = "maps-tools-btn  buttonsEffects";
                                        
                                        
                                        if(freeDraw.size()=== 0){
                                        document.getElementById("editA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("editA").disabled = true;
                                        
                                        document.getElementById("delA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("delA").disabled = true;
                                        
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("saveA").disabled = true;
                                        
                                        } else {
                                        document.getElementById("editA").disabled = false; 
                                        document.getElementById("editA").className = "maps-tools-btn  buttonsEffects";
                                        
                                        document.getElementById("delA").disabled = false; 
                                        document.getElementById("delA").className = "maps-tools-btn  buttonsEffects";
                                        
                                        document.getElementById("saveA").disabled = false; 
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonsEffects";
                                        
                                        }
                                        
                                        });    
                                        
                                        $("body").keydown( function(event) {
                                        
                                        if(event.key === "Escape") { freeDraw.cancel();} 
                                        freeDraw.mode(FreeDraw.NONE);
                                        document.getElementById("createA").className = "maps-tools-btn  buttonsEffects";
                                        });
                                        
                                        
                                        });
                                        
                                        
                                        
                                        $("#editA").on("click", function() {
                                        //alert("did something");
                                        freeDraw.mode(FreeDraw.EDIT | FreeDraw.APPEND );
                                        
                                        
                                        $("#map").off("mouseup");
                                        
                                        var x = document.getElementById("createMess");
                                        if (x.style.display === "none") {
                                        x.style.display = "block";
                                        } else {
                                        x.style.display = "none";
                                        }
                                        
                                        document.getElementById("actmess").innerHTML = "Drag points to edit  polygons ".bold();
                                        document.getElementById("imgmess").src = "edit.png" ;
                                        document.getElementById("cancelstr").innerHTML = "Done";
                                        
                                        document.getElementById("editA").className = "maps-tools-btn buttonpressed";
                                        
                                        $("#cancelact").on("click", function(){
                                        x.style.display = "none";
                                        document.getElementById("editA").className = "maps-tools-btn buttonEffects";
                                        });    
                                        
                                        $("body").keydown( function(event) {
                                        // Cancel the current action when the escape key is pressed.
                                        if(event.key === "Escape"){ freeDraw.cancel();}
                                        freeDraw.mode(FreeDraw.NONE);
                                        document.getElementById("editA").className = "maps-tools-btn buttonsEffects";
                                        x.style.display = "none";
                                        });
                                        
                                        
                                        }); 
                                        
                                        
                                        $("#delA").on("click", function() {
                                        
                                        
                                        document.getElementById("editA").className = "maps-tools-btn buttonEffects";
                                        freeDraw.mode(FreeDraw.DELETE);
                                        
                                        var x = document.getElementById("createMess");
                                        if (x.style.display === "none") {
                                        x.style.display = "block";
                                        }
                                        
                                        $("#map").on("mouseup", function(){
                                        
                                        if(freeDraw.size()=== 0){
                                        document.getElementById("editA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("editA").disabled = true;
                                        
                                        document.getElementById("delA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("delA").disabled = true;
                                        
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("saveA").disabled = true;
                                        
                                        
                                        freeDraw.mode(FreeDraw.NONE);
                                        x.style.display = "none";
                                        
                                        } else {
                                        document.getElementById("editA").disabled = false; 
                                        document.getElementById("editA").className = "maps-tools-btn  buttonEffects";
                                        
                                        document.getElementById("delA").disabled = false; 
                                        document.getElementById("delA").className = "maps-tools-btn  buttonEffects";
                                        document.getElementById("delA").className = "maps-tools-btn buttonpressed";
                                        
                                        document.getElementById("saveA").disabled = false; 
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonsEffects";
                                        
                                        x.style.display = "block";
                                        }
                                        
                                        
                                        document.getElementById("createA").className = "maps-tools-btn buttonEffects";
                                        
                                        });  
                                        
                                        
                                        
                                        document.getElementById("actmess").innerHTML = "Click on polygons to delete it ".bold();
                                        document.getElementById("imgmess").src = "edit.png" ;
                                        document.getElementById("cancelstr").innerHTML = "Done";
                                        
                                        document.getElementById("delA").className = "maps-tools-btn buttonpressed";
                                        
                                        $("#cancelact").on("click", function(){
                                        x.style.display = "none";
                                        document.getElementById("delA").className = "maps-tools-btn buttonEffects";
                                        freeDraw.mode(FreeDraw.NONE);
                                        
                                        $("#map").off("mouseup");
                                        
                                        if(freeDraw.size()=== 0){
                                        document.getElementById("editA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("editA").disabled = true;
                                        
                                        document.getElementById("delA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("delA").disabled = true;
                                        
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("saveA").disabled = true;
                                        
                                        }
                                        
                                        });    
                                        
                                        $("body").keydown( function(event) {
                                        // Cancel the current action when the escape key is pressed.
                                        if(event.key === "Escape"){ freeDraw.cancel();}
                                        freeDraw.mode(FreeDraw.NONE);
                                        document.getElementById("delA").className = "maps-tools-btn buttonEffects";
                                        x.style.display = "none";
                                        
                                        $("#map").off("mouseup");
                                        
                                        if(freeDraw.size()=== 0){
                                        document.getElementById("editA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("editA").disabled = true;
                                        
                                        document.getElementById("delA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("delA").disabled = true;
                                        
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonDisabled";
                                        document.getElementById("saveA").disabled = true;
                                        
                                        }
                                        
                                        
                                        
                                        }); 
                                        
                                        
                                        }); 
                                        

                                        var latln;
                                        freeDraw.on("markers", function(event){                                                                                                                                                      
                                        latln = event.latLngs;
                                        str = JSON.stringify(event.latLngs);

                                        
                                        
                                        $("#saveA").on("click",  function(){


                                        $("#map").off("mauseup");
                                        if(freeDraw.size() !== 0) { 

                                        document.getElementById("nextA").disabled = false;
                                        document.getElementById("nextA").className = "maps-tools-btn buttonEffects";
                                        
                                         $("#nextA").on("click", function(){
                                        
                                        Shiny.onInputChange("nextA", 2)
                                        setTimeout(function(){ map2.invalidateSize();}, 1000);
                                        L.polygon(latln, {color: "red"}).addTo(map2);
         
                                        });


                                        Shiny.onInputChange("mydata", str);
                                        document.getElementById("saveA").className = "maps-tools-btn  buttonsuccess";
                                        
         	setInterval(function(){
         if ($(\'html\').hasClass(\'shiny-busy\')) {
         setTimeout(function() {
         if ($(\'html\').hasClass(\'shiny-busy\')) {
         $(\'div.shinysky-busy-indicator\').show()
         }
         }, 200)  		    
         } else {
         $(\'div.shinysky-busy-indicator\').hide()
         }
         },100)
        





                                        }  
                                        
                                        });
                                        
                                        
                                        
                                        }); 
                                        
                                        
                                        
                                        
                                        $("#keep").click( function(){
                                        
                                        setTimeout(function(){ map.invalidateSize();}, 1000);
                                        
                                        
                                        });
                                        
                                        
                                        </script>                                                                                                                                                        
                                        '
                                        
                               )
#,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)

   
                               )#end div id= map
                         
                         
                                               
        )      
                         
                     )
    
  
                     
        )
        
             )# end tabpanel Landrace
      
      ,tabPanel("CGIAR gaps",
                
                tags$body( 
                  
                  box(width=12, title=HTML("<b>Statement Of The Question 2</b>"),status="warning",solidHeader=TRUE, collapsible = TRUE
                      
                      
                      ,tagList(div( tags$h3(HTML("<b>1. Gaps in CGIAR collections for landraces </b>"))),
                               tags$h4(HTML("<p align=justify> Draw one or more polygons (of any size) over all areas where you think that there are gaps in CGIAR collections with regard to previous collecting trips. <br/> 
                                            ")),
                               textAreaInput("txt2","Write a note or comment:",height = "80px")
                               #,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                               
                      )#end tagList
                      
                      
                  )#END BOX STATEMENT OF THE QUESTION
                  
                  
                  ,div(style="margin: 20px 0 0;"
                       
                       
                       
                       
                       , div(id = "map2", class= "map map-container"
                             
                             
                             
                             ,div( style = "z-index: 1000; position: absolute; left: 0px; top: 0px; width: 100%"  
                                   
                                   ,HTML(   '<div class="shinysky-busy-indicator">
                                            <h2><font color=white>Wait a moment please...</font></h2>
                                            <img src="http://www.cameronbarnes.com/images/loading.gif" width="95px" height="50px"/>
                                            </div>')
                                   
                                   
                                   
                                   ,div(id = "tool-bar2" , style = "z-index: 0; background:rgb(0, 25, 51);background:rgba(0, 25, 51,.6);width:100%  ;height:50px"  
                                        , tags$ul( class="maps-tools"
                                                   ,tags$li(  tags$button( id = "createA2", type = "button", class = "maps-tools-btn buttonsEffects", title = "This help you draw a polygon" ,HTML('<i class="fa fa-paint-brush"></i> Create Feature ')   )  )
                                                   ,tags$li(  HTML( '<button  id="editA2" type="button" class="maps-tools-btn  buttonDisabled" title="This help you edit a drawn polygon" disabled><i class="fa fa-edit"></i> Edit Feature</button>   ' )  )
                                                   ,tags$li(  HTML( '<button  id="delA2" type="button" class="maps-tools-btn  buttonDisabled" title="This help you delete a drawn polygon" disabled><i class="fa fa-minus-circle"></i> Delete Feature</button>   ' )  )
                                                   
                                        )
                                        ,tags$ul(class="maps-mini-tools"
                                                 ,tags$li( HTML( '<button  id="closeA" type="button" class="maps-tools-btn  buttonDisabled" title="Close the survey" disabled><i class="fa fa-external-link-alt"></i> Close ALL </button>   ' )  )
                                                 
                                                 ,tags$li( HTML( '<button  id="saveA2" type="button" class="maps-tools-btn  buttonDisabled" title="Save your work" disabled><i class="fa fa-save"></i> Save Features</button>   ' )  )
                                                 
                                        )
                                   )
                                   ) 
                             
                             ,div(id="createMess2", class="messages unselectable", style=" display: none; z-index: 1100; width:500px;height:50px;"
                                  , tags$span( id="actmess2")
                                  , tags$img(id = "imgmess2",src = "edit.png", height="17", width="71"  )
                                  ,HTML('  <button id="cancelact2" class="createbtn"> <span id="cancelstr">Cancel</span> </button> ')
                                  ,tags$script(  type="text/javascript", charset="UTF-8", '
                                                 
                                                 cancelact2.addEventListener("click",  function () {
                                                 
                                                 freeDraw2.mode(FreeDraw.NONE);
                                                 createMess2.style.display = "none";
                                                 document.getElementById("createA2").className = "maps-tools-btn  buttonsEffects";
                                                 });
                                                 
                                                 ')
                                  )    
                             
                             ,HTML(   '<script  type="text/javascript">  
                                      
                                      var map2;
                                      
                                      map2 = new L.Map("map2", { 
                                      zoominfoControl: true,
                                      zoomControl: false
                                      
                                      });
                                      
                                      var osmUrl="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png";
                                      
                                      
                                      
                                      var osm = L.tileLayer(\'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}\', {
                                      attribution: \'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ\',
                                      maxZoom: 12,
                                      noWrap: true,
                                      minZoom:2
                                      
                                      });		
                                      
                                      
                                      map2.setView(new L.LatLng(40, 40),2);
                                      map2.addLayer(osm);
                                      
                                      map2.setMaxBounds([
                                      [-90, -190],
                                      [90, 190]
                                      ]);
                                      
                                      
                                      var credctrl = L.controlCredits({
                                      image: "ciat-logo.png",
                                      link: "https://ciatnet.ciat.cgiar.org/SitePages/CIATnet_Home.aspx",
                                      text: "<u>Internacional center of <br/> tropical agriculture</u>",
                                      width: 47,
                                      height: 47
                                      }).addTo(map2);
                                      
                                      
                                      L.Control.geocoder({ position:"topleft" }).addTo(map2);
                                      
                                      const freeDraw2 = new FreeDraw({mode: FreeDraw.NONE , leaveModeAfterCreate: true });
                                      map2.addLayer(freeDraw2);
                                      
                                      
                                     
                                      
                                      
                                      
                                      $("#createA2").on("click", function() {
                                      
                                      freeDraw2.mode(FreeDraw.CREATE);
                                      
                                      var x = document.getElementById("createMess2");
                                      if (x.style.display === "none") {
                                      x.style.display = "block";
                                      } else {
                                      x.style.display = "none";
                                      }
                                      
                                      document.getElementById("actmess2").innerHTML = "Press left Click to start drawing ".bold();
                                      document.getElementById("imgmess2").src = "edit.png" ;
                                      
                                      
                                      document.getElementById("createA2").className = "maps-tools-btn buttonpressed";
                                      
                                      $("#map2").on("mouseup", function(){
                                      x.style.display = "none";
                                      document.getElementById("createA2").className = "maps-tools-btn  buttonsEffects";
                                      
                                      
                                      if(freeDraw2.size()=== 0){
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("editA2").disabled = true;
                                      
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("delA2").disabled = true;
                                      
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("saveA2").disabled = true;
                                      
                                      } else {
                                      document.getElementById("editA2").disabled = false; 
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonsEffects";
                                      
                                      document.getElementById("delA2").disabled = false; 
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonsEffects";
                                      
                                      document.getElementById("saveA2").disabled = false; 
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonsEffects";
                                      
                                      }
                                      
                                      });    
                                      
                                      $("body").keydown( function(event) {
                                      
                                      if(event.key === "Escape") { freeDraw2.cancel();} 
                                      freeDraw2.mode(FreeDraw.NONE);
                                      document.getElementById("createA2").className = "maps-tools-btn  buttonsEffects";
                                      });
                                      
                                      
                                      });
                                      
                                      
                                      
                                      $("#editA2").on("click", function() {
                                      //alert("did something");
                                      freeDraw2.mode(FreeDraw.EDIT | FreeDraw.APPEND );
                                      
                                      
                                      $("#map2").off("mouseup");
                                      
                                      var x = document.getElementById("createMess2");
                                      if (x.style.display === "none") {
                                      x.style.display = "block";
                                      } else {
                                      x.style.display = "none";
                                      }
                                      
                                      document.getElementById("actmess2").innerHTML = "Drag points to edit  polygons ".bold();
                                      document.getElementById("imgmess2").src = "edit.png" ;
                                      document.getElementById("cancelstr").innerHTML = "Done";
                                      
                                      document.getElementById("editA2").className = "maps-tools-btn buttonpressed";
                                      
                                      $("#cancelact2").on("click", function(){
                                      x.style.display = "none";
                                      document.getElementById("editA2").className = "maps-tools-btn buttonEffects";
                                      });    
                                      
                                      $("body").keydown( function(event) {
                                      // Cancel the current action when the escape key is pressed.
                                      if(event.key === "Escape"){ freeDraw2.cancel();}
                                      freeDraw2.mode(FreeDraw.NONE);
                                      document.getElementById("editA2").className = "maps-tools-btn buttonsEffects";
                                      x.style.display = "none";
                                      });
                                      
                                      
                                      }); 
                                      
                                      
                                      $("#delA2").on("click", function() {
                                      
                                      
                                      document.getElementById("editA2").className = "maps-tools-btn buttonEffects";
                                      freeDraw2.mode(FreeDraw.DELETE);
                                      
                                      var x = document.getElementById("createMess2");
                                      if (x.style.display === "none") {
                                      x.style.display = "block";
                                      }
                                      
                                      $("#map2").on("mouseup", function(){
                                      
                                      if(freeDraw2.size()=== 0){
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("editA2").disabled = true;
                                      
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("delA2").disabled = true;
                                      
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("saveA2").disabled = true;
                                      
                                      
                                      freeDraw2.mode(FreeDraw.NONE);
                                      x.style.display = "none";
                                      
                                      } else {
                                      document.getElementById("editA2").disabled = false; 
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonEffects";
                                      
                                      document.getElementById("delA2").disabled = false; 
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonEffects";
                                      document.getElementById("delA2").className = "maps-tools-btn buttonpressed";
                                      
                                      document.getElementById("saveA2").disabled = false; 
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonsEffects";
                                      
                                      x.style.display = "block";
                                      }
                                      
                                      
                                      document.getElementById("createA2").className = "maps-tools-btn buttonEffects";
                                      
                                      });  
                                      
                                      
                                      
                                      document.getElementById("actmess2").innerHTML = "Click on polygons to delete it ".bold();
                                      document.getElementById("imgmess2").src = "edit.png" ;
                                      document.getElementById("cancelstr").innerHTML = "Done";
                                      
                                      document.getElementById("delA2").className = "maps-tools-btn buttonpressed";
                                      
                                      $("#cancelact2").on("click", function(){
                                      x.style.display = "none";
                                      document.getElementById("delA2").className = "maps-tools-btn buttonEffects";
                                      freeDraw2.mode(FreeDraw.NONE);
                                      
                                      $("#map2").off("mouseup");
                                      
                                      if(freeDraw2.size()=== 0){
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("editA2").disabled = true;
                                      
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("delA2").disabled = true;
                                      
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("saveA2").disabled = true;
                                      
                                      }
                                      
                                      });    
                                      
                                      $("body").keydown( function(event) {
                                      // Cancel the current action when the escape key is pressed.
                                      if(event.key === "Escape"){ freeDraw2.cancel();}
                                      freeDraw2.mode(FreeDraw.NONE);
                                      document.getElementById("delA2").className = "maps-tools-btn buttonEffects";
                                      x.style.display = "none";
                                      
                                      $("#map2").off("mouseup");
                                      
                                      if(freeDraw2.size()=== 0){
                                      document.getElementById("editA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("editA2").disabled = true;
                                      
                                      document.getElementById("delA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("delA2").disabled = true;
                                      
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonDisabled";
                                      document.getElementById("saveA2").disabled = true;
                                      
                                      }
                                      
                                      
                                      
                                      }); 
                                      
                                      
                                      }); 
                                      


                                      freeDraw2.on("markers", function(event){                                                                                                                                                      
                                      var str2 = JSON.stringify(event.latLngs);

                                         
                                      $("#saveA2").on("click",  function(){
                                 
                                      $("#map2").off("mauseup");

                                      if(freeDraw2.size() !== 0) { 

                                      Shiny.onInputChange("mydata2", str2);
                                      document.getElementById("saveA2").className = "maps-tools-btn  buttonsuccess";

                                      document.getElementById("closeA").className = "maps-tools-btn  buttoncancel";
                                      document.getElementById("closeA").disabled = false;
                                      
                                      setInterval(function(){
                                      if ($(\'html\').hasClass(\'shiny-busy\')) {
                                      setTimeout(function() {
                                      if ($(\'html\').hasClass(\'shiny-busy\')) {
                                      $(\'div.shinysky-busy-indicator\').show()
                                      }
                                      }, 200)  		    
                                      } else {
                                      $(\'div.shinysky-busy-indicator\').hide()
                                      }
                                      },100)
                                      
                                       $("#closeA").on("click", function(){
                                      
                                    
                                      Shiny.onInputChange("closeA", 2);
                                      
                                      
                                      });
                            
                                      
                                      }  
                                      
                                      });
                                      
                                      
                                      
                                      }); 
                                      
                                      
                                      
                                      
                                      $("#keep").click( function(){
                                      
                                      setTimeout(function(){ map2.invalidateSize();}, 1000);
                                      
                                      
                                      });
                                      
                                      
                                      </script>                                                                                                                                                        
                                      '
                                        
                               )
#,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)

   
                               )#end div id= map
                         
                         
                                               
        )      
                         
                     )   
        
    
        
      )# end tab panel 

        )
      )
       ,tabItem( tabName= "Mesoamerican"
         
      ,fluidRow(
      box(background ="black",solidHeader = TRUE,width = 8,textOutput("mymap5")
                                  
                                  
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
                                                        ,bsTooltip(id = "next1", title = "Go to next question", placement = "left", trigger = "hover"))
                                                       )
                                               
                                               
                                                  #,busyIndicator(text = "wait a moment pls...",img = "http://www.cameronbarnes.com/images/loading.gif", wait=200)
                                               
                                                  )
      
             
      
      )
  
       )
                                                                                                          
       ) 
    
 
  
  
  
  #)
  
) #end tabItem for the first survey
 
  )

#)#end tabItems

#)#end dashboard Body

dashboardPage(
  header,
  sidebar,#dashboardSidebar(disable = F),
  body
)




