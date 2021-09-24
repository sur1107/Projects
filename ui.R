



library(tidyverse)
library(formattable)
library(readr)
library(leaflet)
library(leaflet.providers)
library(semantic.dashboard)
library(shiny.semantic)


ui <- shinyUI(semanticPage(
div(class="ui container",  
  h1("Distance Tracker for Ship Vessels",icon="dog"),
  
  segment(class="ui raised segment",
        a(class="ui green ribbon label", "Select Input"),
        h1(class="ui header"),
        div(class="ui two column grid",
            div (class="row",
                 div (class="column",
                       uiOutput("select_ship")),
                 div (class="column",
                      uiOutput("select_vessel")),
                 
            )
        )
        
        
      ),
  

  
  div(class = "ui equal width grid", 
      div(class = "four wide column",
          div( 
            h1(id="card"),
            card(
              div(class="content",
                  div(class="center"),
                  div(class="header", textOutput("distance_info")),
                  
              ),
              div(class = "ui teal button", "Distance Info.")
              
            ))),
      
      
      
      div(class = "four wide column",
          div( 
            h1(id="card"),
            card(
              div(class="content",
                  div(class="center"),
                  div(class="header", textOutput("dest_info")),
                  
                  
              ),
              div(class = "ui orange button", "Ship stats")
              
            ))),
      
      div(class = "four wide column",
          div( 
            h1(id="card"),
            card(
              div(class="content",
                  div(class="center"),
                  div(class="header", textOutput("speed_details")),
                 
              ),
              div(class = "ui teal button", "Speed Info")
              
            ))),
      
      div(class = "four wide column",
          div( 
            h1(id="card"),
            card(
              div(class="content",
                  div(class="center"),
                  div(class="header", textOutput("ship_info")),
                 
              ),
              div(class = "ui orange button", "Ship Details")
              
            ))),
  ),  
  
  sidebarLayout(
    sidebarPanel(
        h3("Let's track"),
   
    ),
    
    mainPanel(
      div (class="column",
           div (class="three wide column",
                div (class="ui segment raised blue",
                     
                     div (id="box",class="ui accordion",leafletOutput("map")))))
      
    ))
    )
  )
)