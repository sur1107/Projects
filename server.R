# setwd("C:\\D DRIVE\\Personal\\APPSILON\\14-9")
options(shiny.maxRequestSize=30*1024^2)
options(warn = -1)
library(tidyverse)
library(formattable)
library(readr)
library(leaflet)
library(leaflet.providers)
library(semantic.dashboard)
library(shiny.semantic)
library(shiny)
library(shinyWidgets)
library(shinythemes)


shinyServer(
  function(session,input, output) {
   
    
    ############## Tab 2: Upload Data Tab ################################
    
    
    #2.1: Upload CSV file. Reactive Dataa     
    #source("files/2.1.reactivedata_server.R", local=TRUE)
    mydata <- read.csv("new_data.csv")
    
    
    
    # Creating dropdown for the ship and vessel types
    
   
    
    output$select_ship <- renderUI({
      
      selectInput("shipname","Ship Name", choices=sort(unique(unlist(mydata$SHIPNAME))))
    })
    
  
    output$select_vessel <- renderUI({
      
      selectInput("vessel", "Ship Type", choices = unique(c(mydata$ship_type[mydata$SHIPNAME == input$shipname])))
      })
      

    
    # leaflet Map
    
    output$map <- renderLeaflet({
      
      df1 <- mydata %>% filter(SHIPNAME == input$shipname) #%>% filter(ship_type ==input$vessel)
      
      pi = 22/7
      
      # converting location co-ordinates into radians
      
      df1$lat_rad <- df1$LAT*pi / 180
      
      df1$long_rad <- df1$LON*pi / 180
      
      # Using Haversine Formula for calculating the distance
      
      df2 <- df1 %>%  mutate(a = {(sin((lat_rad-lag(lat_rad))/2)^2) + cos(lat_rad)*cos(lag(lat_rad)) * (sin((long_rad-lag(long_rad))/2)^2)})
      
      df2$a <- as.numeric(df2$a)
      
      
      df2$a[is.na(df2$a)] <- 0
      
      df2$b <- 1-df2$a
      
      
      df2$as <- sqrt(df2$a)
      
      df2$bs <- sqrt(df2$b)
      
      df2$c = 2*atan2(df2$as,df2$bs)
    
      R <- 6371000  # radius of the earth in meters
      
      df2$distance <- df2$c * R
      
      df3 <- df2[which.max(df2$distance),]
      
      # finding the most recent observation
     
      if(nrow(df3) >1){
        df4 <- df3 %>% 
          slice(which.max(as.Date(date, '%Y-%d-%m')))
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }else{
        df4 <- df3
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }
      
      data <- rbind(ind1,df4)
      
      
      arrowIcon <- makeIcon(
        iconUrl = "ship.png" 
        ,iconWidth = 20, iconHeight = 20 
        ,iconAnchorX = 10, iconAnchorY =10
      )
      
      
      n <- data[1,2]
      m <- data[1,1]
     
      leaflet(data) %>% addTiles() %>% 
      addMarkers(lng= ~LON, lat=~LAT,icon = arrowIcon,popup = ~paste(LAT, LON, sep = '<br/>')) %>% addProviderTiles("Stamen.TonerHybrid")%>% addPolylines(lng = ~LON, lat = ~LAT,color = "red",weight = 1.5) %>% addProviderTiles("Stamen.watercolor") 
    })
    

    
    output$distance_info <- renderText({
      
      
      
      df1 <- mydata %>% filter(SHIPNAME == input$shipname) %>% filter(ship_type ==input$vessel)
      
      pi = 22/7
      
      df1$lat_rad <- df1$LAT*pi / 180
      
      df1$long_rad <- df1$LON*pi / 180
      
      
      df2 <- df1 %>%  mutate(a = {(sin((lat_rad-lag(lat_rad))/2)^2) + cos(lat_rad)*cos(lag(lat_rad)) * (sin((long_rad-lag(long_rad))/2)^2)})
      
      df2$a <- as.numeric(df2$a)
      
      
      df2$a[is.na(df2$a)] <- 0
      
      df2$b <- 1-df2$a
      
      
      df2$as <- sqrt(df2$a)
      
      df2$bs <- sqrt(df2$b)
      
      df2$c = 2*atan2(df2$as,df2$bs)
      
      R <- 6371000
      
      df2$distance <- df2$c * R
      
      df3 <- df2[which.max(df2$distance),]
      
      
      #ind  <- df2 %>% slice(which.max(df2$distance))
      
      
      if(nrow(df3) >1){
        # finding the most recent observation
        df4 <- df3 %>% 
          slice(which.max(as.Date(date, '%Y-%d-%m')))
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }else{
        df4 <- df3
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }
      
      data <- rbind(ind1,df4)

      last <- tail(data,1)
      
      dist <- last$distance
      
     
      paste("Maximum distance covered by",input$shipname,"is",round(dist,2),"meters")
      
    })
    
    
    
    
    output$dest_info <- renderText({
      
      
      
      df1 <- mydata %>% filter(SHIPNAME == input$shipname) %>% filter(ship_type == input$vessel)
      
      
      pi = 22/7
      
      df1$lat_rad <- df1$LAT*pi / 180
      
      df1$long_rad <- df1$LON*pi / 180
      
      
      df2 <- df1 %>%  mutate(a = {(sin((lat_rad-lag(lat_rad))/2)^2) + cos(lat_rad)*cos(lag(lat_rad)) * (sin((long_rad-lag(long_rad))/2)^2)})
      
      df2$a <- as.numeric(df2$a)
      
      
      df2$a[is.na(df2$a)] <- 0
      
      df2$b <- 1-df2$a
      
      
      df2$as <- sqrt(df2$a)
      
      df2$bs <- sqrt(df2$b)
      
      df2$c = 2*atan2(df2$as,df2$bs)
      
      
      R <- 6371000
      
      df2$distance <- df2$c * R
      
      
      
      df3 <- df2[which.max(df2$distance),]
      
      
      #ind  <- df2 %>% slice(which.max(df2$distance))
      
      
      if(nrow(df3) >1){
        # finding the most recent observation
        df4 <- df3 %>% 
          slice(which.max(as.Date(date, '%Y-%d-%m')))
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }else{
        df4 <- df3
        ind1  <- df2 %>% slice(which.max(df2$distance) -1)
      }
      
      data <- rbind(ind1,df4)
      
      last <- tail(data,1)
      
      paste("It departed from",last$port,"and is currently moving towards",last$DESTINATION)
      
    })
    
    
    
    output$speed_details <- renderText({
        
        
      
      df1 <- mydata %>% filter(SHIPNAME == input$shipname) %>% filter(ship_type == input$vessel)
        
        pi = 22/7
        
        df1$lat_rad <- df1$LAT*pi / 180
        
        df1$long_rad <- df1$LON*pi / 180
        
        
        df2 <- df1 %>%  mutate(a = {(sin((lat_rad-lag(lat_rad))/2)^2) + cos(lat_rad)*cos(lag(lat_rad)) * (sin((long_rad-lag(long_rad))/2)^2)})
        
        df2$a <- as.numeric(df2$a)
        
        
        df2$a[is.na(df2$a)] <- 0
        
        df2$b <- 1-df2$a
        
        
        df2$as <- sqrt(df2$a)
        
        df2$bs <- sqrt(df2$b)
        
        df2$c = 2*atan2(df2$as,df2$bs)
        
        
        R <- 6371000
        
        df2$distance <- df2$c * R
        
        
        df3 <- df2[which.max(df2$distance),]
        
        
        #ind  <- df2 %>% slice(which.max(df2$distance))
        
        
        if(nrow(df3) >1){
          # finding the most recent observation
          df4 <- df3 %>% 
            slice(which.max(as.Date(date, '%Y-%d-%m')))
          ind1  <- df2 %>% slice(which.max(df2$distance) -1)
        }else{
          df4 <- df3
          ind1  <- df2 %>% slice(which.max(df2$distance) -1)
        }
        
        data <- rbind(ind1,df4)
        
        dist <- df4$distance
        
        last <- tail(data,1)
        
        if(last$is_parked == 1){
          paste(input$shipname,"Ship is currently parked")
        } else{
          paste(input$shipname,",a",input$vessel,"ship is moving at a speed of",last$SPEED,"knots" )
        }
        
    })
        
        
        
        
        output$ship_info <- renderText({
          
          
          
          df1 <- mydata %>% filter(SHIPNAME == input$shipname) %>% filter(ship_type == input$vessel)
          
          df1 <- head(df1,1)
          
          paste(input$shipname,"weighs",df1$DWT,"tones and has a length of",df1$LENGTH,"meters" )
        })
        
        
        
    })
      

      
      
      
 