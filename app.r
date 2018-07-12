### peer app ###

# A Shiny web application by Mauricio H. Vancine and Thiago Sanna Freire Silva (2018)
# A product of the Ecosystem Dynamics Observatory
# http://tscanada.wix.com/ecodyn

# 2018-07-11

# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

###-------------------------------------------------------------------------------------###

# memory
rm(list = ls())

# packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(tidyverse)
library(leaflet)
library(data.table)

###-------------------------------------------------------------------------------------###

# fields get saved 
fields <- c("class")

# save a response
saveData <- function(data){
  data <- as.data.frame(t(data))
  
  if(exists("responses")){
    responses <<- rbind(responses, data)
  } else{
    responses <<- data
  }
}

# load all previous responses
loadData <- function(){
  if(exists("responses")){
    responses
  }
}

# use colors
usecolors <- cbind(use = c("Varzea Forest", "Igapo Forest", "Woodlands", "Shrubs", 
                           "Herbaceous", "Urban", "Sand Bank", "Bare Rocks", 
                           "White Water", "Black Water", "Clear Water"),
                   colors = c("darkgreen", "lightgreen", "teal", "orange", 
                              "lime", "black", "yellow", "gray", 
                              "aqua", "blue", "lightblue"))


#### shiny app ####
shinyApp(
  
  ### ui
  shinyUI(
    
    # page
    dashboardPage(title = "PEER Mapping",
                  
                  # title
                  dashboardHeader(title = "PEER Mapping"),
                  
                  # side bar
                  dashboardSidebar(
                    sidebarMenu(
                      
                      # menu information
                      menuItem("Instructions", 
                               tabName = "information",
                               icon = icon("info-circle")),
                      
                      # menu mapping
                      menuItem("Mapping", 
                               tabName = "mapping",
                               icon = icon("globe")),
                      
                      # accountants
                      menuItem("Stats", 
                               tabName = "accountants",
                               icon = icon("bar-chart-o"))
                      
                    )),
                  
                  # body
                  dashboardBody(
                    tabItems(
                      
                      # information
                      tabItem(tabName = "information",
                              
                              # introduction
                              column(width = 12, 
                                     fluidRow(
                                       box(title = "Instructions", 
                                           width = NULL, 
                                           includeMarkdown("instructions.md"))))
                              
                              # identification
                              # column(width = 12, 
                              #        fluidRow(
                              #          box(width = NULL,
                              #              title =  "User", 
                              #              splitLayout(
                              #                textInput("name", "Name", ""),
                              #                textInput("email", "E-mail", ""),
                              #                textInput("institution", "Institution", "")))))
                      ),
                      
                      # mapping
                      tabItem(tabName = "mapping",
                              
                              # map
                              column(width = 8,
                                     box(width = NULL,
                                         leafletOutput("map", height = 600)),
                                     
                                     absolutePanel(id = "controls",
                                                   class = "panel panel-default",
                                                   style = "padding-left: 5px; padding-right: 5px; padding-top: 5px; padding-bottom: 5px; opacity: 0.7; background: LightGray; border: gray",
                                                   fixed = TRUE,
                                                   draggable = TRUE,
                                                   top = 80, 
                                                   left = "auto", 
                                                   right = 570, 
                                                   bottom = "auto",
                                                   width = 250, 
                                                   height = 130,
                                                   
                                                   h2("Choose a land use"),
                                                   selectInput("class", 
                                                               "Class",
                                                               choices = c("", 
                                                                           usecolors[, 1]),
                                                               selected = ""))),
                              
                              # erase-submit
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Editing and submiting data",
                                           actionButton("remove", "Remove", width = "45%"),
                                           actionButton("submit", "Submit", width = "45%")))),
                              
                              # data
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Data",
                                           dataTableOutput("responses"))))),
                      
                      # accountants
                      tabItem(tabName = "accountants",
                              
                              # introduction
                              column(width = 12, 
                                     fluidRow(
                                       box(title = "Stats", 
                                           width = NULL, 
                                           "How are we doing?"))),
                              
                              # accountants
                              column(width = 12, 
                                     fluidRow(
                                       valueBox(0, 
                                                "Samples", 
                                                color = "maroon",
                                                icon = icon("globe")),
                                       valueBox(0, 
                                                as.character(usecolors[1, 1]), 
                                                color = "green",
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[2, 1]), 
                                                color = "green",
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[3, 1]), 
                                                color = as.character(usecolors[3, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[4, 1]), 
                                                color = "yellow",
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[5, 1]), 
                                                color = as.character(usecolors[5, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[6, 1]), 
                                                color = as.character(usecolors[6, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[7, 1]), 
                                                color = as.character(usecolors[7, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[8, 1]), 
                                                color = "black",
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[9, 1]), 
                                                color = as.character(usecolors[9, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[10, 1]), 
                                                color = as.character(usecolors[10, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(0, 
                                                as.character(usecolors[11, 1]), 
                                                color = "light-blue",
                                                icon = icon("map-marker"))))
                      )
                    )
                  )
    )
  ),
  
  ### server
  shinyServer(
    
    function(input, output, session){
      
      ## map 
      output$map <- renderLeaflet({
        
        leaflet() %>% 
          
          addTiles() %>%
          
          # add two tiles
          addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
          
          # layer control
          addLayersControl(
            baseGroups = c("Satelite", "Basemap"), 
            options = layersControlOptions(collapsed = TRUE)) %>%
          
          # zoom
          setView(-63, -4, zoom = 6) %>%
          
          # back zoom
          addEasyButton(easyButton(
            icon = "fa-globe",
            title = "Back zoom",
            onClick = JS("function(btn, map){ map.setView([-5, -63], 6); }"))) %>%
          
          # rectangle
          addRectangles(
            lng1 = -68, lat1 = -1,
            lng2 = -58, lat2 = -6,
            fillColor = "transparent") %>%
          
          # minimap
          addMiniMap(
            tiles = providers$Esri.WorldStreetMap,
            position = "bottomleft", 
            zoomLevelOffset = -4,
            toggleDisplay = TRUE) %>%
          
          # scalebar
          addScaleBar(
            position = "bottomleft") %>%
          
          # legend
          addLegend("bottomright", 
                    colors = usecolors[,2], 
                    labels = usecolors[, 1], 
                    title = "Land use")
        
      })
      
      ## store the click
      observeEvent(input$map_click, {
        leafletProxy("map") %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = input$map_click$lng, 
                           lat = input$map_click$lat,
                           label = paste(input$class),
                           labelOptions = labelOptions(noHide = TRUE, opacity = 0.5), 
                           radius = 7,
                           weight = 2.5,
                           color =  as.character(usecolors[usecolors[, 1] == input$class, 2]),
                           opacity = .7,
                           fillColor = "black",
                           fillOpacity = .5)
      })
      
      # whenever a field is filled, aggregate all form data
      data.i <- reactive({
        
        req(input$class)
        req(input$map_click)
        
        data <- sapply(fields, function(x) input[[x]])
        data <- c(data, 
                  lon = round(input$map_click[[2]], 4), 
                  lat = round(input$map_click[[1]], 4))
      })
      
      # show the previous responses
      output$responses <- renderDataTable({
        datatable(saveData(data.i()),
                  loadData(), 
                  options = list(searching = FALSE, 
                                 lengthChange = TRUE))
      })
      
    }
  )
)
