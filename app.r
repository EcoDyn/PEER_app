### peer app ###

# A Shiny web application by Mauricio H. Vancine and Thiago Sanna Freire Silva (2018)
# A product of the Ecosystem Dynamics Observatory
# http://tscanada.wix.com/ecodyn

# 2018-08-10

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
library(googlesheets)

###-----------------------------------------------------------------------------------###

# load samples
samples <- read_csv("samples.csv")
samples

# load responses
responses.table.total <- gs_title("responses")
responses.total <- gs_read(responses.table.total)
responses.total <- rbind(samples, responses.total)

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
                                                               choices = usecolors[, 1]))),
                              
                              # submit
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Submit data",
                                           actionButton("submit", "Submit", width = "100%")))),
                              
                              # data
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Data",
                                           dataTableOutput("responses"))))),
                      
                      # stats
                      tabItem(tabName = "accountants",
                              
                              # introduction
                              column(width = 12, 
                                     fluidRow(
                                       box(title = "Stats", 
                                           width = NULL, 
                                           "How are we doing?"))),
                              
                              # stats
                              column(width = 12, 
                                     fluidRow(
                                       valueBox(nrow(responses.total), 
                                                "Samples", 
                                                color = "maroon",
                                                icon = icon("globe")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[1, 1]), 2]), 
                                                as.character(usecolors[1, 1]), 
                                                color = "green",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[2, 1]), 2]), 
                                                as.character(usecolors[2, 1]), 
                                                color = "green",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[3, 1]), 2]), 
                                                as.character(usecolors[3, 1]), 
                                                color = as.character(usecolors[3, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[4, 1]), 2]), 
                                                as.character(usecolors[4, 1]), 
                                                color = "yellow",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[5, 1]), 2]), 
                                                as.character(usecolors[5, 1]), 
                                                color = as.character(usecolors[5, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[6, 1]), 2]), 
                                                as.character(usecolors[6, 1]), 
                                                color = as.character(usecolors[6, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[7, 1]), 2]), 
                                                as.character(usecolors[7, 1]), 
                                                color = as.character(usecolors[7, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[8, 1]), 2]), 
                                                as.character(usecolors[8, 1]), 
                                                color = "black",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[9, 1]), 2]), 
                                                as.character(usecolors[9, 1]), 
                                                color = as.character(usecolors[9, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[10, 1]), 2]), 
                                                as.character(usecolors[10, 1]), 
                                                color = as.character(usecolors[10, 2]),
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.total[responses.total$class == as.character(usecolors[11, 1]), 2]), 
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
                    title = "Land use") %>% 
          
          # sample markers
          clearMarkerClusters() %>%
          addCircleMarkers(lng = samples$lon, 
                           lat = samples$lat,
                           label = paste0(samples$id, "-" , samples$class),
                           labelOptions = labelOptions(noHide = TRUE, opacity = .5), 
                           radius = 7,
                           weight = 2.5,
                           color =  as.character(usecolors[usecolors[, 1] == samples$class, 2]),
                           opacity = .7,
                           fillColor = "black",
                           fillOpacity = .5)
      })
      
      # click counter
      counter <- reactiveValues(countervalue = nrow(samples)) # Defining & initializing the reactiveValues object
      
      observeEvent(input$map_click, {
        counter$countervalue <- counter$countervalue + 1     # if the add button is clicked, increment the value by 1 and update it
      })
      
      ## store the click
      observeEvent(input$map_click, {
        leafletProxy("map") %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = input$map_click$lng, 
                           lat = input$map_click$lat,
                           label = paste0(counter$countervalue, "-", input$class),
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
        data <- c(
          id = counter$countervalue,
          data, 
          lon = round(input$map_click[[2]], 4), 
          lat = round(input$map_click[[1]], 4))
      })
      
      # when a click is made on the map, the data is saved in the form
      observeEvent(input$map_click, {
                saveData(data.i())
      })
      
      # show the previous responses
      output$responses <- renderDataTable({
        input$map_click
        datatable(rbind(samples, loadData()),
                  editable = TRUE,
                  rownames = FALSE,
                  options = list(searching = FALSE, 
                                 #displayStart = trunc((counter$countervalue + nrow(samples))/5) + 1,
                                 pageLength = 8),
                  class = "cell-border stripe")
        })
      
      # submit data
      dataModal <- function(failed = FALSE){
        modalDialog(
          
          title = "Input information about your session",
          easyClose = TRUE,
          
          textInput("name", "Name", placeholder = "Your name"),
          textInput("email", "E-mail", placeholder = "Your email"),
          textInput("institution", "Institution", placeholder = "Your institution"),
          
          if(failed)
            div(tags$b("Please fill all the fields", style = "color: red;")),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        )
      }
      
      # show modal when button is clicked
      observeEvent(input$submit, {
        showModal(dataModal())
      })
      
      # when OK button is pressed, attempt to send the data set
      observeEvent(input$ok, {
        
        # check that data object exists and is data frame
        if(nzchar(input$name) && nzchar(input$email) && nzchar(input$institution)) {
          
          try(
            boring_ss <- gs_new(paste(input$name, input$email, input$institution, 
                                    sub(" ", "-", gsub(":", "_", gsub("-", "_", as.character(Sys.time())))), 
                                    sep = "-"), 
                              ws_title = paste(input$name, input$email, input$institution, sep = "_"), 
                              input = rbind(samples, loadData()),
                              trim = TRUE, 
                              verbose = FALSE),
          boring_ss %>% 
            gs_read()
          )
          
          removeModal()
          
          showModal(modalDialog(
            title = "Send data",
            "The data was send! Thank's for your help!"
          ))
          
        } else {
          showModal(dataModal(failed = TRUE))
        }
        
      })
 
    }
  )
)

###--------------------------------------------------------------------------------###