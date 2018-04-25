### peer app ###

# A Shiny web application by Mauricio H. Vancine and Thiago Sanna Freire Silva (2018)
# A product of the Ecosystem Dynamics Observatory
# http://tscanada.wix.com/ecodyn

# 2018-04-25

# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

###-------------------------------------------------------------------------------------###

# memory
rm(list = ls())

# packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(dplyr)
library(digest)
library(leaflet)
library(ggmap)
library(data.table)
library(googlesheets)

# fonts
# https://gallery.shinyapps.io/086-bus-dashboard/
# https://deanattali.com/blog/building-shiny-apps-tutorial/#using-uioutput-to-create-ui-elements-dynamically
# https://deanattali.com/blog/shiny-persistent-data-storage/
# https://rstudio.github.io/leaflet/
# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# https://gist.github.com/daattali/c4db11d81f3c46a7c4a5
# https://ipub.com/shiny-crud-app/
# https://gist.github.com/daattali/c4db11d81f3c46a7c4a5

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



#### shiny app ####
shinyApp(
  
  ### ui
  shinyUI(
    
    dashboardPage(
      dashboardHeader(title = "PEER Mapping"),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        fluidRow(
          column(width = 9, 
                 box(width = NULL, solidHeader = FALSE,
                     leafletOutput("map", height = 450))),
          
          column(width = 3,
                 box(width = NULL, solidHeader = TRUE, 
                     title =  "Land use", 
                     selectInput("class", "Class",
                                 choices = c("", 
                                             "Forest", 
                                             "Water", 
                                             "Wetland", 
                                             "Open area", 
                                             "Urban"),
                                 selected = ""),
                 p(class = "text-muted",
                   paste("Note: choose a land use class and land cover.")))),
          
          column(width = 3,
                 box(width = NULL, title = "Edit points",
                     actionButton("edit", "Edit"),
                     actionButton("remove", "Remove"),
                     p(class = "text-muted",
                       paste("Note: Edit or remove points.")))),
          
          column(width = 3, 
                 box(width = NULL, title = "Submit",
                     actionButton("submit", "Submit"),
                     p(class = "text-muted",
                       paste("Note: Submit data.")))),
          
          column(width = 12, 
                 box(width = NULL, title =  "Data", solidHeader = TRUE,
                     DT::dataTableOutput("responses")))
      )
      )
      )
    ),
  
  ### server
  shinyServer(
    function(input, output){
      
      # map 
      output$map <- renderLeaflet({
        leaflet() %>% 
          
          # add two tiles
          addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
          addTiles(group = "Basemap") %>%
          setView(-57, -2, zoom = 6) %>%
          addLayersControl(
            baseGroups = c("Satelite", "Basemap"), 
            options = layersControlOptions(collapsed = FALSE))
        
      })
      
      # store the click
      observeEvent(input$map_click, {
        leafletProxy("map") %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = input$map_click$lng, 
                           lat = input$map_click$lat,
                           label = paste(input$id, "-", input$class),
                           labelOptions = labelOptions(noHide = TRUE))
      })
      
      
      # whenever a field is filled, aggregate all form data
      formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data <- c(data, lon = round(input$map_click[[2]], 4), 
                  lat = round(input$map_click[[1]], 4))
      })
      
      # when the submit button is clicked, save the form data
      observeEvent(input$submit, {
        saveData(formData())
      })
      
      # show the previous responses
      output$responses <- DT::renderDataTable({
        input$submit
        loadData()
      })
      
    }
  )
)
