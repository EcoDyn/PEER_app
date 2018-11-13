### peer app ###

# A Shiny web application by Mauricio H. Vancine and Thiago Sanna Freire Silva (2018)
# A product of the Ecosystem Dynamics Observatory
# http://tscanada.wix.com/ecodyn

# 2018-09-12

# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

###-------------------------------------------------------------------------------------###

# memory
rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

# packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(data.table)
library(DT)
library(tidyverse)
library(leaflet)
library(googlesheets)

###-----------------------------------------------------------------------------------###

# load responses
responses.gs.title <- gs_title("responses")
responses.gs <- gs_read(responses.gs.title)

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
                   colors = c("green", "lightgreen", "cadetblue",
                              "orange", "beige", "black", "red", "gray", 
                              "blue", "darkblue", "lightblue"))


#### shiny app ####
shinyApp(
  
  ### ui
  shinyUI(
    
    # page
    dashboardPage(title = "PEER Mapping",
                  
                  # title
                  dashboardHeader(title = "Projeto PEER"),
                  
                  # side bar
                  dashboardSidebar(
                    sidebarMenu(
                      
                      # menu information
                      menuItem("Instruções", 
                               tabName = "information",
                               icon = icon("info-circle")),
                      
                      # menu mapping
                      menuItem("Seleção de Amostras", 
                               tabName = "mapping",
                               icon = icon("globe")),
                      
                      # menu review
                      menuItem("Revisão", 
                               tabName = "review",
                               icon = icon("eye")),
                    )),
                  
                  
                  # body
                  dashboardBody(
                    tabItems(
                      
                      ## information
                      tabItem(tabName = "information",
                              
                              # introduction
                              column(width = 12, 
                                     fluidRow(
                                       box(title = "Instruções", 
                                           width = NULL, 
                                           includeMarkdown("instructions.md"))))),
                      
                      ## mapping
                      tabItem(tabName = "mapping",
                              
                              # map
                              column(width = 8,
                                     box(width = NULL,
                                         leafletOutput("map", height = 600))),
                              
                              # class, data and finish
                              column(width = 4,
                                     fluidRow(
                                       
                                       # choose class
                                       box(width = NULL,
                                           title = "Escolha a classe a ser identificada:",
                                           selectInput("class",
                                                       "(Antes de clicar, aproxime o zoom até 200 m!)",
                                                       choices = usecolors[, 1])),
                                       
                                       # data
                                       box(width = NULL, 
                                           title =  "Pontos Coletados",
                                           dataTableOutput("data")),
                                       
                                       # send data to review
                                       box(width = NULL, 
                                           title =  "Clique aqui para finalizar (não é possível adicionar pontos depois!)",
                                           actionButton("finish", "Finalizar!", width = "100%"))))),
                      
                      ## review
                      tabItem(tabName = "review",
                              
                              # map
                              column(width = 8,
                                     box(width = NULL,
                                         leafletOutput("map_review", height = 600))),
                              
                              # submit
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Plotar pontos ou finalizar envio de dados",
                                           actionButton("submit", "Enviar dados!", width = "49%"),
                                           actionButton("plot", "Redesenhar pontos", width = "49%")))),
                              
                              # data
                              column(width = 4,
                                     fluidRow(
                                       box(width = NULL, 
                                           title =  "Revise sua seleção e remova pontos se necessário.",
                                           helpText("Após remover pontos, clique em 'Redesenhar pontos' para atualizar o mapa.
                                                    Ao finalizar, clique em 'Enviar Dados'."),
                                           dataTableOutput("data_review"),
                                           tags$script("$(document).on('click', '#data_review button', function () {
                                                       Shiny.onInputChange('lastClickId',this.id);
                                                       Shiny.onInputChange('lastClick', Math.random()) });"))))),
                      
                      ## stats
                      tabItem(tabName = "stats",
                              
                              # title
                              column(width = 12, 
                                     fluidRow(
                                       box(title = "Stats", 
                                           width = NULL, 
                                           "Como estamos progredindo?"))),
                              
                              # stats
                              column(width = 12, 
                                     fluidRow(
                                       valueBox(nrow(responses.gs), 
                                                "Responses", 
                                                width = 12,
                                                color = "maroon",
                                                icon = icon("globe")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[1, 1]), 2]), 
                                                as.character(usecolors[1, 1]), 
                                                width = 3,
                                                color = "green",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[2, 1]), 2]), 
                                                as.character(usecolors[2, 1]), 
                                                width = 3,
                                                color = "olive",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[3, 1]), 2]), 
                                                as.character(usecolors[3, 1]), 
                                                width = 3,
                                                color = "teal",
                                                icon = icon("map-marker")),
                                        valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[5, 1]), 2]), 
                                                as.character(usecolors[4, 1]), 
                                                width = 3,
                                                color = "orange",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[6, 1]), 2]), 
                                                as.character(usecolors[5, 1]), 
                                                width = 3,
                                                color = "yellow",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[7, 1]), 2]), 
                                                as.character(usecolors[6, 1]), 
                                                width = 3,
                                                color = "black",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[8, 1]), 2]), 
                                                as.character(usecolors[7, 1]), 
                                                width = 3,
                                                color = "yellow",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[9, 1]), 2]), 
                                                as.character(usecolors[8, 1]), 
                                                width = 3,
                                                color = "black",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[10, 1]), 2]), 
                                                as.character(usecolors[9, 1]), 
                                                width = 3,
                                                color = "light-blue",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[11, 1]), 2]), 
                                                as.character(usecolors[10, 1]), 
                                                width = 3,
                                                color = "navy",
                                                icon = icon("map-marker")),
                                       valueBox(nrow(responses.gs[responses.gs$class == as.character(usecolors[1, 1]), 2]), 
                                                as.character(usecolors[11, 1]), 
                                                width = 3,
                                                color = "blue",
                                                icon = icon("map-marker"))))))
                                     )
                              )
  ),
  
  ### server
  shinyServer(
    
    function(input, output, session){
      
      ### mapping 
      ## icon
      icons.response.gs <- awesomeIcons(icon = "check-circle", 
                                        library = "fa", 
                                        markerColor = as.character(usecolors[usecolors[, 1] == responses.gs$class, 2]))
      
      ## map 
      output$map <- renderLeaflet({
        
        # add map
        leaflet() %>% 
          
          # add tiles
          addTiles() %>%
          
          # specif the tiles
          addProviderTiles("Esri.WorldImagery", 
                           group = "Satelite",
                           options = providerTileOptions(minZoom = 5)) %>%
          
          # layer control
          addLayersControl(
            baseGroups = c("Satelite", "Basemap"), 
            options = layersControlOptions(collapsed = TRUE)) %>%
          
          # zoom
          setView(-56, -6, zoom = 5) %>%
          
          # back zoom
          addEasyButton(easyButton(
            icon = "fa-globe",
            title = "Back zoom",
            onClick = JS("function(btn, map){ map.setView([-4, -56], 5); }"))) %>%
          
          # rectangle
          addRectangles(
            lng1 = -68, lat1 = 0.5,
            lng2 = -48, lat2 = -6,
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
                    labels = usecolors[, 1], 
                    colors = usecolors[, 2],
                    title = "Land use") %>% 
          
          # sample markers
          addAwesomeMarkers(lng = responses.gs$lon, 
                            lat = responses.gs$lat,
                            label = paste0(responses.gs$id, "-", responses.gs$class),
                            labelOptions = labelOptions(noHide = FALSE, opacity = .5),
                            icon = icons.response.gs)
      })
      
      
      ## click counter
      counter <- reactiveValues(countervalue = 0)
      
      ## store the click
      observeEvent(input$map_click, {
        
        counter$countervalue <- counter$countervalue + 1
        
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
        
        saveData(data.i())
        
        icons.click.map <- awesomeIcons(icon = "circle",
                                        library = "fa",
                                        iconColor = "gray",
                                        markerColor = as.character(usecolors[usecolors[, 1] == input$class, 2]))
        
        leafletProxy("map") %>%
          addAwesomeMarkers(lng = input$map_click$lng,
                            lat = input$map_click$lat,
                            label = paste0(counter$countervalue, " - ", input$class),
                            labelOptions = labelOptions(noHide = FALSE, opacity = .5),
                            icon = icons.click.map)
      })
      
      # show the previous responses
      output$data <- renderDataTable({
        input$map_click
        datatable(loadData(),
                  rownames = FALSE,
                  options = list(searching = FALSE, pageLength = 4,
                                 order = list(list(2, "desc"))),
                  class = "cell-border stripe")
      })
      
      
      ### review 
      
      ## map 
      output$map_review <- renderLeaflet({
        
        # add map
        leaflet() %>% 
          
          # add tiles
          addTiles() %>%
          
          # specif the tiles
          addProviderTiles("Esri.WorldImagery", 
                           group = "Satelite",
                           options = providerTileOptions(minZoom = 5)) %>%
          
          # layer control
          addLayersControl(
            baseGroups = c("Satelite", "Basemap"), 
            options = layersControlOptions(collapsed = TRUE)) %>%
          
          # zoom
          setView(-56, -6, zoom = 5) %>%
          
          # back zoom
          addEasyButton(easyButton(
            icon = "fa-globe",
            title = "Back zoom",
            onClick = JS("function(btn, map){ map.setView([-4, -56], 5); }"))) %>%
          
          # rectangle
          addRectangles(
            lng1 = -68, lat1 = 0.5,
            lng2 = -48, lat2 = -6,
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
                    labels = usecolors[, 1], 
                    colors = usecolors[, 2],
                    title = "Land use")
      })
      
      
      
      ## review data
      vals <- reactiveValues()
      
      observeEvent(input$finish, {
        req(input$finish)
        vals$Data <- data.table(loadData())
        showModal(modalDialog(
          title = "Fim da coleta",
          "Coleta finalizada, vá para a aba 'Revisão'!"
        ))
      })
      
      output$data_review <- renderDataTable({
        input$finish
        req(input$finish)
        DT <- vals$Data
        DT[["actions"]] <- paste0('<div class="btn-group" role="group" aria-label="Basic example">
                                  <button type="button" class="btn btn-secondary delete" id=delete_', 1:nrow(vals$Data), '>Delete</button>
                                  </div>')
        datatable(DT, rownames = FALSE, class = "cell-border stripe", escape = FALSE, 
                  options = list(searching = FALSE, pageLength = 5, order = list(list(2, "desc"))))
      })
      
      observeEvent(input$lastClick,
                   {
                     if(input$lastClickId %like% "delete")
                     {
                       row_to_del <- as.numeric(gsub("delete_", "", input$lastClickId))
                       vals$Data <- vals$Data[-row_to_del]
                     }
                   }
      )
      
      ## icons in map
      observeEvent(input$plot, {
        req(input$plot)
        
        leafletProxy(map = "map_review", data = data.table(vals$Data)) %>%
          clearMarkers() %>%
          addCircleMarkers(
            lng = ~as.numeric(as.character(lon)),
            lat = ~as.numeric(as.character(lat)),
            label = ~as.character(paste0(id, " - ", class)),
            color = ~left_join(data.table(use = as.character(vals$Data$class)), data.table(usecolors))[, 2],
            opacity = .7,
            fill = TRUE,
            fillColor = "white",
            fillOpacity = 0,
            weight = 5,
            radius = 8,
            labelOptions = labelOptions(noHide = TRUE, opacity = .5))
        
      })
      
      ## submit data
      dataModal <- function(failed = FALSE){
        
        modalDialog(
          
          title = "Preencha suas informações, para que possamos entrar em contato em caso de dúvidas!",
          easyClose = TRUE,
          
          textInput("name", "Nome", placeholder = "Seu nome"),
          textInput("email", "E-mail", placeholder = "Seu e-mail"),
          textInput("institution", "Instituição", placeholder = "Sua instituição"),
          
          span("Depois de clicar 'OK', aguarde! O envio pode levar alguns segundos."),
          
          if(failed)
            div(tags$b("Por favor preencha todos os campos!", 
                       style = "color: red;")),
          
          footer = tagList(
            modalButton("Cancelar"),
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
          
          da.gs <- gs_new(paste(input$name, input$email, input$institution,
                                sub(" ", "_", gsub(":", "-", as.character(Sys.time())))),
                          ws_title = paste(input$name, input$email, input$institution, sep = "_"),
                          input = as.data.table(vals$Data),
                          trim = TRUE,
                          verbose = FALSE) %>%
            gs_read()
          
          removeModal()
          
          showModal(modalDialog(
            title = "Dados Enviados",
            "Os dados foram enviados com sucesso! Muito obrigado por sua contribuição!"
          ))
          
        } else {
          showModal(dataModal(failed = TRUE))
        }
        
      })
      
      # stop app when close browser
      session$onSessionEnded(function() {
        stopApp()
      })
      
    })
  
  )


###--------------------------------------------------------------------------------###
