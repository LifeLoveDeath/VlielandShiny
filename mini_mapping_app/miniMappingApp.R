# Mini mapping app

# Load packages
library(shiny)
library(tidyverse)
library(osmdata)
library(leaflet)


#Load data
data <- read.csv("data/Coordinates_Boxes_Vlieland.csv", row.names = NULL)

#They're all in the north, so recentre map
#was: lng = 4.960574, lat = 53.264568
#try 53.286226, 5.018424


#m <- leaflet() %>% addTiles() %>% # adds default OpenStreetMap map tiles 
#  setView(lng = 5.018424, lat = 53.286226, zoom = 11) # got long and lat from google maps - sets the view to be on Vlieland

#m %>% addMarkers(
#  lng = data$Lon,
#  lat = data$Lat)







# UI function
mappingUIFunction <- function() {
  nestBoxes <- unique(data$Nestbox)
  tagList(
    selectizeInput(
      "Box", "Nestbox", 
      choices = nestBoxes,
      multiple = TRUE
    )
  )
}


# Sever function
mappingFunction <- function(input, output, data, session) {
  
  output$map <- renderLeaflet({
    # Base map
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
      addTiles() %>% 
      setView(lng = 5.018424, lat = 53.286226, zoom = 13) %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ")
    
    # If a selection is made
    if (!is.null(input$Box) && length(input$Box) > 0) {
      # Filter data for selected nest boxes
      filtered_data <- data[data$Nestbox %in% input$Box, ]
      
      # Add markers
      m <- m %>% addMarkers(
        lng = filtered_data$Lon,
        lat = filtered_data$Lat,
        label = filtered_data$Nestbox
      )
    }
    
    m
  })
  # Clear button observer
  observeEvent(input$clear_button, {
    updateSelectizeInput(session, "Box", selected = character(0))
  })
}




# UI definition
ui_mapping <- fluidPage(
  titlePanel("Mapping nest boxes"),
  
  # search bar and print button
  fluidRow(
    column(5, mappingUIFunction()),
    column(6,
           div(style = "margin-top: 25px; text-align: left;",
               actionButton("clear_button", "Clear Selection")
           )
    )
  ),
  
  # map area
  fluidRow(
    column(12, leafletOutput("map", width = "100%", height = "600px"))
  )
)



# Server definition
server_mapping <- function(input, output, session) {
  mappingFunction(input, output, data, session)
}


shinyApp(ui = ui_mapping, server = server_mapping)

