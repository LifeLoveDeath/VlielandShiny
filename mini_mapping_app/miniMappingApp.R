# Mini mapping app

# A standalone app where you can enter nestbox numbers and see them in a map
# Created it to figure out the mapping but kept it in case it’s useful for fieldwork


# Load packages
# Required packages
required_packages <- c("shiny", "tidyverse", "osmdata", "leaflet")

# Install any missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(required_packages, library, character.only = TRUE)


#Load data
data <- read.csv("data/Coordinates_Boxes_Vlieland.csv", row.names = NULL)



# Checking nestbox locations and basic mapping
#m <- leaflet() %>% addTiles() %>% # adds default OpenStreetMap map tiles 
#  setView(lng = 5.018424, lat = 53.286226, zoom = 11) # got long and lat from google maps - sets the view to be on Vlieland

#m %>% addMarkers(
#  lng = data$Lon,
#  lat = data$Lat)

#They're all in the north, so recentre map
#was: lng = 4.960574, lat = 53.264568
#try 53.286226, 5.018424





# UI function
mappingUIFunction <- function() {
  nestBoxes <- unique(data$Nestbox)
  tagList(
    fluidRow(
      column(5,
             selectizeInput(
               "Box", "Nestbox", 
               choices = nestBoxes,
               multiple = TRUE
             )
      ),
      column(4,
             div(style = "margin-top: 25px;",
                 actionButton("clear_button", "Clear Selection")
             )
      )
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
  mappingUIFunction(),
  
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

