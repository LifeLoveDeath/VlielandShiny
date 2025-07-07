# Vlieland Shiny app
# Server functions to create map for selected bird


# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)


gen_map <- function(input, output, search_results, session) { # data intput is search_results from the individual search server function. Is it right to do it like this or should it stil be data?
  
  # Show map if a row is selected
  output$map_ui <- renderUI({
    req(input$summary_info_rows_selected)  # Only render if a row is selected
    leafletOutput("map", width = "95%", height = "600px")
  })
  
  # baseline map to test
    output$map <- renderLeaflet({
      # Check there is data
      req(search_results()) # i.e. search_results not NULL                
      req(input$summary_info_rows_selected) # a row has been selected
      
      # Base map
      m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
        addTiles() %>% 
        setView(lng = 5.018424, lat = 53.286226, zoom = 13) %>%
        htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ")
      
      # Filer data based on row selection
      filtered_data <- search_results()[input$summary_info_rows_selected, ]
      
      # Check location ingo exists:
      validate(
        need(!is.null(filtered_data$OriginNestLon), "No location data"),
        need(!is.null(filtered_data$OriginNestLat), "No location data")
      )
      
      # Add markers
      m <- m %>% addMarkers(
        lng = filtered_data$OriginNestLon,
        lat = filtered_data$OriginNestLat,
        label = filtered_data$OriginNestbox
      )
    
    m
    })
}

