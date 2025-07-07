# Vlieland Shiny app
# Server functions to create map for selected bird


# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)


gen_map <- function(input, output, data, session) {
  
  # Show map if a row is selected
  output$map_ui <- renderUI({
    req(input$summary_info_rows_selected)  # Only render if a row is selected
    leafletOutput("map", width = "95%", height = "600px")
  })
  
  # baseline map to test (but don't show if no birds selected?)
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
  })
}

