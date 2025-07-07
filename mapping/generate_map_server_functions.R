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
    
    
    #observeEvent(input$summary_info_rows_selected, {
    #  row <- input$summary_info_rows_selected
    #  if (is.null(row)) return()
      
    #  selected_ind <- data[row, "RingNumber"]
}