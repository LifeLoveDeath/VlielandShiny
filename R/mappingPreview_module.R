
# ==========================================================
# Preview Map Module
# ==========================================================
# This module provides a preview map showing the last recorded
# location of a bird selected in the search results table.

# Inputs:
#   - search_results: reactive expression returning filtered bird table
#   - location.data: data frame of bird locations with columns:
#       RingNumber, Year, Month, NestLon, NestLat, Event
#   - input$summary_info_rows_selected: row selection in the search table
#
# Outputs:
#   - map_preview_ui: UI output for the leaflet map
#   - map_preview: rendered Leaflet map showing last location of selected bird


# Load packages - moved to app.r
# library(shiny)
# library(leaflet)
# library(bslib)
# library(viridis)
# library(dplyr)
# library(reactable)

# UI ----------------------------------------------------

# # Server ------------------------------------------------

genPreviewMapServer <- function(input, output, search_results, location.data, session) {
  
  # store the last selected RingNumber
  last_ring <- reactiveVal(NULL)
  
  # Update last selected ring based on table selection
  observeEvent(input$summary_info_rows_selected, {
    if (length(input$summary_info_rows_selected) > 0) {
      ring <- search_results()[input$summary_info_rows_selected, "RingNumber"]
      last_ring(ring)
    }
  })
  
  # Show map only if a bird has been selected
  output$map_preview_ui <- renderUI({
    req(last_ring())
    leafletOutput("map_preview", width = "95%", height = "600px")
  })
  
  # Render Leaflet map
  output$map_preview <- renderLeaflet({
    req(last_ring(), search_results())
    loc_data <- get_last_location(location.data, last_ring())
    build_preview_map(loc_data)
  })
}
