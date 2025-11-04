
# Mapping preview module

# Probably need a separate dataframe in long format for nestboxes associated with ring numbers

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
  
  observeEvent(input$summary_info_rows_selected, {
    if (length(input$summary_info_rows_selected) > 0) {
      ring <- search_results()[input$summary_info_rows_selected, "RingNumber"]
      last_ring(ring)
    }
  })
  
  # Show map if a row has ever been selected
  output$map_preview_ui <- renderUI({
    req(last_ring())
    leafletOutput("map_preview", width = "95%", height = "600px")
  })
  
  output$map_preview <- renderLeaflet({
    req(search_results(), last_ring())
    
    # Filter location.data by selected ring, take last location
    filtered_data <- location.data %>%
      filter(RingNumber == last_ring()) %>%
      arrange(desc(Year), desc(Month)) %>%
      slice(1)
    
    # Default map coordinates
    default_lat <- 53.286226
    default_lng <- 5.018424
    default_zoom <- 12
    
    # Build base map
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addTiles() %>%
      setView(lng = default_lng, lat = default_lat, zoom = default_zoom) %>%
      htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
      
      # Add title
      addControl(
        html = paste0("<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>",
                      filtered_data$RingNumber, " Last recorded location</div>"),
        position = "topleft"
      ) %>%
      
      #Add reset zoom button
      addEasyButton(
        easyButton(
          icon = fontawesome::fa("crosshairs"),
          title = "Reset zoom",
          onClick = JS(sprintf("function(btn, map){ map.setView([%s, %s], %s); }",
                               default_lat, default_lng, default_zoom))
        )
      )
    
    
    # Add control or message if no location data
    if (nrow(filtered_data) == 0 || is.na(filtered_data$NestLon) || is.na(filtered_data$NestLat)) {
      m <- m %>%
        addControl(
          html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data</div>",
          position = "topleft"
        )
    } else {
      # Add marker for the last location
      m <- m %>%
        addCircleMarkers(
          lng = filtered_data$NestLon,
          lat = filtered_data$NestLat,
          label = paste0(filtered_data$Month, " ", filtered_data$Year, ": ", filtered_data$Event),
          color = "#1b0c41",
          fillOpacity = 0.6,
          opacity = 1,
          radius = 8,
          weight = 2
        ) 
    }
    
    m
  })
}
