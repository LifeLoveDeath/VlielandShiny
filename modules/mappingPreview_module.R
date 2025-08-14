
# Mapping preview module

# Probably need a separate dataframe in long format for nestboxes associated with ring numbers

library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)

# UI ----------------------------------------------------

# Server ------------------------------------------------

# this could also be where they were last seen?
# this should maybe go in birdFinderServer

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
    #req(input$summary_info_rows_selected)  # Only render if a row is selected
    req(last_ring()) # Render if a row has ever been selected
    leafletOutput("map_preview", width = "95%", height = "600px")
  })
  
  # baseline map to test
  output$map_preview <- renderLeaflet({
    # Check there is data
    req(search_results()) # i.e. search_results not NULL                
    #req(input$summary_info_rows_selected) # a row has been selected
    req(last_ring()) # a row has ever been selected
    
    # Filer data based on row selection
    #filtered_data <- search_results()[input$summary_info_rows_selected, ]
    # Should actually filter a second nestboxes dataframe based on bird ring number here
    # Find the RingNumber from the clicked row
    #ring <- search_results()[input$summary_info_rows_selected, "RingNumber"]
        
    # Filter location.data by this RingNumber
    #filtered_data <- location.data[location.data$RingNumber == ring & location.data$Event == "birth", ] # if plotting birth
    #print(filtered_data)  # debugging
    filtered_data <- location.data %>%
      filter(RingNumber == last_ring()) %>%
      arrange(desc(Year), desc(Month)) %>%
      slice(1) # if last location
    
    
    # If no location data, show message on map
    # Need to check this properly deals with missing data - introduce a row with missing data
    if (nrow(filtered_data) == 0 || is.na(filtered_data$NestLon) || is.na(filtered_data$NestLat)) {
      return(
        leaflet() %>%
          addTiles() %>%
          addControl(
            html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data</div>",
            position = "topleft"
          ) %>%
          setView(lng = 5.018424, lat = 53.286226, zoom = 12)
      )
    }
    
    
    # Create map with marker
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
      addTiles() %>% 
      addControl(
        html = paste0("<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>", as.character(filtered_data$RingNumber), " Last recorded location","</div>"), # change this title depending on what we're plotting
        position = "topleft"
      ) %>%
      addEasyButton(
        easyButton(
          icon = "fa-rotate-right",    # reset icon? Can also do fa-home?
          title = "Reset zoom",
          onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }"),
          position = "topleft"
        )
      ) %>%
      setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
      htmlwidgets::onRender("function(el, x) {
      this.zoomControl.setPosition('topleft');}") %>%
      #addAwesomeMarkers( # change to circles?
       # lng = filtered_data$OriginNestLon,
       # lat = filtered_data$OriginNestLat,
       # label = "Origin nestbox",
       # icon = awesomeIcons(icon = "leaf", markerColor = "darkgreen") # the icon is the symbol/shape in the middle, the marker is the pin
        # default icons are https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
        # can change to "fa" (fontawesome) or "ion" (ionicons)
        
      #)
      addCircleMarkers(
        lng = filtered_data$NestLon,
        lat = filtered_data$NestLat,
        label = "Last location",
        #labelOptions = labelOptions(noHide = TRUE), # Makes labels static but they're in an odd place? Also green probably not the best
        #color = "darkgreen"
        color = "#1b0c41",
        fillOpacity = 0.6,
        opacity = 1,
        radius = 8,
        weight = 2
      )
    
    
    m
  })
}