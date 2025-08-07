
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

genPreviewMapServer <- function(input, output, search_results, session) { # data intput is search_results from the individual search server function. Is it right to do it like this or should it still be data?
  
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
    
    # Filer data based on row selection
    filtered_data <- search_results()[input$summary_info_rows_selected, ]
    # Should actually filter a second nestboxes dataframe based on bird ring number here
    
    # Check location info exists:
    validate(
      need(!is.null(filtered_data$OriginNestLon), "No location data"),
      need(!is.null(filtered_data$OriginNestLat), "No location data")
    )
    
    # specify markers style
    #originNestIcons <- awesomeIcons(
    #  iconColor = 'black',
    #  markerColor = getColor(df.20)
    #)
    
    # Create map with marker
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% 
      addTiles() %>% 
      setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
      htmlwidgets::onRender("
    function(el, x) {
      this.zoomControl.setPosition('topright');
    }
  ") %>%
      addAwesomeMarkers(
        lng = filtered_data$OriginNestLon,
        lat = filtered_data$OriginNestLat,
        label = "Origin nestbox",
        icon = awesomeIcons(icon = "home", markerColor = "darkgreen") # the icon is the symbol/shape in the middle, the marker is the pin
        # default icons are https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
        # can change to "fa" (fontawesome) or "ion" (ionicons)
        
      )
    
    
    m
  })
}