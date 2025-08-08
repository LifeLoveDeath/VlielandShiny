
# Mapping individuals module



library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)

# UI ----------------------------------------------------

#Side panel with check boxes etc.


# Server ------------------------------------------------


genMapServer <- function(input, output, location.data, selected_ring, session) { # data intput is search_results from the individual search server function. Is it right to do it like this or should it still be data?
  
  # Show map if a row is selected - maybe don't need this one here because the individual has been selected to open this page
  output$map_ind_ui <- renderUI({
    req(selected_ring())  # Only render if a row is selected
    leafletOutput("map_individual", width = "95%", height = "600px")
  })
  
  # baseline map to test
  output$map_individual <- renderLeaflet({
    # Check there is data
    req(selected_ring()) # i.e. selected_ring not NULL                
    #req(input$summary_info_rows_selected) # a row has been selected
    
    # Filer data based on row selection
    #filtered_data <- selected_ring()[input$summary_info_rows_selected, ]
    bird_data <- location.data %>%
      filter(Event == "birth", RingNumber == selected_ring())
    # Should actually filter a second nestboxes dataframe based on bird ring number here
    
    # Check location info exists:
    validate(
      need(nrow(bird_data) > 0, "No matching bird data"),
      need(!is.na(bird_data$NestLon), "No location data"),
      need(!is.na(bird_data$NestLat), "No location data")
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
        lng = bird_data$NestLon,
        lat = bird_data$NestLat,
        label = as.character(paste0("Birth nest: ", bird_data$NestNo)),
        icon = awesomeIcons(icon = "home", markerColor = "darkgreen") # the icon is the symbol/shape in the middle, the marker is the pin
        # default icons are https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
        # can change to "fa" (fontawesome) or "ion" (ionicons)
        
      )
    
    
    m
  })
}