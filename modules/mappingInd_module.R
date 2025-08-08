
# Mapping individuals module



library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)

# UI ----------------------------------------------------

# Add time slider
# Could add an animation of them appearing over time

#Side panel with check boxes etc.
mapUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      wellPanel(
        #checkboxInput(ns("show_birth"), "Birth nest", value = TRUE),
        #checkboxInput(ns("show_breeding"), "Breeding nests", value = TRUE) # shuold actually be one line with choices
        checkboxGroupInput(ns("event_filter"), "Show locations for:", 
                           choices = c("Birth nest" = "birth", "Breeding nests" = "nest"),
                           selected = c("birth", "nest")),
        sliderInput("year_range", "Year range:",
                    min = min(location.data$Year, na.rm = TRUE),
                    max = max(location.data$Year, na.rm = TRUE),
                    value = c(1980, 2025), sep = "")
      )
    ),
    column(
      width = 9,
      uiOutput(ns("map_ind_ui"))  # this is your leafletOutput wrapped in renderUI
    )
  )
}


# Server ------------------------------------------------

genMapServer <- function(id, location.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    # Render UI placeholder for the map
    output$map_ind_ui <- renderUI({
      req(selected_ring())
      leafletOutput(session$ns("map_individual"), width = "100%", height = "600px")
    })
    
    # Render Leaflet map
    output$map_individual <- renderLeaflet({
      req(selected_ring())
      
      # Get data for selected individual
      bird_data <- location.data %>%
        filter(RingNumber == selected_ring())
      
      # Check these is data
      validate(
        need(nrow(bird_data) > 0, "No matching bird data")
      )
      
      # Map:
      m <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
        htmlwidgets::onRender("
          function(el, x) {
            this.zoomControl.setPosition('topright');
          }
        ")
      
      # Add birth nest marker if selected
      if ("birth" %in% input$event_filter) {
        birth_data <- bird_data %>% filter(Event == "birth")
        if (nrow(birth_data) > 0) {
          m <- m %>%
            addAwesomeMarkers(
              lng = birth_data$NestLon,
              lat = birth_data$NestLat,
              label = paste0("Birth nest: ", birth_data$Year),
              icon = awesomeIcons(icon = "leaf", markerColor = "darkgreen")
            )
        }
      }
      
      # Add breeding/reproduction nest markers if selected
      if ("nest" %in% input$event_filter) {
        breeding_data <- bird_data %>% filter(Event == "nest")
        if (nrow(breeding_data) > 0) {
          m <- m %>%
            addAwesomeMarkers(
              lng = breeding_data$NestLon,
              lat = breeding_data$NestLat,
              label = paste0("Breeding nest: ", breeding_data$Year),
              icon = awesomeIcons(icon = "leaf", markerColor = "darkblue"),
              clusterOptions = markerClusterOptions()
            )
        }
      }
      
      m
    })
  })
}