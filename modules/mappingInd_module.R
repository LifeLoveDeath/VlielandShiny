
# Mapping individuals module



library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)
library(leaflet.extras2)
library(leaftime)

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
        checkboxGroupInput(ns("event_filter"), "Show locations for:", 
                           choices = c("Birth nest" = "birth", "Breeding nests" = "nest"),
                           selected = c("birth", "nest")),
        checkboxInput(ns("timeline"), "Show timeline path", value = FALSE, width = NULL),
        
        uiOutput(ns("year_slider")
      )
    )),
    column(
      width = 9,
      uiOutput(ns("map_ind_ui"))  # leafletOutput from server wrapped in renderUI
    )
  )
}


# Server ------------------------------------------------

# Whenever you interact with the map the zoom resets

genMapServer <- function(id, location.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Year slider - year range of selected individual (maybe this should include month)
    output$year_slider <- renderUI({
      req(selected_ring())
      bird_data <- location.data %>%
        filter(RingNumber == selected_ring(), !is.na(Year))
      validate(need(nrow(bird_data) > 0, "No year data"))
      
      sliderInput(ns("year_range"), "Year range:",
                  min = min(bird_data$Year, na.rm = TRUE),
                  max = max(bird_data$Year, na.rm = TRUE),
                  value = c(min(bird_data$Year, na.rm = TRUE), max(bird_data$Year, na.rm = TRUE)),
                  sep = "", step = 1)
    })
    
    
    # Render UI placeholder for the map
    output$map_ind_ui <- renderUI({
      req(selected_ring())
      leafletOutput(session$ns("map_individual"), width = "100%", height = "600px")
    })
    
    # Render Leaflet map
    output$map_individual <- renderLeaflet({
      req(selected_ring(), input$year_range)
      
      # Get data for selected individual
      bird_data <- location.data %>%
        filter(RingNumber == selected_ring(),
               Year >= input$year_range[1],
               Year <= input$year_range[2])
      
      # Check there is data
      validate(
        need(nrow(bird_data) > 0, "No matching bird data")
      )
      
      # Sort by year
      bird_data <- bird_data %>% arrange(Year)
      
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
            #addAwesomeMarkers(
            #  lng = birth_data$NestLon,
            #  lat = birth_data$NestLat,
            #  label = paste0("Birth nest: ", birth_data$Year),
            #  icon = awesomeIcons(icon = "leaf", markerColor = "darkgreen")
            #)
          addCircleMarkers(
            lng = birth_data$NestLon,
            lat = birth_data$NestLat,
            label = paste0("Birth nest: ", birth_data$Month, ", ", birth_data$Year),
            color = "darkgreen"
            )
        }
      }
      
      # Add breeding/reproduction nest markers if selected. Should maybe all be one code block (birth and nesting)
      if ("nest" %in% input$event_filter) {
        breeding_data <- bird_data %>% filter(Event == "nest")
        if (nrow(breeding_data) > 0) {
          m <- m %>%
            #addAwesomeMarkers(
            #  lng = breeding_data$NestLon,
            #  lat = breeding_data$NestLat,
            #  label = paste0("Breeding nest: ", breeding_data$Year),
            #  icon = awesomeIcons(icon = "leaf", markerColor = "darkblue")#,
              #clusterOptions = markerClusterOptions() # removing clustering might make timeline clearer?
            #) 
            addCircleMarkers(
              lng = breeding_data$NestLon,
              lat = breeding_data$NestLat,
              label = paste0("Breeding nest: ", breeding_data$Month, ", ", breeding_data$Year),
              color = "darkblue"
            )
            #%>%
            
            #addTimeline(
              #data = bird_data) %>% # not working but look into this from leaftime package
            #addPolylines(
             # lat = ~NestLat,
            #  lng = ~NestLon,
            #  group = ~RingNumber,
            #  color = "blue",
            #  weight = 2,
            #  opacity = 0.7,
            #  popup = ~paste("Year:", Year, "<br>", "NestNo:", NestNo)
            #) #old path line code
        }
      }
      
      if (nrow(bird_data) > 1 & input$timeline == TRUE) {
        if ("birth" %in% input$event_filter & "nest" %in% input$event_filter) {
        m <- m %>%
          addPolylines(
            lng = bird_data$NestLon,
            lat = bird_data$NestLat,
            color = "darkblue",
            weight = 3,
            opacity = 0.7,
            label = paste0("Nest timeline")
          )
        }
        if ("nest" %in% input$event_filter) {
          breeding_data <- bird_data %>% filter(Event == "nest")
          m <- m %>%
            addPolylines(
              lng = breeding_data$NestLon,
              lat = breeding_data$NestLat,
              color = "darkblue",
              weight = 3,
              opacity = 0.7,
              label = paste0("Nest timeline")
            )
        }
        
        }
      
      m
    })
  })
}