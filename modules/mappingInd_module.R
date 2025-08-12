
# Mapping individuals module

library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)
library(leaflet.extras2)
library(leaftime)
library(leaflet.extras)


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
        tags$label("Timeline:", style = "margin-bottom: 0; display: block;"),
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

# If I got back to zoom and then back to ind page map, there are no icons etc. Selected ring must not update/set to NULL?
# Removing the year slider etcc = markers don't appear at first but do when user tick/unticks the checkboxes. Return to search issue still the same

genMapServer <- function(id, location.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render UI placeholder for the map
    output$map_ind_ui <- renderUI({
      req(selected_ring())
      leafletOutput(session$ns("map_individual"), width = "100%", height = "600px")
    })
    
    
    
    # Render Leaflet map
    output$map_individual <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
        htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl.setPosition('topleft');
      }
    ") %>%
        addEasyButton(
          easyButton(
            icon = "fa-rotate-right",    # reset icon? Can also do fa-home?
            title = "Reset zoom",
            onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }"),
          )
        )
    })
    
    observe({ # this means the map updates but isn't re-rendered when inputs change, so zoom stays the same and doesn't reset
      req(selected_ring())
      
      bird_data <- location.data %>%
        filter(RingNumber == selected_ring()) %>%
        arrange(Year)
      
      validate(need(nrow(bird_data) > 0, "No matching bird data"))
      
      m <- leafletProxy("map_individual", session) %>%
        clearMarkers() %>%
        clearShapes()
      
      # Birth nest markers
      if ("birth" %in% input$event_filter) {
        birth_data <- bird_data %>% filter(Event == "birth")
        if (nrow(birth_data) > 0) {
          m <- m %>%
            addCircleMarkers(
              lng = birth_data$NestLon,
              lat = birth_data$NestLat,
              label = paste0("Birth nest: ", birth_data$Month, " ", birth_data$Year),
              color = "darkgreen"
            )
        }
      }
      
      # Breeding nest markers
      if ("nest" %in% input$event_filter) {
        breeding_data <- bird_data %>% filter(Event == "nest")
        if (nrow(breeding_data) > 0) {
          m <- m %>%
            addCircleMarkers(
              lng = breeding_data$NestLon,
              lat = breeding_data$NestLat,
              label = paste0("Breeding nest: ", breeding_data$Month, " ", breeding_data$Year),
              color = "darkblue"
            )
        }
      }
      
      # Path / timeline
      if (nrow(bird_data) > 1 && input$timeline == TRUE) {
        if ("birth" %in% input$event_filter & "nest" %in% input$event_filter) {
        m <- m %>%
          addPolylines(
            lng = bird_data$NestLon,
            lat = bird_data$NestLat,
            color = "darkblue",
            weight = 3,
            opacity = 0.7,
            label = "Timeline path"
          ) }
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
      m # not needed for rendering the map but adding to try and fix return to search issue
  })
})
}


genMapServerOld <- function(id, location.data, selected_ring) {
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
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
        htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl.setPosition('topleft');
      }
    ") %>%
        addEasyButton(
          easyButton(
            icon = "fa-rotate-right",    # reset icon? Can also do fa-home?
            title = "Reset zoom",
            onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }"),
          )
        )
    })
    
    observe({ # this means the map updates but isn't re-rendered when inputs change, so zoom stays the same and doesn't reset
      req(selected_ring(), input$year_range)
      
      bird_data <- location.data %>%
        filter(RingNumber == selected_ring(),
               Year >= input$year_range[1],
               Year <= input$year_range[2]) %>%
        arrange(Year)
      
      validate(need(nrow(bird_data) > 0, "No matching bird data"))
      
      m <- leafletProxy("map_individual", session) %>%
        clearMarkers() %>%
        clearShapes()
      
      # Birth nest markers
      if ("birth" %in% input$event_filter) {
        birth_data <- bird_data %>% filter(Event == "birth")
        if (nrow(birth_data) > 0) {
          m <- m %>%
            addCircleMarkers(
              lng = birth_data$NestLon,
              lat = birth_data$NestLat,
              label = paste0("Birth nest: ", birth_data$Month, " ", birth_data$Year),
              color = "darkgreen"
            )
        }
      }
      
      # Breeding nest markers
      if ("nest" %in% input$event_filter) {
        breeding_data <- bird_data %>% filter(Event == "nest")
        if (nrow(breeding_data) > 0) {
          m <- m %>%
            addCircleMarkers(
              lng = breeding_data$NestLon,
              lat = breeding_data$NestLat,
              label = paste0("Breeding nest: ", breeding_data$Month, " ", breeding_data$Year),
              color = "darkblue"
            )
        }
      }
      
      # Path / timeline
      if (nrow(bird_data) > 1 && input$timeline == TRUE) {
        if ("birth" %in% input$event_filter & "nest" %in% input$event_filter) {
          m <- m %>%
            addPolylines(
              lng = bird_data$NestLon,
              lat = bird_data$NestLat,
              color = "darkblue",
              weight = 3,
              opacity = 0.7,
              label = "Timeline path"
            ) }
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
      m # not needed for rendering the map but adding to try and fix return to search issue
    })
  })
}