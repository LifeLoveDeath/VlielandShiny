
# Mapping individuals module

# Load packages - moved to app.r
# library(shiny)
# library(leaflet)
# library(bslib)
# library(viridis)
# library(dplyr)
# library(reactable)
# library(leaflet.extras2)
# library(leaftime)
# library(leaflet.extras)


# UI ----------------------------------------------------

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

# If I go back to zoom and then back to ind page map, there are no icons etc. Selected ring must not update/set to NULL? - Fixed?
# Year range should maybe be greyed out if only birth nests checked

genMapServer <- function(id, location.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # --- Check boxes and slider ---
    
    # If the timeline checkbox is checked, ensure 'nest' is selected
    observeEvent(input$timeline, {
      if (input$timeline && !("nest" %in% input$event_filter)) {
        updateCheckboxGroupInput(session, "event_filter",
                                 selected = c(input$event_filter, "nest"))
      }
    })
    
    # If 'nest' is unchecked, ensure timeline checkbox is FALSE
    observeEvent(input$event_filter, {
      if (!("nest" %in% input$event_filter) && input$timeline) {
        updateCheckboxInput(session, "timeline", value = FALSE)
      }
    })
    
    # Year slider UI
    output$year_slider <- renderUI({
      req(selected_ring())
      bird_data <- location.data %>% filter(RingNumber == selected_ring(), !is.na(Year))
      #validate(need(nrow(bird_data) > 0, "No year data")) # need another way to deal with missing data as this creates errors
      
      if (nrow(bird_data) == 0) {
        # No data: show a disabled slider
        sliderInput(ns("year_range"), "Year range:",
                    min = 0, max = 0, value = c(0, 0),
                    sep = "", step = 1,
                    width = "100%",
                    ticks = FALSE
        )
      } else {
        # Normal slider
        sliderInput(ns("year_range"), "Year range:",
                    min = min(bird_data$Year, na.rm = TRUE),
                    max = max(bird_data$Year, na.rm = TRUE),
                    value = c(min(bird_data$Year, na.rm = TRUE),
                              max(bird_data$Year, na.rm = TRUE)),
                    sep = "", step = 1,
                    width = "100%"
        )
      }
    })
    
    # --- Reset when select_ring changes ---
    
    observeEvent(selected_ring(), {
      req(selected_ring())
      bird_data <- location.data %>% filter(RingNumber == selected_ring())
      if (nrow(bird_data) == 0) return()
      
      # Reset year slider
      updateSliderInput(session, "year_range",
                        min = min(bird_data$Year, na.rm = TRUE), # what is they're all NA?
                        max = max(bird_data$Year, na.rm = TRUE),
                        value = c(min(bird_data$Year, na.rm = TRUE), max(bird_data$Year, na.rm = TRUE)))
      
      # Reset checkboxes
      updateCheckboxGroupInput(session, "event_filter", selected = c("nest", "birth"))
      updateCheckboxInput(session, "timeline", value = FALSE)
    }, ignoreInit = TRUE)
    
    
    # --- Get data ---
    
    # Reactive filtered bird data
    bird_data <- reactive({
      req(selected_ring())
      if (!is.null(input$year_range) && !all(is.na(input$year_range))) {
        location.data %>%
          filter(RingNumber == selected_ring(),
                 Year >= input$year_range[1],
                 Year <= input$year_range[2]
          ) %>%
          arrange(Year)} else {
            location.data %>%
              filter(RingNumber == selected_ring())
            
          }
    })
      
    
    
    # --- Render map ---
    
    # Map UI
    output$map_ind_ui <- renderUI({
      leafletOutput(ns("map_individual"), width = "100%", height = "600px")
    })
    
    
    # Render Leaflet map
    output$map_individual <- renderLeaflet({
      data <- bird_data() # added this to check for no data
      map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% # added map <-
        addTiles() %>%
        setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
        htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
        addEasyButton(
          easyButton(
            icon = "fa-rotate-right",
            title = "Reset zoom",
            onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }")
          )
        )
      
      # Add message if no location data
      if (nrow(data) == 0 || all(is.na(data$NestLon)) || all(is.na(data$NestLat))) {
        map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
          addTiles() %>%
          addControl(
            html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data for this bird</div>",
            position = "topleft"
          ) %>%
          addEasyButton(
            easyButton(
              icon = "fa-rotate-right",    # reset icon? Can also do fa-home?
              title = "Reset zoom",
              onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }"),
              position = "topleft"
            )) %>%
          setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
          htmlwidgets::onRender("function(el, x) {
      this.zoomControl.setPosition('topleft');}")
      } 
      map
    })
    
    

    
    
    # --- Update based on checkboxes/sliders ---
    
    # Observe and update map markers
    observe({
      req(bird_data())
      #validate(need(nrow(bird_data()) > 0, "No matching bird data")) # need another way to deal with missing data as this creates errors
      
      m <- leafletProxy("map_individual", session) %>%
        clearMarkers() %>%
        clearShapes()
      
      # Birth markers
      if ("birth" %in% input$event_filter) {
        birth_data <- bird_data() %>% filter(Event == "birth")
        if (nrow(birth_data) > 0) {
          m <- m %>% addCircleMarkers(
            lng = birth_data$NestLon, lat = birth_data$NestLat,
            label = paste0("Birth nest: ", birth_data$Month, " ", birth_data$Year),
            color = "#440154", fillOpacity = 0.6, opacity = 1, radius = 8, weight = 2
          )
        }
      }
      
      # Nest markers
      if ("nest" %in% input$event_filter) {
        nest_data <- bird_data() %>% filter(Event == "nest")
        if (nrow(nest_data) > 0) {
          m <- m %>% addCircleMarkers(
            lng = nest_data$NestLon, lat = nest_data$NestLat,
            label = paste0("Breeding nest: ", nest_data$Month, " ", nest_data$Year),
            color = "#3b528b", fillOpacity = 0.6, opacity = 1, radius = 8, weight = 2
          )
        }
      }
      
      # Timeline path
      if (input$timeline && nrow(bird_data()) > 1) {
        timeline_data <- bird_data() %>%
          filter(Event %in% input$event_filter)
        if (nrow(timeline_data) > 1) {
          m <- m %>% addPolylines(
            lng = timeline_data$NestLon,
            lat = timeline_data$NestLat,
            color = "#3b528b", weight = 3, opacity = 0.7
          )
        }
      }
    })
    
  })
}
