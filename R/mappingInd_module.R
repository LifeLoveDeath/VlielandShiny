
# ----------------------------------------------------------------------
# Mapping Individuals Module
# ----------------------------------------------------------------------
#   Provides a map showing the locations of a selected bird, 
#   including its birth nest and breeding nests over multiple years.
#   Shows timeline of movements and distance travelled.
#
# Inputs:
#   - location.data: Dataframe with bird locations (RingNumber, Event, Year, Month, NestLon, NestLat, etc.)
#   - selected_ring: Reactive value of currently selected bird RingNumber


# Load packages
# library(shiny)
# library(leaflet)
# library(bslib)
# library(viridis)
# library(dplyr)
# library(reactable)
# library(leaflet.extras2)
# library(leaftime)
# library(leaflet.extras)
# library(sf)
library("fontawesome")


# UI ----------------------------------------------------

#Side panel with check boxes etc.
mapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
  fluidRow(
    column(
      width = 12,
      br(),
      h4("Location data", style = "color:#3f5262; font-weight:500;")
    )
  ),
  
  fluidRow(
    column(
      width = 4,
        
        # Instructions text:
        helpText(HTML("<b>See where the selected bird was born and has bred
        during different breeding seasons.</b><br>
        • Use the checkboxes to choose the type of location to display.<br>
        • The timeline path connects nests in chronological order.<br>
        • Adjust the slider to filter locations by year.")),
      
      br(),
        
        checkboxGroupInput(ns("event_filter"), "Show locations for:", 
                           choices = c("Birth nest" = "birth", "Breeding nests" = "nest"),
                           selected = c("birth", "nest")),
        tags$label("Timeline:", style = "margin-bottom: 0; display: block;"),
        checkboxInput(ns("timeline"), "Show timeline path", value = FALSE, width = NULL),
        
        uiOutput(ns("year_slider")),
      
      # Placeholder further info text:
      br(),
      helpText(HTML("Placeholder further info text.<br>
                    E.g. General info about dispersal.<br>
                    Or interpretation of map: Clusters of points suggest repeated nesting in the same area.<br>"))

    ),
    column(
      width = 8,
      uiOutput(ns("map_ind_ui"))  # leafletOutput from server wrapped in renderUI
      
    )
  # ),
  # tags$head(
  #   tags$link(
  #     rel = "stylesheet",
  #     href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
  #   )
  )
  )
}


# # Server ------------------------------------------------


genMapServer <- function(id, location.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------
    # Helper: reset event_filter if timeline changes
    # -------------------------
    observeEvent(input$timeline, {
      if (input$timeline && !("nest" %in% input$event_filter)) {
        updateCheckboxGroupInput(session, "event_filter",
                                 selected = c(input$event_filter, "nest"))
      }
    })
    
    observeEvent(input$event_filter, {
      if (!("nest" %in% input$event_filter) && input$timeline) {
        updateCheckboxInput(session, "timeline", value = FALSE)
      }
    })
    
    # -------------------------
    # Year slider UI
    # -------------------------
    output$year_slider <- renderUI({
      req(selected_ring())
      bird_data <- get_bird_data(location.data, selected_ring())
      
      if (nrow(bird_data) == 0) {
        sliderInput(ns("year_range"), "Year range:", min = 0, max = 0, value = c(0,0),
                    sep = "", step = 1, width = "100%", ticks = FALSE)
      } else {
        sliderInput(ns("year_range"), "Year range:",
                    min = min(bird_data$Year, na.rm = TRUE),
                    max = max(bird_data$Year, na.rm = TRUE),
                    value = c(min(bird_data$Year, na.rm = TRUE),
                              max(bird_data$Year, na.rm = TRUE)),
                    sep = "", step = 1, width = "100%")
      }
    })
    
    # -------------------------
    # Reset inputs when selected_ring changes
    # -------------------------
    observeEvent(selected_ring(), {
      req(selected_ring())
      bird_data <- get_bird_data(location.data, selected_ring())
      if (nrow(bird_data) == 0) return()
      
      updateSliderInput(session, "year_range",
                        min = min(bird_data$Year, na.rm = TRUE),
                        max = max(bird_data$Year, na.rm = TRUE),
                        value = c(min(bird_data$Year, na.rm = TRUE),
                                  max(bird_data$Year, na.rm = TRUE)))
      
      updateCheckboxGroupInput(session, "event_filter", selected = c("nest", "birth"))
      updateCheckboxInput(session, "timeline", value = FALSE)
    }, ignoreInit = TRUE)
    
    # -------------------------
    # Reactive filtered data
    # -------------------------
    filtered_data <- reactive({
      req(selected_ring())
      get_bird_data(location.data, selected_ring(), input$year_range)
    })
    
    # -------------------------
    # Render map UI
    # -------------------------
    output$map_ind_ui <- renderUI({
      leafletOutput(ns("map_individual"), width = "100%", height = "600px")
    })
    
    # -------------------------
    # Render base map (once)
    # -------------------------
    output$map_individual <- renderLeaflet({
      build_base_map()
    })
    
    # -------------------------
    # Update map markers dynamically
    # -------------------------
    observe({
      data <- filtered_data()
      req(data)
      
      m <- leafletProxy("map_individual", session) %>%
        clearMarkers() %>%
        clearShapes()
      
      # Only add markers / timeline if data exists
      if ("birth" %in% input$event_filter) {
        m <- add_event_markers(m, data, "birth", "#e97158", radius = 9)
      }
      if ("nest" %in% input$event_filter) {
        m <- add_event_markers(m, data, "nest", "#0d088775")
      }
      if (input$timeline && nrow(data) > 1) {
        m <- add_timeline(m, data %>% filter(Event %in% input$event_filter))
      }
    })
    
  })
}