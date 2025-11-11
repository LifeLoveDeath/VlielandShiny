
# Mapping individuals module

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
        helpText(HTML("<b>See where the selected bird was born and has nested
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
# 
# # Year range should maybe be greyed out if only birth nests checked
# 
# genMapServer <- function(id, location.data, selected_ring) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     
#     # --- Check boxes and slider ---
#     
#     # If the timeline checkbox is checked, ensure 'nest' is selected
#     observeEvent(input$timeline, {
#       if (input$timeline && !("nest" %in% input$event_filter)) {
#         updateCheckboxGroupInput(session, "event_filter",
#                                  selected = c(input$event_filter, "nest"))
#       }
#     })
#     
#     # If 'nest' is unchecked, ensure timeline checkbox is FALSE
#     observeEvent(input$event_filter, {
#       if (!("nest" %in% input$event_filter) && input$timeline) {
#         updateCheckboxInput(session, "timeline", value = FALSE)
#       }
#     })
#     
#     # Year slider UI
#     output$year_slider <- renderUI({
#       req(selected_ring())
#       bird_data <- location.data %>% filter(RingNumber == selected_ring(), !is.na(Year))
#       #validate(need(nrow(bird_data) > 0, "No year data")) # need another way to deal with missing data as this creates errors
#       
#       if (nrow(bird_data) == 0) {
#         # No data: show a disabled slider
#         sliderInput(ns("year_range"), "Year range:",
#                     min = 0, max = 0, value = c(0, 0),
#                     sep = "", step = 1,
#                     width = "100%",
#                     ticks = FALSE
#         )
#       } else {
#         # Normal slider
#         sliderInput(ns("year_range"), "Year range:",
#                     min = min(bird_data$Year, na.rm = TRUE),
#                     max = max(bird_data$Year, na.rm = TRUE),
#                     value = c(min(bird_data$Year, na.rm = TRUE),
#                               max(bird_data$Year, na.rm = TRUE)),
#                     sep = "", step = 1,
#                     width = "100%"
#         )
#       }
#     })
#     
#     # --- Reset when select_ring changes ---
#     
#     observeEvent(selected_ring(), {
#       req(selected_ring())
#       bird_data <- location.data %>% filter(RingNumber == selected_ring())
#       if (nrow(bird_data) == 0) return()
#       
#       # Reset year slider
#       updateSliderInput(session, "year_range",
#                         min = min(bird_data$Year, na.rm = TRUE), # what is they're all NA?
#                         max = max(bird_data$Year, na.rm = TRUE),
#                         value = c(min(bird_data$Year, na.rm = TRUE), max(bird_data$Year, na.rm = TRUE)))
#       
#       # Reset checkboxes
#       updateCheckboxGroupInput(session, "event_filter", selected = c("nest", "birth"))
#       updateCheckboxInput(session, "timeline", value = FALSE)
#     }, ignoreInit = TRUE)
#     
#     
#     # --- Get data ---
#     
#     # Reactive filtered bird data
#     bird_data <- reactive({
#       req(selected_ring())
#       if (!is.null(input$year_range) && !all(is.na(input$year_range))) {
#         location.data %>%
#           filter(RingNumber == selected_ring(),
#                  Year >= input$year_range[1],
#                  Year <= input$year_range[2]
#           ) %>%
#           arrange(Year)} else {
#             location.data %>%
#               filter(RingNumber == selected_ring())
#             
#           }
#     })
#       
#     
#     
#     # --- Render map ---
#     
#     # Map UI
#     output$map_ind_ui <- renderUI({
#       leafletOutput(ns("map_individual"), width = "100%", height = "600px")
#     })
#     
#     
#     # Render Leaflet map
#     # Map
#     output$map_individual <- renderLeaflet({
#       data <- bird_data() # added this to check for no data
#       map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>% # added map <-
#         addTiles() %>%
#         setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
#         htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
#         addEasyButton(
#           easyButton(
#             icon=icon("clock-o"),
#             #icon = "fa-refresh",     # FA4 icon class
#             title = "Reset zoom",
#             onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }")
#           )
#         )
#       
#       # Add message if no location data
#       if (nrow(data) == 0 || all(is.na(data$NestLon)) || all(is.na(data$NestLat))) {
#         map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
#           addTiles() %>%
#           addControl(
#             html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data for this bird</div>",
#             position = "topleft"
#           ) %>%
#           addEasyButton(
#             easyButton(
#               icon = htmltools::tags$img(src = "crosshairs_icon.png", width = "24px", height = "24px"),
#               #icon=icon("clock-o"),
#               #icon = "fa-globe",     # FA4 icon class
#               title = "Reset zoom",
#               onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }")
#             )) %>%
#           setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
#           htmlwidgets::onRender("function(el, x) {
#       this.zoomControl.setPosition('topleft');}")
#       } 
#       map
#     
#     })
#     
#     
# 
#     
#     
#     # --- Update based on checkboxes/sliders ---
#     
#     # Observe and update map markers
#     observe({
#       req(bird_data())
#       #validate(need(nrow(bird_data()) > 0, "No matching bird data")) # need another way to deal with missing data as this creates errors
#       
#       m <- leafletProxy("map_individual", session) %>%
#         clearMarkers() %>%
#         clearShapes()
#       
#       # Birth markers
#       if ("birth" %in% input$event_filter) {
#         birth_data <- bird_data() %>% filter(Event == "birth")
#         if (nrow(birth_data) > 0) {
#           m <- m %>% addCircleMarkers(
#             lng = birth_data$NestLon, lat = birth_data$NestLat,
#             label = paste0("Birth nest: ", birth_data$Month, " ", birth_data$Year),
#             color = "#e97158", fillOpacity = 0.9, opacity = 1, radius = 8, weight = 2
#           )
#         }
#       }
#       
#       # Nest markers
#       if ("nest" %in% input$event_filter) {
#         nest_data <- bird_data() %>% filter(Event == "nest")
#         if (nrow(nest_data) > 0) {
#           m <- m %>% addCircleMarkers(
#             lng = nest_data$NestLon, lat = nest_data$NestLat,
#             label = paste0("Breeding nest: ", nest_data$Month, " ", nest_data$Year),
#             color = "#0d0887", fillOpacity = 0.6, opacity = 1, radius = 8, weight = 2
#           )
#         }
#       }
#       
#       # Timeline path
#       if (input$timeline && nrow(bird_data()) > 1) {
#         freq <- nrow(bird_data()) * 2
#         timeline_data <- bird_data() %>%
#           filter(Event %in% input$event_filter)
#         
#         if (nrow(timeline_data) > 1) {
#           m <- m %>% addPolylines(
#             lng = timeline_data$NestLon,
#             lat = timeline_data$NestLat,
#             color = "#0d0887", weight = 3, opacity = 0.7,
#             layerId = "timeline"
#           ) %>%
#         #   m <- m %>%
#             addArrowhead(
#             layerId = "timeline",
#             lng = timeline_data$NestLon,
#             lat = timeline_data$NestLat,
#             color = "#0d0887", weight = 3, opacity = 0.7,
#             options = arrowheadOptions(yawn = 50, size = "7%",
#                                        frequency = freq,
#                                        #frequency = "allvertices"
#                                        # offsets = list(     # to offset arrow from end or line
#                                        #   start = "100m",   # or "100m" or "50px"
#                                        #   end   = "15px"    # or "15px"
#                                        # )
#                                        )
#           )
#           }
#       }
#     })
#     
#   })
# }


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
      
      if (nrow(bird_data) == 0) {
        sliderInput(ns("year_range"), "Year range:",
                    min = 0, max = 0, value = c(0, 0),
                    sep = "", step = 1,
                    width = "100%", ticks = FALSE)
      } else {
        sliderInput(ns("year_range"), "Year range:",
                    min = min(bird_data$Year, na.rm = TRUE),
                    max = max(bird_data$Year, na.rm = TRUE),
                    value = c(min(bird_data$Year, na.rm = TRUE),
                              max(bird_data$Year, na.rm = TRUE)),
                    sep = "", step = 1,
                    width = "100%")
      }
    })
    
    # --- Reset when select_ring changes ---
    observeEvent(selected_ring(), {
      req(selected_ring())
      bird_data <- location.data %>% filter(RingNumber == selected_ring())
      if (nrow(bird_data) == 0) return()
      
      # Reset year slider
      updateSliderInput(session, "year_range",
                        min = min(bird_data$Year, na.rm = TRUE),
                        max = max(bird_data$Year, na.rm = TRUE),
                        value = c(min(bird_data$Year, na.rm = TRUE),
                                  max(bird_data$Year, na.rm = TRUE)))
      
      # Reset checkboxes
      updateCheckboxGroupInput(session, "event_filter", selected = c("nest", "birth"))
      updateCheckboxInput(session, "timeline", value = FALSE)
    }, ignoreInit = TRUE)
    
    # --- Reactive filtered bird data ---
    bird_data <- reactive({
      req(selected_ring())
      if (!is.null(input$year_range) && !all(is.na(input$year_range))) {
        location.data %>%
          filter(RingNumber == selected_ring(),
                 Year >= input$year_range[1],
                 Year <= input$year_range[2]) %>%
          arrange(Year)
      } else {
        location.data %>% filter(RingNumber == selected_ring())
      }
    })
    
    # --- Render map UI ---
    output$map_ind_ui <- renderUI({
      leafletOutput(ns("map_individual"), width = "100%", height = "600px")
    })
    
    # --- Render base Leaflet map (once) ---
    output$map_individual <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
        setView(lng = 5.018424, lat = 53.286226, zoom = 12) %>%
        addTiles() %>%
        htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
        addEasyButton(
          easyButton(
            icon = fontawesome::fa("crosshairs"),
            title = "Reset zoom",
            onClick = JS("function(btn, map){ map.setView([53.286226, 5.018424], 12); }")
          )#,
          #position = "topright"
        )
    })
    
    # --- Update map markers / polylines dynamically ---
    observe({
      req(bird_data())
      m <- leafletProxy("map_individual", session) %>%
        clearMarkers() %>%
        clearShapes()
      
      data <- bird_data()
      
      # Show message if no data
      if (nrow(data) == 0 || all(is.na(data$NestLon)) || all(is.na(data$NestLat))) {
        m %>% addControl(
          html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data for this bird</div>",
          position = "topleft"
        )
      } else {
        # Birth markers
        if ("birth" %in% input$event_filter) {
          birth_data <- data %>% filter(Event == "birth")
          if (nrow(birth_data) > 0) {
            m <- m %>% addCircleMarkers(
              lng = birth_data$NestLon, lat = birth_data$NestLat,
              label = paste0(
                "Birth nest: ",
                ifelse(!is.na(birth_data$Month), paste0(birth_data$Month, " "), ""),
                birth_data$Year,
                ifelse(!is.na(birth_data$ClutchSize),
                       paste0(" (Birth clutch size: ", birth_data$ClutchSize, ")"),
                       "")
                ),
              color = "#e97158", fillOpacity = 0.9, opacity = 1, radius = 8, weight = 2
            )
          }
        }
        
        # Nest markers
        if ("nest" %in% input$event_filter) {
          nest_data <- data %>% filter(Event == "nest")
          if (nrow(nest_data) > 0) {
            m <- m %>% addCircleMarkers(
              lng = nest_data$NestLon, lat = nest_data$NestLat,
              label = paste0(
                "Breeding nest: ",
                ifelse(!is.na(nest_data$Month), paste0(nest_data$Month, " "), ""),
                nest_data$Year,
                ifelse(!is.na(nest_data$ClutchSize),
                       paste0(" (Clutch size: ", nest_data$ClutchSize, ")"),
                       "")
              ),
              color = "#0d0887", fillOpacity = 0.6, opacity = 1, radius = 8, weight = 2
            )
          }
        }
        
        # Timeline path
        if (input$timeline && nrow(data) > 1) {
          timeline_data <- data %>% filter(Event %in% input$event_filter)
          if (nrow(timeline_data) > 1) {
            freq <- nrow(timeline_data) * 2
            m <- m %>% addPolylines(
              lng = timeline_data$NestLon,
              lat = timeline_data$NestLat,
              color = "#0d0887", weight = 3, opacity = 0.7,
              layerId = "timeline"
            ) %>%
              addArrowhead(
                layerId = "timeline",
                lng = timeline_data$NestLon,
                lat = timeline_data$NestLat,
                color = "#0d0887", weight = 3, opacity = 0.7,
                options = arrowheadOptions(yawn = 50, size = "7%", frequency = freq)
              )
          }
        }
      }
    })
  })
}

