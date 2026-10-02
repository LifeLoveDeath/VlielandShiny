
# ----------------------------------------------------------------------
# Mapping Individuals Module - functions
# ----------------------------------------------------------------------


# Filter bird data --------------------------------------------------------


get_bird_data <- function(location.data, ring, year_range = NULL) {
  data <- location.data %>% filter(RingNumber == ring)
  if (!is.null(year_range)) {
    data <- data %>% filter(Year >= year_range[1], Year <= year_range[2])
  }
  data %>% arrange(Year)
}



# Build bade map ----------------------------------------------------------

build_base_map <- function(lng = 5.013775, lat = 53.273449, zoom = 12.5) {
  leaflet(options = leafletOptions(zoomControl = TRUE, zoomSnap = 0.5)) %>%
    addTiles() %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%
    htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
    addEasyButton(
      easyButton(
        icon = fontawesome::fa("crosshairs"),
        title = "Reset zoom",
        onClick = JS(sprintf("function(btn, map){ map.setView([%s, %s], %s); }", lat, lng, zoom))
      )
    ) %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))
}



# Add markers -------------------------------------------------------------

add_event_markers <- function(map, data, event_type, color, radius = 7) {
  event_data <- data %>% filter(Event == event_type)
  if (nrow(event_data) == 0) return(map)
  
  map %>% addCircleMarkers(
    lng = event_data$NestLon,
    lat = event_data$NestLat,
    label = paste0(event_data$Month, " ", event_data$Year),
    color = color,
    fillOpacity = 0.9,
    weight = 2,
    radius = radius
  )
}




# Add timeline path -------------------------------------------------------

add_timeline <- function(map, data) {
  timeline_data <- data %>% filter(Event %in% c("birth", "nest"))
  if (nrow(timeline_data) <= 1) return(map)
  
  freq <- nrow(timeline_data) * 2
  map %>% addPolylines(
    lng = timeline_data$NestLon,
    lat = timeline_data$NestLat,
    color = "#0d0887", weight = 3, opacity = 0.7,
    layerId = "timeline"
  ) %>% addArrowhead(
    layerId = "timeline",
    lng = timeline_data$NestLon,
    lat = timeline_data$NestLat,
    color = "#0d0887", weight = 3, opacity = 0.7,
    options = arrowheadOptions(yawn = 70, size = "4%", frequency = freq)
  )
}





