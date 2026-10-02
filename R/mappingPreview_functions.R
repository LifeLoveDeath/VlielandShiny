
# ==========================================================
# Preview Map Module - Functions
# ==========================================================


# --- Get last location for a RingNumber ---
get_last_location <- function(location_data, ring) {
  location_data %>%
    filter(RingNumber == ring) %>%
    arrange(desc(Year), desc(Month)) %>%
    slice(1)
}

# --- Build Leaflet map for a given location ---
build_preview_map <- function(loc_data, default_lat = 53.273449, default_lng = 5.013775, default_zoom = 12.5) {
  m <- leaflet(options = leafletOptions(zoomControl = TRUE, zoomSnap = 0.5)) %>%
    addTiles() %>%
    setView(lng = default_lng, lat = default_lat, zoom = default_zoom) %>%
    htmlwidgets::onRender("function(el, x) { this.zoomControl.setPosition('topleft'); }") %>%
    addEasyButton(
      easyButton(
        icon = fontawesome::fa("crosshairs"),
        title = "Reset zoom",
        onClick = JS(sprintf("function(btn, map){ map.setView([%s, %s], %s); }",
                             default_lat, default_lng, default_zoom))
      )
    ) %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))

  
  if (nrow(loc_data) == 0 || is.na(loc_data$NestLon) || is.na(loc_data$NestLat)) {
    m <- m %>%
      addControl(
        html = "<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>No location data</div>",
        position = "topright"
      )
  } else {
    m <- m %>%
      addControl(
        html = paste0("<div style='font-weight:bold; font-size:16px; background:white; padding:4px; border-radius:4px;'>",
                      loc_data$RingNumber, " Last recorded location</div>"),
        position = "topright"
      ) %>%
      addCircleMarkers(
        lng = loc_data$NestLon,
        lat = loc_data$NestLat,
        label = paste0(loc_data$Month, " ", loc_data$Year, ": ", loc_data$Event),
        color = "#1b0c41",
        fillOpacity = 0.6,
        opacity = 1,
        radius = 8,
        weight = 2
      )
  }
  
  m
}