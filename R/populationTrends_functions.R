
# ----------------------------------------------------------------------
# Population Trends Module - functions
# ----------------------------------------------------------------------



# Get lay date data -------------------------------------------------------

summarise_laydate <- function(BroodData, species_filter, lay_type = "mean_day") {
  BroodData %>%
    mutate(
      SpeciesName = str_to_title(SpeciesName),
      LayDate = as.Date(LayDate),
      DayOfYear = yday(LayDate)
    ) %>%
    filter(SpeciesName %in% species_filter) %>%
    group_by(SpeciesName, BroodYear) %>%
    summarise(
      mean_day = mean(DayOfYear, na.rm = TRUE),
      earliest_day = min(DayOfYear, na.rm = TRUE),
      latest_day = max(DayOfYear, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      y_date = as.Date(mean_day - 1, origin = "2000-01-01"),
      earliest_date = as.Date(earliest_day - 1, origin = "2000-01-01"),
      latest_date = as.Date(latest_day - 1, origin = "2000-01-01"),
      tooltip = paste0(
        SpeciesName, "<br>", BroodYear,
        "<br>Mean lay date: ", format(y_date, "%d %b"),
        "<br>Range: ", format(earliest_date, "%d %b"), " - ", format(latest_date, "%d %b")
      ),
      y_var = .data[[lay_type]]
    )
}



# Get clutch size data ----------------------------------------------------

summarise_clutch <- function(BroodData, species_filter) {
  BroodData %>%
    mutate(SpeciesName = str_to_title(SpeciesName)) %>%
    filter(SpeciesName %in% species_filter) %>%
    group_by(SpeciesName, BroodYear) %>%
    summarise(
      mean_clutch = mean(ClutchSize, na.rm = TRUE),
      min_clutch = min(ClutchSize, na.rm = TRUE),
      max_clutch = max(ClutchSize, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tooltip = paste0(
        SpeciesName, "<br>",
        BroodYear, "<br>",
        "Mean clutch size: ", round(mean_clutch, 2),
        "<br>Range: ", round(min_clutch, 2), " - ", round(max_clutch, 2)
      )
    )
}




# General plotting function -----------------------------------------------

plot_time_trend <- function(plot_data, y_var, show_points = TRUE) {
  y_min <- min(plot_data[[y_var]], na.rm = TRUE)
  y_max <- max(plot_data[[y_var]], na.rm = TRUE)
  x_min <- min(plot_data$BroodYear, na.rm = TRUE)
  x_max <- max(plot_data$BroodYear, na.rm = TRUE)
  
  p <- ggplot(plot_data, aes(x = BroodYear, y = .data[[y_var]], colour = SpeciesName, group = SpeciesName)) +
    geom_line(alpha = 0.5)
  
  if (show_points) {
    p <- p + geom_point(aes(text = tooltip), size = 2)
  }
  
  p <- p + geom_smooth(aes(group = SpeciesName), method = "lm", se = FALSE, linetype = "dashed") +
    scale_y_continuous(limits = c(y_min, y_max)) +
    scale_x_continuous(limits = c(x_min, x_max)) +
    scale_colour_viridis(discrete = TRUE, option = "D", end = 0.8) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}
