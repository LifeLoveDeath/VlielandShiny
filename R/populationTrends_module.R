
# populationTrends module

library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)
library(stringr)
library(lubridate)

# helper functions --------------------------------------------------------------


# BroodData %>%
#   group_by(BroodPopulationName, BroodYear) %>%
#   summarise(n_broods = n()) %>%
#   ggplot(aes(x = BroodYear, y = n_broods, colour = BroodPopulationName)) +
#   geom_line() +
#   labs(title = "Number of Broods per Year", y = "Number of broods", x = "Year")
# 
# 
# 
# BroodData %>%
#   group_by(BroodPopulationName, BroodYear) %>%
#   summarise(mean_clutch = mean(ClutchSize, na.rm = TRUE)) %>%
#   ggplot(aes(x = BroodYear, y = mean_clutch, colour = BroodPopulationName)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
#   labs(title = "Average Clutch Size over Time", y = "Mean clutch size", x = "Year")
# 
# 
# BroodData %>%
#   mutate(
#     LayDate = as.Date(LayDate),
#     LayDay_April = as.numeric(LayDate - as.Date(paste0(BroodYear, "-04-01")))
#   ) %>%
#   group_by(BroodPopulationName, BroodYear) %>%
#   summarise(mean_layday = mean(LayDay_April, na.rm = TRUE), .groups = "drop") %>%
#   ggplot(aes(x = BroodYear, y = mean_layday, colour = BroodPopulationName)) +
#   geom_line() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
#   labs(
#     title = "Mean Lay Date Over Time",
#     x = "Year",
#     y = "Mean lay date (April day)",
#     colour = "Population"
#   ) +
#   theme_minimal()
# 
# 





# UI function --------------------------------------------------------------


populationTrendsUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = "background-color: #f2f4f5; padding: 5px;",
    
    # --- White panel container ---
    div(
      style = "
        background-color: #ffffff;
        max-width: 90vw; 
        min-height: 800px;
        margin: 0 auto;
        padding: 30px 40px;
        border-radius: 8px;
        box-shadow: 0 0 12px rgba(0,0,0,0.08);
      ",
      
      # --- Page title ---
      fluidRow(
        column(
          width = 12,
          h3("Population trends", style = "color:#3f5262; font-weight:500; margin-bottom: 20px;"),
          p(HTML("Here you can view population trends....<br>
                            Use the tabs below to explore trends over time ..."),
            style = "font-size:14px; color:#3f5262; margin-bottom: 20px;")
        )
      ),
      
      # --- Tabs container (for future expandability) ---
      tabsetPanel(
        id = ns("trend_tabs"),
        
        # ===== Main population trends tab =====
        tabPanel(
          title = "Trends over time",
          
          # LAY DATE
          fluidRow(
            column(
              width = 12,
              h4("Lay Date Over Time", style = "margin-top: 15px; color:#3f5262;")
            ),
            column(
              width = 4,
              checkboxGroupInput(ns("laydate_species_filter"), "Select species:",
                                 choices = NULL),  # <- will update in server
              radioButtons(ns("laydate_type"), "Lay date type:",
                           choices = c("Mean" = "mean_day",
                                       "Earliest" = "earliest_day",
                                       "Latest" = "latest_day"),
                           selected = "mean_day"),
              checkboxInput(ns("show_points"), "Show points", value = TRUE)
            ),
            column(
              width = 8,
              plotlyOutput(ns("laydate_plot"), height = "600px")
            )
          ),
          
          
          fluidRow(
            column(
              width = 12,
              h4("Number of Broods Per Year", style = "margin-top: 30px; color:#3f5262;"),
              #plotlyOutput(ns("broods_plot"), height = "400px")
            )
          ),
          
          # CLUTCH SIZE
          fluidRow(
            column(
              width = 12,
              h4("Mean Clutch Size Over Time", style = "margin-top: 30px; color:#3f5262;"),
              column(
                width = 4,
                # Species checkbox input
                checkboxGroupInput(
                  ns("clutch_species_filter"),
                  "Select species:",
                  choices = NULL
                ),
                # Show points checkbox
                checkboxInput(ns("clutch_show_points"), "Show points", value = TRUE)
              ),
              column(
                width = 8,
                plotlyOutput(ns("clutch_plot"), height = "500px")
              )
            )
          )
        ),
        
        # ===== Placeholder for future tabs =====
        tabPanel(
          title = "More Analyses",
          fluidRow(
            column(
              width = 12,
              p("Additional analyses and plots will go here.", 
                style = "color:#6c757d; font-style: italic; margin-top: 20px;")
            )
          )
        )
      )
    )
  )
}







# Server function --------------------------------------------------------------

popTrendsServer <- function(id, IndData, BroodData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Initialize species choices dynamically
    observe({
      req(BroodData)
      species_choices <- str_to_title(unique(BroodData$SpeciesName))
      
      # Update Lay Date species filter
      updateCheckboxGroupInput(session, "laydate_species_filter",
                               choices = species_choices,
                               selected = species_choices)
      
      # Update Clutch species filter
      updateCheckboxGroupInput(session, "clutch_species_filter",
                               choices = species_choices,
                               selected = species_choices)
    })
    
    

## Lay date plot -----------------------------------------------------------

    output$laydate_plot <- renderPlotly({
      req(input$laydate_species_filter)
      
      y_var <- input$laydate_type
      
      plot_data <- BroodData %>%
        mutate(SpeciesName = str_to_title(SpeciesName),
               LayDate = as.Date(LayDate),
               DayOfYear = yday(LayDate)) %>%
        filter(SpeciesName %in% input$laydate_species_filter) %>%
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
          )
        )
      
      # Determine axes limits
      # y-axis range
      y_min <- min(c(
        BroodData %>% mutate(DayOfYear = yday(as.Date(LayDate))) %>% pull(DayOfYear)
      ), na.rm = TRUE)
      y_max <- BroodData %>%
        mutate(DayOfYear = yday(as.Date(LayDate))) %>%
        summarise(max_day = max(DayOfYear, na.rm = TRUE)) %>%
        pull(max_day)
      # x-axis range
      x_min <- min(BroodData$BroodYear, na.rm = TRUE)
      x_max <- max(BroodData$BroodYear, na.rm = TRUE)
      
      
      
      # Build plot
      p <- ggplot(plot_data, aes(x = BroodYear, y = .data[[y_var]], colour = SpeciesName, group = SpeciesName)) +
        geom_line(alpha = 0.5)
      
      if (input$show_points) {
        p <- p + geom_point(aes(text = tooltip), size = 2)
      }
      
      p <- p + geom_smooth(aes(group = SpeciesName), method = "lm", se = FALSE, linetype = "dashed") +
        # scale_y_continuous(
        #   breaks = seq(90, 180, by = 10),
        #   labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%d %b")
        # ) +
        scale_y_continuous(
          limits = c(y_min, y_max),
          breaks = seq(y_min, (y_max + 1), by = 10),
          labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%d %b")
        ) +
        scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 10)) +
        scale_colour_viridis(discrete = TRUE, option = "D", end = 0.8) +
        labs(x = "Year", y = "Day of Year", colour = "Species") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    

## Clutch size plot --------------------------------------------------------
    output$clutch_plot <- renderPlotly({
      req(input$clutch_species_filter)
      
      plot_data <- BroodData %>%
        mutate(SpeciesName = str_to_title(SpeciesName)) %>%
        filter(SpeciesName %in% input$clutch_species_filter) %>%
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
            "Mean clutch size: ", round(mean_clutch, 2), "<br>",
            "Range: ", round(min_clutch, 2), " - ", round(max_clutch, 2)
          )
        )
      
      # Axis limits
      y_min <- min(plot_data$mean_clutch, na.rm = TRUE)
      y_max <- max(plot_data$mean_clutch, na.rm = TRUE)
      x_min <- min(plot_data$BroodYear, na.rm = TRUE)
      x_max <- max(plot_data$BroodYear, na.rm = TRUE)
      
      p <- ggplot(plot_data, aes(x = BroodYear, y = mean_clutch, colour = SpeciesName, group = SpeciesName)) +
        geom_line(alpha = 0.5)
      
      if (input$clutch_show_points) {
        p <- p + geom_point(aes(text = tooltip), size = 2)
      }
      
      p <- p +
        geom_smooth(aes(group = SpeciesName), method = "lm", se = FALSE, linetype = "dashed") +
        scale_y_continuous(limits = c(y_min, y_max)) +
        scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 5)) +
        scale_colour_viridis(discrete = TRUE, option = "D", end = 0.8) +
        labs(title = "Mean Clutch Size Over Time",
             x = "Year",
             y = "Mean Clutch Size",
             colour = "Species") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    
    

  })
}