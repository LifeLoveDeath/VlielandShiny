
# ----------------------------------------------------------------------
# Population Trends Module
# ----------------------------------------------------------------------
#   Placeholder for population-level
#   Has two tabs - first is temporal trends, second could be used for maps etc.
#   Can also add more tabs
#   Have included two example temporal trend plots with filtering by species and trend type:
#     - Mean, earliest, and latest lay dates per year

# Inputs:
#   - IndData: Dataframe containing individual-level information (optional, for future expansion)
#   - BroodData: Dataframe with brood-level information (BroodYear, LayDate, ClutchSize, SpeciesName, etc.)


library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)
library(stringr)
library(lubridate)


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
      
      data <- summarise_laydate(BroodData,
                                species_filter = input$laydate_species_filter,
                                lay_type = input$laydate_type)
      
      plot_time_trend(data, y_var = "y_var", show_points = input$show_points)
    })
    
    

## Clutch size plot --------------------------------------------------------
    output$clutch_plot <- renderPlotly({
      req(input$clutch_species_filter)
      
      data <- summarise_clutch(BroodData, species_filter = input$clutch_species_filter)
      
      plot_time_trend(data, y_var = "mean_clutch", show_points = input$clutch_show_points)
    })
    
    
    

  })
}