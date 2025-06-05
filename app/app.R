# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)

# Load data
vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)


# Sort data ------------------------------------------------


# App UI ---------------------------------------------------

ui_simpleFormat <- fluidPage(
  # Overall app title
  titlePanel("Great Tits & Blue Tits of Vlieland"),
  
  # Format tabs
  tabsetPanel(
    
    # Tab 1: project info
    tabPanel("Project info"),
    
    # Tab 2: look up an individual
    tabPanel("Find an individual"),
    
    # Tab 3: population-level trends
    tabPanel("Population trends"),
    
    # Tab 4: citizen science data entry
    tabPanel("Citizen science")
  )
)


