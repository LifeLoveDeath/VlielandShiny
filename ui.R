# Vlieland Shiny app
# UI

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)


# Source files/functions -----------------------------------
source("ui_find_ind.R")

# App UI ---------------------------------------------------

ui <- fluidPage(
  # Overall app title
  titlePanel("Great Tits & Blue Tits of Vlieland"),
  
  # Format tabs
  tabsetPanel(
    
    # Tab 1: project info
    tabPanel("Project info"),
    
    # Tab 2: look up an individual
    tabPanel("Find an individual",
             findIndividualUI() # calls functions from ui_find_ind script that produces the lookup
    ),
    
    # Tab 3: population-level trends
    tabPanel("Population trends"),
    
    # Tab 4: citizen science data entry
    tabPanel("Citizen science")
  )
)
