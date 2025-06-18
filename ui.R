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

# This will be strictly layout stuff (panels etc.)
# The rest will call other scripts


ui <- fluidPage(
  titlePanel("Great Tits & Blue Tits of Vlieland"),
  
  tabsetPanel(
    tabPanel("Project info"),
    
    tabPanel("Find an individual",
             sidebarLayout(
               sidebarPanel(
                 findIndividualUI()
               ),
               mainPanel()
             )
    ),
    
    tabPanel("Population trends"),
    
    tabPanel("Citizen science")
    
  )
)
