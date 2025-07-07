# Vlieland Shiny app
# UI

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(reactable)


# Source files/functions -----------------------------------
source("individual_search/find_ind_ui_functions.R")

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
                 findIndividualUI_withIcons()
               ),
               mainPanel(
                 h3("Matching bird record:"),
                 #reactableOutput("summary_info") # changed because using datatable instead now (below):
                 DT::dataTableOutput("summary_info"), # selectable datatable
                 verbatimTextOutput("text"), # validation text
                 fluidRow(
                   column(4, leafletOutput("map", width = "100%", height = "600px")) # map area?
                 )
               )
             )
    ),
    
    tabPanel("Population trends"),
    
    tabPanel("Citizen science")
    
  )
)

