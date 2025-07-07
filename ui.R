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
               mainPanel( # area to show results
                 h3("Matching bird record:"), # heading
                 #reactableOutput("summary_info") # table placeholder filled by "output$summary_output <- " in server code
                 DT::dataTableOutput("summary_info"), # changed to this to change the clickable datatable
                 verbatimTextOutput("text")
               )
             )
    ),
    
    tabPanel("Population trends"),
    
    tabPanel("Citizen science")
    
  )
)

