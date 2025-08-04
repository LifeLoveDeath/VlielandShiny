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
#source("individual_search/find_ind_ui_functions.R")

# Can add more files as needed:
files_to_source <- c("individual_search/find_ind_ui_functions.R")
lapply(files_to_source, source)


# App UI ---------------------------------------------------

# This will be strictly layout stuff (panels etc.)
# The rest will call other scripts


ui <- page_navbar(
  title = "Great Tits & Blue Tits of Vlieland",
  bg = viridis(1)[1],
  inverse = TRUE,
  
  # Dropdown menu on the top right
  nav_menu("Menu", align = "right",
           
  nav_panel("Project info"),
  
  nav_panel("Find an individual",
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
                   #leafletOutput("map", width = "95%", height = "600px") # map area?
                   uiOutput("map_ui") # map output shows when row is selected (defined in map server function)
                 )
               )
             )
    ),
    
  nav_panel("Population trends"),
    
  nav_panel("Citizen science")
    
  )
)

