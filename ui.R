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

# Have the top-level menu to dropdown so I can use tabs on the find an individual page
# Page title appear in a weird place - formatting need improving

ui <- navbarPage(
  title = "Great Tits & Blue Tits of Vlieland",
  position = "static-top",
  #bg = viridis(1)[1], # change background colour
  #inverse = TRUE,
  
  # Dropdown menu on the top right
  navbarMenu("Menu", align = "right",
           
  # Defining the pages
  nav_panel("Project info", 
            h3("Project info"), # Title
            "Project info will appear here"), # Placeholder text
  
  nav_panel("Find an individual",
            #h3("Find an individual"), # Title
            tabsetPanel(
              tabPanel("Search for an individual",
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
              )
            )
    ),
    
  nav_panel("Population trends",
            h3("Poupulation trends")),
    
  nav_panel("Citizen science",
            h3("Citizen science"))
    
  )
)

