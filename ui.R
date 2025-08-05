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
# Page title appear in a weird place - formatting needs improving

ui <- navbarPage(
  title = "Great Tits & Blue Tits of Vlieland",
  position = "static-top",
  #bg = viridis(1)[1], # change background colour
  #inverse = TRUE,
  
  # Dropdown menu on the top right
  navbarMenu("Menu", align = "right",
           
  # Defining the pages
  tabPanel("Project info", 
            h3("Project info"), # Title
            "Project info will appear here"), # Place holder text
  
  #tabPanel("Find an individual",
  #          #h3("Find an individual"), # Title
  #            tabPanel("Search for an individual",
  #          tabsetPanel(id = "Find_ind_tabs",
  #           sidebarLayout(
  #             sidebarPanel(
  #               findIndividualUI_withIcons()
  #             ),
  #             mainPanel(
  #               h3("Matching bird record:"),
  #               #reactableOutput("summary_info") # changed because using datatable instead now (below):
  #               DT::dataTableOutput("summary_info"), # selectable datatable
  #               verbatimTextOutput("text"), # validation text
  #               fluidRow(
  #                 #leafletOutput("map", width = "95%", height = "600px") # map area?
  #                 uiOutput("map_ui") # map output shows when row is selected (defined in map server function)
  #               )
  #             )
  #           )
  #            )
  #          )
  #  ),
  
  tabPanel("Find an individual",
           # Two conditional panels: search and individual view
           conditionalPanel(
             condition = "output.birdSelected == false",
             h3("Search for an individual"),
             sidebarLayout(
               sidebarPanel(
                 # Drop down search bars:
                 findIndividualUI_withIcons()
               ),
               mainPanel(
                 DT::dataTableOutput("summary_info"),
                 verbatimTextOutput("text"),
                 fluidRow(
                   #leafletOutput("map", width = "95%", height = "600px") # map area?
                   uiOutput("map_ui") # map output shows when row is selected (defined in map server function)
               )
             )
           )),
           conditionalPanel(
             condition = "output.birdSelected == true",
             tagList (
               h3("Explore individual info"),
               actionButton("back_to_search", "Return to search"),
             tabsetPanel(
               id = "bird_tabs",
               tabPanel("General Info", verbatimTextOutput("bird_general")),
               tabPanel("Map", leafletOutput("bird_map")),
               tabPanel("Pedigree", plotOutput("bird_pedigree"))
             )
           )
  )),
    
  tabPanel("Population trends",
            h3("Poupulation trends")),
    
  tabPanel("Citizen science",
            h3("Citizen science"))
    
  )
)

