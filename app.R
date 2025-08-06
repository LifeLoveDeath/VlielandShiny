
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(reactable)


# Source files/functions -----------------------------------
files_to_source <- c("individual_search/find_ind_ui_functions.R",
                     "individual_search/find_ind_server_functions.R",
                     "mapping/generate_map_server_functions.R")  # Can add more files as needed
lapply(files_to_source, source)


# App UI ---------------------------------------------------

# Notes and fixes:
# Using conditional tabs so bird info appears when individual selected. Alternative it to dynamically add tabs
# Page title appear in a weird place - formatting needs improving

ui <- navbarPage(
  title = "Great Tits & Blue Tits of Vlieland",
  position = "static-top",
  #bg = viridis(1)[1], # change background colour
  #inverse = TRUE,
  
  # Dropdown menu on the top right
  navbarMenu("Menu", align = "right",
             
             # Defining the pages
             ## Project info page
             tabPanel("Project info", 
                      h3("Project info"), # Title
                      "Project info will appear here"), # Place holder text
  
             ## Find an individual page
             tabPanel("Find an individual",
                      # Two conditional panels: search and individual view
                      ## Initial search panel:
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
                      
                      ## Individual view panel - appears when bird selected
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
             
             # Population trends page
             tabPanel("Population trends",
                      h3("Poupulation trends")),
             
             # Citizen pages
             tabPanel("Citizen science",
                      h3("Citizen science"))
             
  )
)


# App server ----------------------------------------------

server <- function(input, output, session) {
  #load data
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  
  # Find individual by colour rings - narrows dropdown options as selections made
  findIndividualServer_updateDropdowns(input, vlieland.data, session)
  
  # Find individual by colour rings - perform search
  search_results <- findIndividualServer_search(input, output, vlieland.data, session)
  
  # Generate map
  map <- gen_map(input, output, search_results, session)
  
}


shinyApp(ui, server)

