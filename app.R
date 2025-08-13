
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(reactable)
library(shinyjs)


# Source files/functions -----------------------------------
files_to_source <- c(#"individual_search/find_ind_ui_functions.R",
                     #"individual_search/find_ind_server_functions.R",
                     "modules/birdFinder_module.R",
                     "modules/mappingPreview_module.R",
                     "modules/mappingInd_module.R",
                     #"mapping/generate_map_server_functions.R",
                     "helpers/colour_ring_data_func.R")  # Can add more files as needed
lapply(files_to_source, source)


# App UI ---------------------------------------------------

# Notes and fixes:
# Using conditional tabs so bird info appears when individual selected. Alternative it to dynamically add tabs
# Page title appear in a weird place - formatting needs improving
# I think some of this could be moved into the functions

ui <- navbarPage(
  title = "Great Tits & Blue Tits of Vlieland",
  position = "static-top",
  
  navbarMenu("Menu", align = "right",
             
             # Project info page
             tabPanel("Project info", 
                      h3("Project info"), 
                      "Project info will appear here"),
             
             # Find an individual page
             tabPanel("Find an individual",
                      useShinyjs(),  # enable shinyjs
                      
                      # Search panel
                      hidden(
                        div(id = "search_panel",
                            h3("Search for an individual"),
                            sidebarLayout(
                              sidebarPanel(
                                birdFinderUI()
                              ),
                              mainPanel(
                                DT::dataTableOutput("summary_info"),
                                fluidRow(
                                  uiOutput("map_preview_ui") # preview map
                                )
                              )
                            )
                        )
                      ),
                      
                      # Individual view panel
                      hidden(
                        div(id = "individual_panel",
                            actionButton("back_to_search", "Return to search"),
                            uiOutput("selected_bird"),
                            tabsetPanel(
                              id = "bird_tabs",
                              tabPanel("General Info"),
                              tabPanel("Map", value = "Map", mapUI("map_individual")),
                              tabPanel("Pedigree", plotOutput("bird_pedigree"))
                            )
                        )
                      )
             ),
             
             # Population trends page
             tabPanel("Population trends",
                      h3("Population trends")),
             
             # Citizen science page
             tabPanel("Citizen science",
                      h3("Citizen science"))
  )
)


# App server ----------------------------------------------

server <- function(input, output, session) {
  # Load data
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)
  
  # Find individual by colour rings
  finder <- birdFinderServer(input, output, vlieland.data, session)
  search_results <- finder$search_results
  selected_ring <- finder$selected_ring
  
  # Reactive title for individual info page
  output$selected_bird <- renderUI({
    req(selected_ring())
    h3(paste0("Explore individual info: ", selected_ring()))
  })
  
  # Show/hide panels based on selection
  observe({
    if (is.null(selected_ring())) {
      shinyjs::show("search_panel")
      shinyjs::hide("individual_panel")
    } else {
      shinyjs::hide("search_panel")
      shinyjs::show("individual_panel")
    }
  })
  
  # Back button to reset selection
  observeEvent(input$back_to_search, {
    selected_ring(NULL)
    shinyjs::show("search_panel")
    shinyjs::hide("individual_panel")
  })
  
  # Generate preview map
  map_preview <- genPreviewMapServer(input, output, search_results, location.data, session)
  
  # Generate interactive map (always present)
  map_individual <- genMapServer("map_individual", location.data, selected_ring)
}

shinyApp(ui, server)

