
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
# Required packages
required_packages <- c("shiny", "tidyverse", "bslib", "leaflet", "viridis", "reactable", "shinyjs", "shinyWidgets", "reactable", "DT", "ggpedigree", "ggplot2", "plotly", "imager", "grid", "leaflet.extras2", "leaftime", "leaflet.extras", "bslib", "tools")


# Install any missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the packages
lapply(required_packages, library, character.only = TRUE)

# Source files/functions -----------------------------------
#files_to_source <- c(#"individual_search/find_ind_ui_functions.R",
                     #"individual_search/find_ind_server_functions.R",
#                     "modules/birdFinder_module.R",
#                     "modules/mappingPreview_module.R",
#                     "modules/mappingInd_module.R",
                     #"mapping/generate_map_server_functions.R",
#                     "helpers/colour_ring_data_func.R")  # Can add more files as needed
#lapply(files_to_source, source)
# It sources everything in "R" folder automatically


# App UI ---------------------------------------------------

# Notes and fixes:
# Using conditional tabs so bird info appears when individual selected. Alternative it to dynamically add tabs
# Page title appear in a weird place - formatting needs improving
# I think some of this could be moved into the functions

ui <- navbarPage(
  # theme
  #theme = bs_theme(), 
  
  title = "Great Tits & Blue Tits of Vlieland",
  position = "static-top",
  
  navbarMenu("Menu", align = "right",
             
             # Project info page
             tabPanel("Project info", 
                      h3("Project info"), 
                      "Project info will appear here"),
             
             # Find an individual page
             tabPanel("Find an individual",
                      useShinyjs(),  # enable shinyjs - needed for hidden tabs
                      
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
                              tabPanel("General Info"#, individualInfoUI("individual_info")
                                       ),
                              tabPanel("Map", value = "Map", mapUI("map_individual")),
                              tabPanel("Pedigree", familyTreeUI("pedigree_module"))
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
  # Experimenting with themes
  #bs_themer()
  
  # Load data
  vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
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
  
  # Generate pedigree
  familyTreeServer(
    id = "pedigree_module",
    ped.data = vlieland.data,
    selected_ring = selected_ring
  )
  
  # Generate individual info table
  # individualInfoServer(
  #   id = "individual_info",
  #   vlieland.data = vlieland.data,
  #   selected_ring = selected_ring
  # )
}

shinyApp(ui, server)

