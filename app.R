
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
# Required packages
required_packages <- c("shiny", "tidyverse", "bslib", "leaflet", "viridis", "reactable", "shinyjs", "shinyWidgets", "reactable", "DT", "ggpedigree", "ggplot2", "plotly", "imager", "grid", "leaflet.extras2", "leaftime", "leaflet.extras", "bslib", "tools", "shinythemes", "fontawesome", "tibble")

# Install any missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the packages
lapply(required_packages, library, character.only = TRUE)


# App UI ---------------------------------------------------


ui <- div(
  
  ## --- Formatting -------------------------------------------
  appThemeUI("appFormat"), # app formatting
  
  
  ## --- Navbar ----------------------------------------------
  navbarPage(
    id = "navbar_id",
    position = "fixed-top",
    windowTitle = "Vlieland Great Tits",
    fluid = TRUE,
    collapsible = TRUE,
    #theme = shinytheme("sandstone"), # change theme here - shouldn't override custom css formatting (as it is marked "!important") but will apply to rest of app
    # To change theme, remove appThemeUI and apply theme? Messes up spacing a bit - need to fix this
    
    ### --- Title section-------
    
    title = div(
      id = "logo",
      # nioo logo from /www and link to webpage
      tags$a(
        href = "https://nioo.knaw.nl/en", target = "_blank",
        tags$img(src = "nioo_logo.svg", height = "30px")
      ),
      # divider line
      div(class = "divider"),   
      #title text
      tags$span("Vlieland Great Tits", 
                style = "
    color:#3f5262;
    font-weight:300;
    font-size:20px;
    text-transform:uppercase;
    letter-spacing:1px;
    font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
  "
      )
    ),
    
    ## --- Tab panels -------------------------------------
    
    ### --- Project info homepage -------------------------
    
    tabPanel("About this project and app",
             projectInfoUI("projectInfoPage"),
    ),
    
    
    ### --- Find an individual page -------------------------
    
    tabPanel("Find an individual",
             useShinyjs(),
             hidden(
               div(
                 id = "search_panel",
                 birdFinderUI("birdFinder")  # birdFinderUI now produces the full layout
               )
             ),
             
             hidden(
               # Sort formatting to match other pages
               div(id = "individual_panel",
                   style = "background-color: #f2f4f5; padding: 5px;",
                   
                   # --- White panel container ---
                   div(
                     style = "
          background-color: #ffffff;
          max-width: 90vw;   
          min-height: 800px;
          margin: 0 auto;
          padding: 30px 40px;
          border-radius: 8px;
          box-shadow: 0 0 12px rgba(0,0,0,0.08);
        ",
                     
                     # --- Back button and selected bird ---
                     actionButton("back_to_search", "Return to search"),
                     uiOutput("selected_bird"),
                     
                     # --- Tabs ---
                     tabsetPanel(
                       id = "bird_tabs",
                       tabPanel("General Info"),
                       tabPanel("Map", value = "Map", mapUI("map_individual")),
                       tabPanel("Pedigree", familyTreeUI("pedigree_module"))
                     )
                   )
               )
             )
    ),
    
    ### --- Population trends page -------------------------
    
    tabPanel("Population trends",
             populationTrendsUI()
    ),
    
    
    ### --- Citizen science page -------------------------   
    
    tabPanel("Citizen science",
             citizenScienceUI()
    ),
    
  ),
  
  hr(),
  
  ## --- Fixed footer text --------------------------------------
  tags$footer(
    HTML("<p>Footer text e.g. Contact or Copyright © 2025 — All Rights Reserved.</p>"),
    align = "right"
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
    h3(HTML(paste0(
      "Explore individual info: Ring number ",
      "<span style='color:#dd4f2a;'>", selected_ring(), "</span>"
    )), style = "color:#3f5262; font-weight:500;")
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

