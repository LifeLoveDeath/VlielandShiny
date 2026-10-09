
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages - for running locally
## Required packages
required_packages <- c(
  "shiny", "tidyverse", "dplyr", "bslib", "leaflet", "viridis", "reactable",
  "shinyjs", "shinyWidgets", "DT", "ggpedigree", "ggplot2", "plotly",
  "grid", "leaflet.extras2", "leaftime", "leaflet.extras", "tools", "shinythemes",
  "fontawesome", "tibble", "htmltools", "RColorBrewer", "kinship2", "osmdata",
  "patchwork", "shiny.i18n"
)

## Install any missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

## Load the packages
lapply(required_packages, library, character.only = TRUE)

# Load packages for deployment
## Explicitly load packages so shinyapps.io detects them
## - this works for publishing app when above code caused "Shiny application failed (exit status 1)." error
# library(shiny)
# library(tidyverse)
# library(dplyr)
# library(reactable)
# library(leaflet)
# library(bslib)
# library(viridis)
# library(shinyjs)
# library(shinyWidgets)
# library(DT)
# library(ggpedigree)
# library(plotly)
# #library(imager) - only needed for creating colour rings
# library(leaflet.extras2)
# library(leaftime)
# library(leaflet.extras)
# library(shinythemes)
# library(fontawesome)
# library(htmltools)
# library(RColorBrewer)
# library(kinship2)
# library(osmdata)
# library(patchwork)


# Translation ------------------------------------------------

i18n <- Translator$new(translation_csvs_path = "translation_data/")
i18n$set_translation_language("nl")


# App UI -----------------------------------------------------


ui <- div(
  
  ## --- Formatting -------------------------------------------
  appThemeUI("appFormat"), # app formatting
  navbarUI("navbarFormat"), # use this for custom title bar
  #navbarShinyThemeUI("navbarShinyTheme"),  # use this navbar formatting if using a shiny theme (below)

  
  ## --- Navbar -----------------------------------------------
  navbarPage(
    # To add favicon - not working
    # header = tags$head(
    #   tags$link(rel = "icon", type = "image/png", href = "favicon.png")
    # ),
    #id = "navbar_id",
    position = "fixed-top",
    windowTitle = i18n$t("app_title"),
    fluid = TRUE,
    collapsible = TRUE,
    #theme = shinytheme("flatly"), # change theme here
    # To change theme, use navbarShinyThemeUI above instead of navbarUI and apply theme
    
    ### --- Title section-------
    title = div(
      id = "logo",
      # nioo logo from /www and if clicked opens NIOO webpage in separate browser
      #tags$a(
        #href = "https://nioo.knaw.nl/en", target = "_blank",
        tags$img(src = "NIOO_logo.svg", height = "30px"),
        #),
      # divider line
      div(class = "divider"),
      # title text and link to home page if clicked
      tags$a(
        href = "/", target = "_self",
        tags$span(i18n$t("app_title"),
        style = "
        color:#3f5262;
        font-weight:300;
        font-size:20px;
        text-transform:uppercase;
        letter-spacing:1px;
        font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
      "
        ))
    ),
    
    ## --- Tab panels -------------------------------------
    
    ### --- Project info homepage -------------------------
    
    tabPanel(i18n$t("app_tab_home"),
             projectInfoUI("projectInfoPage", i18n),
    ),
    
    
    ### --- Find an individual page -------------------------
    
    tabPanel(i18n$t("tab2"),
             useShinyjs(), # for allowing hidden panels
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
                     actionButton("back_to_search", "back_to_search"),
                     uiOutput("selected_bird"),
                     p(HTML(i18n$t("here_you_can_view")),
                       style = "font-size:14px; color:#3f5262; margin-bottom: 20px;"),
                     
                     # --- Tabs ---
                     tabsetPanel(
                       id = "bird_tabs",
                       tabPanel("General Info", individualInfoUI("individual_info", i18n)),
                       tabPanel("Map", value = "Map", mapUI("map_individual")),
                       tabPanel("Family tree", familyTreeUI("pedigree_module"))
                     )
                   )
               )
             )
    ),
    
    ### --- Population trends page -------------------------
    
    tabPanel(
      i18n$t("tab3"),
      populationTrendsUI("popTrends", i18n)
    ),
    
    
    ### --- Citizen science page -------------------------   
    
    tabPanel(i18n$t("tab4"),
             citizenScienceUI()
    ),
    
  ),
  
  hr(),
  
  ## --- Fixed footer text --------------------------------------
  tags$footer(
    # Share this app text
    #tags$span("Share this app: "),
    #add custom buttons or use AddThis
    
    
    # Footer text
    p(i18n$t("footer")),
    align = "right"
  )
  
)


# App server ----------------------------------------------

server <- function(input, output, session) {
  # Experimenting with themes
  #bs_themer()

  # Load data
  IndividualDataVlieland <- readRDS('data/IndividualDataVlieland.rds')
  BroodData <- readRDS("data/BroodDataApp.rds")
  location.data <- readRDS("data/location_data.rds")
  IndividualInfo <- readRDS('data/IndividualInfo.rds')
  
  # Find individual by colour rings
  #finder <- birdFinderServer(input, output, vlieland.data, session) # feeding in dummy data
  finder <- birdFinderServer(input, output, IndividualDataVlieland, session) # feeding in real data
  search_results <- finder$search_results
  selected_ring <- finder$selected_ring
  selected_colours <- finder$selected_colours
  
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
    ped.data = IndividualDataVlieland,
    brood.data = BroodData,
    selected_ring = selected_ring
  )
  
  # Generate individual info table
  individualInfoServer(
    id = "individual_info",
    vlieland.data = IndividualInfo,
    selected_ring = selected_ring,
    selected_colours = selected_colours
  )
  
  
  # Population trends module
  popTrendsServer(
    id = "popTrends",
    BroodData = BroodData
  )
  
  
}




shinyApp(ui, server)

