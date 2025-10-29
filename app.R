
# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
# Required packages
required_packages <- c("shiny", "tidyverse", "bslib", "leaflet", "viridis", "reactable", "shinyjs", "shinyWidgets", "reactable", "DT", "ggpedigree", "ggplot2", "plotly", "imager", "grid", "leaflet.extras2", "leaftime", "leaflet.extras", "bslib", "tools", "shinythemes")


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

# ui <- #navbarPage(
#   #page_navbar(
#   # theme
#   #theme = shinytheme("sandstone"), 
#     
#     navbarPage(
#   #title = "Vlieland Great Tits & Blue Tits",
#   id = "navbar_id",
#   # Title with logo, divider, and text
#   title = tags$div(
#     tags$a(href = "https://nioo.knaw.nl/en", target = "_blank",
#            tags$img(src = "nioo_logo.svg", height = "30px", style = "margin-right:10px;")),
#     tags$span(style = "border-left:3px solid white; height:30px; display:inline-block; margin-left:10px; margin-right:10px;"),
#     tags$span("Vlieland Great Tits & Blue Tits", style = "color:white;")
#   ),
#   
#    bg = "#004b84",
#   # inverse = TRUE,
#   # position = "static-top",
#   
#   # Custom CSS in header
#   header = tags$head(
#     tags$style(HTML("
#       /* Make navbar taller */
#       .navbar { min-height: 40px; }
# 
#       /* Float the 'Menu' dropdown to the right */
#       .navbar-nav > .dropdown { float: right; }
#     "))
#   ),
#   
#   navbarMenu("Menu", align = "right",
#              
#              # Project info page
#              tabPanel("Project info", 
#                       h3("Project info"), 
#                       "Project info will appear here"),
#              
#              # Find an individual page
#              tabPanel("Find an individual",
#                       useShinyjs(),  # enable shinyjs - needed for hidden tabs
#                       
#                       # Search panel
#                       hidden(
#                         div(id = "search_panel",
#                             h3("Search for an individual"),
#                             sidebarLayout(
#                               sidebarPanel(
#                                 birdFinderUI()
#                               ),
#                               mainPanel(
#                                 DT::dataTableOutput("summary_info"),
#                                 fluidRow(
#                                   uiOutput("map_preview_ui") # preview map
#                                 )
#                               )
#                             )
#                         )
#                       ),
#                       
#                       # Individual view panel
#                       hidden(
#                         div(id = "individual_panel",
#                             actionButton("back_to_search", "Return to search"),
#                             uiOutput("selected_bird"),
#                             tabsetPanel(
#                               id = "bird_tabs",
#                               tabPanel("General Info"#, individualInfoUI("individual_info")
#                                        ),
#                               tabPanel("Map", value = "Map", mapUI("map_individual")),
#                               tabPanel("Pedigree", familyTreeUI("pedigree_module"))
#                             )
#                         )
#                       )
#              ),
#              
#              # Population trends page
#              tabPanel("Population trends",
#                       h3("Population trends")),
#              
#              # Citizen science page
#              tabPanel("Citizen science",
#                       h3("Citizen science"))
#   )
# )


ui <- div(
  
  ## --- Formatting  -------------------------------------------------
  tags$style(HTML("

    /* spacing between navbar and page content */
    body > div > .container-fluid:nth-of-type(1) {
        margin: 0 auto;
        padding-top: 65px;
    }

    /* align menu to the right */
    body > div > nav .nav.navbar-nav {
        float: right !important;
    }

    /* keep logo on the left */
    .navbar-header {
        float: left !important;
    }

    /* layout for title+logo */
    #logo {
        display: flex;
        align-items: center;
        gap: 10px;
    }

    /* vertical divider */
    .divider {
        border-left: 1px solid #004b84;
        height: 30px;
        margin-right: 10px;
    }

    /* tab menu spacing */
    .nav-tabs > li {
        float: left;
        margin-bottom: -1px;
        padding-right: 100px;
    }
  ")),
  
  ### --- Collapse menu when screen is narrow ------------
  tags$style(HTML("
  /* collapse navbar earlier */
  @media (max-width: 1150px) {

    /* show hamburger toggle button */
    .navbar-toggle {
      display: block !important;
    }

    /* prevent nav items staying on one line */
    .navbar-nav {
      float: none !important;
    }

    /* stacked menu items */
    .navbar-nav > li {
      float: none !important;
    }

    /* title area centered on collapse */
    .navbar-header {
      float: none !important;
    }

    /* ensure menu actually collapses/expands */
    .navbar-collapse.collapse {
      display: none !important;
    }
    .navbar-collapse.in {
      display: block !important;
    }
  }
")),
  
  # Close dropdown when menu item selected
  tags$script(HTML("
    $(document).on('click', '.navbar-collapse.in a', function() {
      $('.navbar-collapse').collapse('hide');
    });
  ")),
  
  ### Make this dropdown background blue -----------
  tags$style(HTML("
  @media (max-width: 1150px) {

    /* force background before/during/after collapse */
    .navbar-default .navbar-collapse,
    .navbar-default .navbar-collapse.collapsing,
    .navbar-default .navbar-collapse.in {
      background-color: #004b84 !important;
    }

    /* menu link colors */
    .navbar-default .navbar-nav > li > a {
      color: white !important;
    }

    .navbar-default .navbar-nav > li > a:hover {
      background-color: #033a67 !important;
      color: #ffffff !important;
    }
    
     /* selected menu item formatting */
    .navbar-default .navbar-nav > .active > a {
      background-color: #033a67 !important;
      color: white !important;
    }

    /* hover state for active tab formatting */
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #033a67 !important;
      color: white !important;
    }

  }
")), 

  
  ### Formatting menu item text (both versions) ----------
  # --- Non-collapsed menu (horizontal) --------------------------
  tags$style(HTML("
/* Normal menu items */
.navbar-default .navbar-nav > li > a {
  color: #3f5262 !important;  /* default text color */
  font-size: 16px;
  font-weight: 500;
  text-decoration: none;       /* no underline */
  letter-spacing: 1px;
}

/* Hover state: underline and color change */
.navbar-default .navbar-nav > li > a:hover {
  text-decoration: underline !important;
  color: #0d5088 !important;  
}

/* Active (selected) tab: underline and color change */
.navbar-default .navbar-nav > .active > a {
  text-decoration: underline !important;
  color: #0d5088 !important;
}

/* Active tab on hover */
.navbar-default .navbar-nav > .active > a:hover {
  text-decoration: underline !important;
  color: #0d5088 !important;
}
")),
  
  # --- Collapsed menu (hamburger) --------------------------
  tags$style(HTML("
/* Collapsed menu items inside hamburger */
.navbar-collapse.in .navbar-nav > li > a {
  color: white !important;  /* keep white text */
  background-color: #004b84 !important;  /* dark blue background */
  text-decoration: none !important;
}

/* Hover state in hamburger */
.navbar-collapse.in .navbar-nav > li > a:hover {
  background-color: #033a67 !important;
  color: white !important;
  text-decoration: underline !important;
}

/* Active item in hamburger */
.navbar-collapse.in .navbar-nav > .active > a {
  background-color: #033a67 !important;
  color: white !important;
  text-decoration: underline !important;
}

/* Active item hover in hamburger */
.navbar-collapse.in .navbar-nav > .active > a:hover {
  background-color: #033a67 !important;
  color: white !important;
  text-decoration: underline !important;
}
")),

  
  ## --- Navbar ----------------------------------------------
  navbarPage(
    id = "navbar_id",
    position = "fixed-top",
    windowTitle = "Vlieland Great Tits & Blue Tits",
    fluid = TRUE,
    collapsible = TRUE,
    
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
      tags$span("Vlieland Great Tits & Blue Tits", 
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
    # (aligned right)
    tabPanel("Project info",
             h3("Project info", style = "color:#3f5262; font-weight:500;"),
             "Project info will appear here."
    ),
    
    tabPanel("Find an individual",
             useShinyjs(),
             hidden(
               div(id = "search_panel",
                   h3("Search for an individual", style = "color:#3f5262; font-weight:500;"),
                   sidebarLayout(
                     sidebarPanel(
                       birdFinderUI()
                     ),
                     mainPanel(
                       DT::dataTableOutput("summary_info"),
                       fluidRow(
                         uiOutput("map_preview_ui")
                       )
                     )
                   )
               )
             ),
             hidden(
               div(id = "individual_panel",
                   actionButton("back_to_search", "Return to search"),
                   uiOutput("selected_bird"),
                   tabsetPanel(
                     id = "bird_tabs",
                     tabPanel("General Info"),
                     tabPanel("Map", value = "Map", mapUI("map_individual")),
                     tabPanel("Pedigree", familyTreeUI("pedigree_module"))
                   )
               )
             )
    ),
    
    tabPanel("Population trends",
             h3("Population trends", style = "color:#3f5262; font-weight:500;")
    ),
    
    tabPanel("Citizen science",
             h3("Citizen science", style = "color:#3f5262; font-weight:500;")
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

