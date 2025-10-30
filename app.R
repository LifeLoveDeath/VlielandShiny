
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


# App UI (old - no formatting) ---------------------------------------------------

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



# App UI (with formatting) ---------------------------------------------------


ui <- div(
  
  ## --- Formatting  -------------------------------------------------
  # Shiny theme won't override custom title panel formatting marked "!important" but will apply to the rest of the app
  # To change title theme, edit custom formatting or remove custom formatting and apply the theme to the whole app
  tags$style(HTML("
  
    /* title bar background colour */
    .navbar.navbar-default {
    background-color: #ffffff !important;
    border-color: #e7e7e7 !important;
  }

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
  
    /* Hamburger icon background colour */
    .navbar-toggle {
      background-color: transparent !important;
      border-color: #cccccc !important;
    }

    /* Hamburger bars colour */
    .navbar-toggle .icon-bar {
      background-color: #888 !important;
    }
    
      /* Hover and focus state */
    .navbar-default .navbar-toggle:hover,
    .navbar-default .navbar-toggle:focus {
        background-color: #e7e7e7 !important; 
        border-color: #cccccc !important; 
    }
    

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
  # Non-collapsed menu (horizontal)
  tags$style(HTML("
    /* Normal menu items */
    .navbar-default .navbar-nav > li > a {
      color: #3f5262 !important;  /* default text color */
      font-size: 16px;
      font-weight: 500;
      text-decoration: none;       /* no underline */
      letter-spacing:0.5px
    }
    
    /* Hover state: underline and color change */
    .navbar-default .navbar-nav > li > a:hover {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    
    /* Active (selected) tab: underline and color change */
    .navbar-default .navbar-nav:not(.in) > .active > a {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    
    /* Active tab on hover */
    .navbar-default .navbar-nav:not(.in) > .active > a:hover {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    ")),
      
      ## Collapsed menu (hamburger)
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
    
    
    # Make sure formatting is correct during collapsing animation:
        /* Collapsed menu items (hamburger) */
    .navbar-collapse.in .navbar-nav > li > a,
    .navbar-collapse.collapsing .navbar-nav > li > a {
      color: white !important;
      background-color: #004b84 !important; /* dark blue background */
      text-decoration: none !important;
    }

    /* Hover state in hamburger */
    .navbar-collapse.in .navbar-nav > li > a:hover,
    .navbar-collapse.collapsing .navbar-nav > li > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a,
    .navbar-collapse.collapsing .navbar-nav > .active > a {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item hover in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a:hover,
    .navbar-collapse.collapsing .navbar-nav > .active > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
      /* Force text white during collapse animation */
    .navbar-collapse.collapsing .navbar-nav > li > a {
        color: white !important;
    }
    ")),

  
  ## --- Navbar ----------------------------------------------
  navbarPage(
    id = "navbar_id",
    position = "fixed-top",
    windowTitle = "Vlieland Great Tits & Blue Tits",
    fluid = TRUE,
    collapsible = TRUE,
    theme = shinytheme("flatly"), # change theme here - shouldn't overide custom css formatting above (as it is marked "!important" but will apply to rest of app)
    
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

    ### --- Project info homepage -------------------------
    tabPanel("Project info",
             #h3("Project info", style = "color:#3f5262; font-weight:500;"),
             # Page wrapper
             div(
               style = "padding: 20px; background-color: #ffffff;",
               
               #### --- Top section with optional image ----
          #      fluidRow(
          #        column(
          #          width = 12,
          #          style = "position: relative; text-align: center; margin-bottom: 30px;",
          #          # Placeholder for hero image or banner
          #          tags$img(
          #            src = "passerine_proj_background.jpg",
          #            alt = "Project background image",
          #            style = "width: 100%; max-height: 300px; object-fit: cover; border-radius: 5px;"
          #          ),
          #          # Overlayed title text
          #          tags$div(
          #            style = "
          #   position: absolute;
          #   top: 50%;
          #   left: 50%;
          #   transform: translate(-50%, -50%);
          #   color: white;
          #   font-size: 32px;
          #   font-weight: 700;
          #   text-shadow: 1px 1px 3px rgba(0,0,0,0.7);
          # ",
          #            "Vlieland Great Tits & Blue Tits"
          #          )
          #        )
          #      ),
          
          
          fluidRow(
            column(
              width = 12,
              style = "position: relative; padding: 0;",  # remove padding so image spans full width
              # Full-width hero image
              tags$img(
                src = "passerine_proj_background.jpg",
                alt = "Project background image",
                style = "width: 100%; height: 400px; object-fit: cover;"  # full width, fixed height
              ),
              
              # Overlayed info card on the left
              tags$div(
                style = "
        position: absolute;
        top: 50%;
        left: 5%;
        transform: translateY(-50%);
        background-color: rgba(255, 255, 255, 0.85);  /* semi-transparent white */
        padding: 20px;
        border-radius: 8px;
        max-width: 400px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.3);
      ",
                h2("About the Project", style = "color: #3f5262;"),
                p("Placeholder for a brief introduction to the research project.",
                  style = "font-size: 16px; line-height: 1.6;")
              )
            )
          ),
               
               #### --- Intro / overview section ----
               # fluidRow(
               #   column(
               #     width = 12,
               #     h2("About the Project", style = "color: #3f5262;"),
               #     p("Placeholder for a brief introduction to the research project.", 
               #       style = "font-size: 16px; line-height: 1.6;")
               #   )
               # ),
               
               fluidRow(
                 column(
                   width = 6,
                   h2("About the Project", style = "color: #3f5262;"),
                   p("Placeholder for a brief introduction to the research project.", 
                     style = "font-size: 16px; line-height: 1.6;")
                 ),
                 column(
                   width = 6,
                   tags$img(
                     src = "passerine_proj_background.jpg",  # replace with your image filename
                     style = "width: 100%; height: auto; border-radius: 8px;"
                   )
                 )
               ),
               
               #### --- What the App Does section ----
               fluidRow(
                 column(
                   width = 12,
                   h2("About This App", style = "color: #3f5262; margin-top: 30px;"),
                   p("Placeholder describing the app's functionality: searching for individual birds, exploring population trends and contributing to citizen science.", 
                     style = "font-size: 16px; line-height: 1.6;")
                 )
               ),
               
               #### --- Features / cards section ----
               fluidRow(
                 column(
                   width = 4,
                   wellPanel(
                     h4("Find an individual"),
                     p("Search for a bird by its color rings and explore its data, including general information, a map of its breeding sites and its family tree."),
                     style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     background-color: #f8f9fa;
                     height: 170px;"
                   )
                 ),
                 column(
                   width = 4,
                   wellPanel(
                     h4("Population Trends"),
                     p("Placeholder: view population trends and visualise analysis."),
                     style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     background-color: #f8f9fa;
                     height: 170px;"
                   )
                 ),
                 column(
                   width = 4,
                   wellPanel(
                     h4("Citizen Science"),
                     p("Placeholder: contribute data or observations to citizen science."),
                     style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     background-color: #f8f9fa;
                     height: 170px;"
                   )
                 )
               ),
               
               #### --- Optional image section ----
               fluidRow(
                 column(
                   width = 6,
                   tags$img(
                     src = "passerine_proj_background.jpg",
                     alt = "Image 1",
                     style = "width: 100%; border-radius: 5px; margin-top: 30px;"
                   )
                 ),
                 column(
                   width = 6,
                   tags$img(
                     src = "passerine_proj_background.jpg",
                     alt = "Image 2",
                     style = "width: 100%; border-radius: 5px; margin-top: 30px;"
                   )
                 )
               ),
               
               #### --- Footer / contact section ----
               fluidRow(
                 column(
                   width = 12,
                   style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
                   h4("Contact / References"),
                   p("Placeholder for contact info, acknowledgements, or references.", style = "font-size: 14px;")
                 )
               )
             )
    ),
    
    
 ### --- Find an individual page -------------------------
   
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

 ### --- Population trends page -------------------------
 
    tabPanel("Population trends",
             h3("Population trends", style = "color:#3f5262; font-weight:500;")
    ),

 
 ### --- Citizen science page -------------------------   
 
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

