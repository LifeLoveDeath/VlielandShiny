# Vlieland Shiny app

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(vi)

# Load data
vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)


# Sort data ------------------------------------------------


# App UI ---------------------------------------------------

ui_simpleFormat <- fluidPage(
  # Overall app title
  titlePanel("Great Tits & Blue Tits of Vlieland"),
  
  # Format tabs
  tabsetPanel(
    
    # Tab 1: project info
    tabPanel("Project info"),
    
    # Tab 2: look up an individual
    tabPanel("Find an individual",
             sidebarLayout(
               sidebarPanel(
                 textInput("search", label = "Search for an individual by colour ring sequence:",
                           placeholder = "e.g. white-black-metal-red"),
                 actionButton("goButton", "Search"),
                 helpText(HTML("<strong>How to enter a colour ring sequence:</strong><br>
                 <ul>
                 <li>Start with the <strong>bird's left leg</strong>, then the <strong>right leg</strong></li>
                 <li>For each leg, enter colours from <strong>top to bottom</strong></li>
                 <li>Every bird has a metal ring; include it as <strong>metal</strong> in the correct position within the colour sequence</li>
                 <li>Use <strong>dashes</strong> to separate colours (e.g. white-black-metal-red)</li>
                 </ul>
                               "))
               ),
               
               mainPanel(
                 h3(textOutput("ring_number_title")),
                 fluidRow(
                   column(12,
                          tableOutput("summary_info")
                   )
                 ),
                 hr()
               )  # closes mainPanel
               
             )  # closes sidebarLayout
    ),   # closes tabPanel("Find an individual")
    
    # Tab 3: population-level trends
    tabPanel("Population trends"),
    
    # Tab 4: citizen science data entry
    tabPanel("Citizen science")
  )
)



### Alternative format --------------------

# Using page_navbar - with a drop down menu
ui_dropDownFormat <- page_navbar(
  title = "Great Tits & Blue Tits of Vlieland",
  bg = viridis(1)[1],
  inverse = TRUE,
  # Panel 1: project info
  nav_panel(title = "Project info"),
  
  # Panel 2: individual look-up
  nav_panel(title = "Find an individual"),
  
  # Panel 3: population trends
  nav_panel(title = "Population trends"),
  
  # Panel 4: citizen science data entry
  nav_panel(title = "Citizen science"),
  
  # Links dropdown
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(tags$a("Vlieland project", href = "https://nioo.knaw.nl/en/facilities/hole-breeding-passerines-monitoring-vlieland?_gl=1#*uxa8vu*_ga*NTk1NjQ0Njk4LjE3NDcyMjcyNzg.*_ga_HTXYJ4973R*czE3NDgzNDI0OTMkbzQkZzEkdDE3NDgzNDI1MDMkajAkbDAkaDA.")),
    nav_item(tags$a("Netherlands Insitute of Ecology", href = "https://nioo.knaw.nl/en"))
  )
)






# App server ----------------------------------------------

server <- function(input, output) {}






# Run app -------------------------------------------------
# Run the application 
shinyApp(ui = ui_simpleFormat, server = server)
#shinyApp(ui = ui_dropDownFormat, server = server)

