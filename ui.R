# Vlieland Shiny app
# UI

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)



# App UI ---------------------------------------------------

ui <- fluidPage(
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
