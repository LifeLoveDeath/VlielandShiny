# Vlieland Shiny app
# functions to create individual bird lookup

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)




# Load data - remove this later and just load data in server?
#vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)


# UI function - drop down menus -------------------------------------
# This creates four drop down menus with labels ("Left leg - top ring etc.), place holder text ("Select a colour...") and options (coours list)
# To do:
# Needs icons for the ring colours (or at least a key)
findIndividualUI <- function() {
  colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")
  tagList(
    selectInput("Left1", "Left leg - top ring", choices = c("Select a colour..." = "", colours),
                selected = ""), # this should introduce placeholder text
    selectInput("Left2", "Left leg - bottom ring", choices = c("Select a colour..." = "", colours),
                selected = ""),
    selectInput("Right1", "Right leg - top ring", choices = c("Select a colour..." = "", colours),
                selected = ""),
    selectInput("Right2", "Right leg - bottom ring", choices = c("Select a colour..." = "", colours),
                selected = "")
  )
}

#selectInput is the basic dropdown: https://shiny.posit.co/r/reference/shiny/latest/selectinput.html
#There's also dropdown: https://appsilon.github.io/shiny.fluent/reference/Dropdown.html#ref-examples
#Need to look into narrowing down options as colours are selected: https://stackoverflow.com/questions/75026080/how-can-i-narrow-a-menu-in-r-shiny-based-on-menu-selections

  

# could use  navset_card_underline so can change between individual info, map, family tree etc. https://shiny.posit.co/r/articles/build/layout-guide/
# or Multi-column apps
# maybe a card on the left with individual info and a card with multiple tabs on the right which can switch between map and family tree?




# UI function (old) - with search bar -------------------------------------
# ui_find_individual.R
findIndividualUI_searchbar <- function() {
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
    )
  )
}





# Server function - search data based on dropdowns ---------------------------
# To do:
# Needs to start search as soon as one of the drop downs is selected/narrow down options for other dropdowns
# Notification if none of the dropdowns is set to metal?

findIndividualServer <- function(input, data) {
  matched_data <- reactive({
    req(input$Left1, input$Left2, input$Right1, input$Right2)
    
    match <- data[
      data$ColourRingLeft1 == input$Left1 &
        data$ColourRingLeft2 == input$Left2 &
        data$ColourRingRight1 == input$Right1 &
        data$ColourRingRight2 == input$Right2,
    ]
    
    if (nrow(match) == 0) {
      showNotification("No matching bird found.", type = "error")
      return(NULL)
    }
    
    match <- match[ c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
  })
  return(matched_data)
}

