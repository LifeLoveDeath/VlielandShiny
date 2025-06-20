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
# Needs icons for the ring colours (or at least a key): https://www.r-bloggers.com/2024/01/icons-in-a-shiny-dropdown-input/
##### Might require HTML rendering and selectizeInput() (can render images in options)
##### https://shiny.posit.co/r/articles/build/selectize/
##### Need to use multiple = FALSE to make it a dropdown rather than textbox?
##### Images of colours are here but not totally clear: https://avianid.co.uk/plastic-striped-split-rings
##### Rendering html https://stackoverflow.com/questions/66884854/use-html-in-selectizeinput-with-r-shiny 


# html rednering for icons - have removed from function
colour_icons <- c(
  "blue" = "blue <div style='width:15px; height:10px; background-color:blue; display:inline-block; margin-right:5px; '></div>",
  "blue/white" = "blue/white",
  "green" = "green",
  "metal" = "metal",
  "orange" = "orange",
  "pink/blue" = "pink/blue",
  "pink/green" = "pink/green",
  "red" = "red",
  "red/white" = "red/white",
  "white" = "white",
  "white/blue" = "white/blue",
  "yellow" = "yellow",
  "yellow/black" = "yellow/black")


findIndividualUI_withIcons <- function() {
  colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")
  
  tagList(
    selectizeInput("Left1", "Left leg - top ring", choices = c("Select a colour..." = "", colours), selected =,
                   options = list(render = I('
    {
      option: function(item, escape) {
        var icons = {
          "blue": "<div style=\\"width:20px; height:10px; background-color:blue; float:right; margin-right:5px;\\"></div>",
          "green": "<div style=\\"width:20px; height:10px; background-color:green; float:right; margin-right:5px;\\"></div>"
        };
        var icon = icons[item.value] || "";
        return "<div style=\\"overflow:hidden;\\">" + escape(item.label) + icon + "</div>";
      },
      item: function(item, escape) {
        var icons = {
          "blue": "<div style=\\"width:20px; height:10px; background-color:blue; float:right; margin-left:5px;\\"></div>",
          "green": "<div style=\\"width:px; height:10px; background-color:green; float:right; margin-left:5px;\\"></div>"
        };
        var icon = icons[item.value] || "";
        return "<div style=\\"overflow:hidden;\\">" + escape(item.label) + icon + "</div>";
      }
    }
  '))),
    selectizeInput(
      "Left2", "Left leg - bottom ring", choices =  c("Select a colour..." = "", colours), selected = ""
    ),
    selectizeInput(
      "Right1", "Right leg - top ring", choices =  c("Select a colour..." = "", colours), selected = ""
    ),
    selectizeInput(
      "Right2", "Right leg - bottom ring", choices =  c("Select a colour..." = "", colours), selected = ""
    )
  )
}

  

  
  
  
 


# Original with basic dropdowns
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
#### One way to do this might be making the started text "select a colour" an actual option"?
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

