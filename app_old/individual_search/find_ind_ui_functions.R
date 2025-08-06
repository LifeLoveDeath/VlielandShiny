# Vlieland Shiny app
# UI functions to create individual bird lookup

## Need to organise/rename all these

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(shinyWidgets)



# Load data - remove this later and just load data in server?
#vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)




## Drop down menus with icons -------------------------------------
## This creates four drop down menus with labels ("Left leg - top ring etc.), place holder text ("Select a colour...") and options (coours list)

# To do:
# Needs icons for the ring colours (or at least a key): https://www.r-bloggers.com/2024/01/icons-in-a-shiny-dropdown-input/
##### Might require HTML rendering and selectizeInput() (can render images in options)
##### https://shiny.posit.co/r/articles/build/selectize/
##### Need to use multiple = FALSE to make it a dropdown rather than textbox?
##### Images of colours are here but not totally clear: https://avianid.co.uk/plastic-striped-split-rings
##### Rendering html https://stackoverflow.com/questions/66884854/use-html-in-selectizeinput-with-r-shiny 


# This might be better for colour icons: https://stackoverflow.com/questions/30486412/r-shiny-custom-icon-image-in-selectinput
# For making the icons? https://www.datanovia.com/en/blog/how-to-create-icon-in-r/


findIndividualUI_withIcons <- function(data) {
  
  colour_rings <- get_colour_rings()
  
  tagList(
    # Old drop down
    #selectizeInput("Left1", "Left leg - top ring", 
    #               choices =  c("", colours), selected = ""
     #              ),
    
    #Formatting for icons
    tags$head(tags$style(HTML("
  .picker-item {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
  }

  .picker-item .text {
    flex-grow: 1;
  }

  .picker-item .icon {
    height: 1em; /* Match the text height */
    width: auto;
    margin-left: 5px;
  }
"))),
    
    # new dropdown format - icons work but search doesn't work anymore (and issues with starting selection/placeholder text)
    # Search actually working ok but option in the dropdown containing icons is not narrowing down
    # Also, for the other format, the "Select colour..." came from somewhere else so this might interfere with the search functions
    pickerInput(inputId = "Left1",
                label = "Left leg - top ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour...",
                #options = list(title = "Select colour...")
                ),
    
    pickerInput(inputId = "Left2",
                label = "Left leg - bottom ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour...",
                #options = list(title = "Select colour...")
    ),
    
    pickerInput(inputId = "Right1",
                label = "Right leg - top ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour...",
                #options = list(title = "Select colour...")
    ),
    
    pickerInput(inputId = "Right2",
                label = "Right leg - bottom ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour...",
                #options = list(title = "Select colour...")
    ),
  
    
    #selectizeInput(
    #  "Left2", "Left leg - bottom ring", 
    #  choices =  c("", colours), selected = ""

    #),
    
    #selectizeInput(
    #  "Right1", "Right leg - top ring", choices =  c("", colours), selected = ""
    #),
    
    #selectizeInput(
    #  "Right2", "Right leg - bottom ring", choices =  c("", colours), selected = ""
    #),
    
    actionButton("reset_filters", "Reset filters"),
  helpText(HTML("placeholder instructions text")))
}


#Old code using html to create logos:
#selectizeInput("Left1", "Left leg - top ring", 
#               choices =  c("", colours), selected = "",
#               options = list(render = I('
#    {
#      option: function(item, escape) {
#        var icons = {
#          "blue": "<div style=\\"width:20px; height:10px; background-color:blue; float:right; margin-right:5px;\\"></div>",
#          "green": "<div style=\\"width:20px; height:10px; background-color:green; float:right; margin-right:5px;\\"></div>"
#        };
#        var icon = icons[item.value] || "";
#        return "<div style=\\"overflow:hidden;\\">" + escape(item.label) + icon + "</div>";
#      },
#      item: function(item, escape) {
#        var icons = {
#          "blue": "<div style=\\"width:20px; height:10px; background-color:blue; float:right; margin-left:5px;\\"></div>",
#          "green": "<div style=\\"width:px; height:10px; background-color:green; float:right; margin-left:5px;\\"></div>"
#        };
#        var icon = icons[item.value] || "";
#        return "<div style=\\"overflow:hidden;\\">" + escape(item.label) + icon + "</div>";
#      }
#    }
#  '))),

# html rednering for icons - have removed from function
colour_icons <- c(
  "blue" = "blue <div style='width:15px; height:10px; background-color:blue; display:inline-block; margin-right:5px; '></div>",
  "blue/white" = "blue/white", # update rest
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




## Basic dropdowns (no icons) --------------
findIndividualUI <- function() {
  colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")
  
  tagList(
    selectInput("Left1", "Left leg - top ring", choices = c("Select a colour..." = "", colours),
                selected = "", # the placeholder text isn't working properly
                options = list(placeholder = 'Select or leave blank...')), # this might introduce a placeholder but would need to update choices and selected on server side to check 
    selectInput("Left2", "Left leg - bottom ring", choices = c("Select a colour..." = "", colours),
                selected = ""),
    selectInput("Right1", "Right leg - top ring", choices = c("Select a colour..." = "", colours),
                selected = ""),
    selectInput("Right2", "Right leg - bottom ring", choices = c("Select a colour..." = "", colours),
                selected = "")
  )
}

findIndividualUI2 <- function() {
  colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green",
               "red", "red/white", "white", "white/blue", "yellow", "yellow/black")
  
  named_colours <- setNames(colours, colours)
  clear_choice <- c("Clear selection" = "")
  
  tagList(
    selectInput("Left1", "Left leg - top ring", 
                choices = c(clear_choice, named_colours),
                selected = clear_choice),
    
    selectInput("Left2", "Left leg - bottom ring", 
                choices = c(clear_choice, named_colours),
                selected = ""),
    
    selectInput("Right1", "Right leg - top ring", 
                choices = c(clear_choice, named_colours),
                selected = ""),
    
    selectInput("Right2", "Right leg - bottom ring", 
                choices = c(clear_choice, named_colours),
                selected = "")
  )
}


#selectInput is the basic dropdown: https://shiny.posit.co/r/reference/shiny/latest/selectinput.html
#There's also dropdown: https://appsilon.github.io/shiny.fluent/reference/Dropdown.html#ref-examples
#Need to look into narrowing down options as colours are selected: https://stackoverflow.com/questions/75026080/how-can-i-narrow-a-menu-in-r-shiny-based-on-menu-selections

  

# could use  navset_card_underline so can change between individual info, map, family tree etc. https://shiny.posit.co/r/articles/build/layout-guide/
# or Multi-column apps
# maybe a card on the left with individual info and a card with multiple tabs on the right which can switch between map and family tree?




## Old search - with search bar -------------------------------------
## ui_find_individual.R
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
