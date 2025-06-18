# Vlieland Shiny app
# function to create individual bird lookup

# Load data - remove this later and just load data in server?
#vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
colours <- c("red", "white", "blue", "yellow/black","red/white", "blue/white", "white-blue", "white", "yellow", "orange", "green", "pink/blue", "pink/green")

# Function - drop down menus -------------------------------------

findIndividualUI <- function() {
         selectInput(
           inputId = "LeftLegRing1DropDown",
           label = "Left leg, ring 1",
           choices = colours
         )}
  
  
  

#selectInput is the basic dropdown: https://shiny.posit.co/r/reference/shiny/latest/selectinput.html
#There's also dropdown: https://appsilon.github.io/shiny.fluent/reference/Dropdown.html#ref-examples
#Need to look into narrowing down options as colours are selected: https://stackoverflow.com/questions/75026080/how-can-i-narrow-a-menu-in-r-shiny-based-on-menu-selections

  

# could use  navset_card_underline so can change between individual info, map, family tree etc. https://shiny.posit.co/r/articles/build/layout-guide/
# or Multi-column apps
# maybe a card on the left with individual info and a card with multiple tabs on the right which can switch between map and family tree?








# Function (old) - with search bar -------------------------------------
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
