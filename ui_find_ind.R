# Vlieland Shiny app
# function to create individual bird lookup



# Function - drop down menus -------------------------------------

findIndividualUI <- function() {
  page_fillable(
    
    card(card_header("Search for an individual bird by colour ring sequence:"),
         selectInput(
           inputId = "dropdown_example",
         
         ))}
  
  
  
  
  

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
