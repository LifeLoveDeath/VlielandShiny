# Vlieland Shiny app
# function to create individual bird lookup



# Function - drop down menus -------------------------------------

findIndividualUI <- function() {
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
