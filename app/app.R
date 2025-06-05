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

server <- function(input, output) {
  
  # do I need to load the data here?
  
  # Tab 2: look up an individual - search result
  # Searches when search button is clicked
  result <- eventReactive(input$goButton, {
    req(input$search)
    
    # Clean search data - need to deal with errors in entry
    # make all lower case and remove spaces
    clean_input <- tolower(trimws(input$search))
    clean_input <- gsub("--", "-", clean_input) 
    clean_input <- gsub("[–—−]", "-", clean_input) # normalises the hyphen, but should maybe change to an easier character
    
    match <- vlieland.data[vlieland.data$ColourRing == clean_input, ] # need to deal with duplicated colour ring combs
    
    # If there are multiple birds with the same colour ring combination, choose most recent born because this will be the one they've seen? But might need to be able to look up previous birds as well?
    if (nrow(match) > 1) {        
      max_year <- max(match$BirthYear, na.rm = TRUE)
      match <- match[match$BirthYear == max_year, ]
    }
    
    
    if (nrow(match) == 0) {
      return(NULL)
    } else {
      match
    }
  })
  
  # Ring number title
  output$ring_number_title <- renderText({
    res <- result()
    if (is.null(res)) {
      "No individual found"
    } else {
      paste("Ring number:", res$RingNumber)
    }
  })
  
  # Summary info as a table
  ## Function to replace missing data with "Unknown"
  replace_missing <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
      return("Unknown")
    } else {
      return(as.character(x))
    }
  }
  
  # Render table
  output$summary_info <- renderTable({
    res <- result()
    if (is.null(res)) return(NULL)
    
    # Create a data.frame for display with named rows
    info_df <- data.frame(
      Info = c("Species:", "Year of birth:"),
      Value = c(
        replace_missing(res$Species),
        replace_missing(res$BirthYear)), # need to deal with missing data
      stringsAsFactors = FALSE
    )
    
    info_df
  }, rownames = FALSE, colnames = FALSE)
  
}






# Run app -------------------------------------------------
# Run the application 
shinyApp(ui = ui_simpleFormat, server = server)
#shinyApp(ui = ui_dropDownFormat, server = server)

