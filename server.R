# Vlieland Shiny app
# Sever

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)


# App server ----------------------------------------------

server <- function(input, output) {
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  
  # Store the result in a reactiveVal
  matched_data <- reactiveVal(NULL)
  
  observeEvent(input$searchBird, { # should change this so it starts searching as soon as a dropdown item is selected so it can update the choices for the other dropdowns
    
    req(input$Left1, input$Left2, input$Right1, input$Right2)
    
    match <- vlieland.data[
      vlieland.data$ColourRingLeft1 == input$Left1 &
        vlieland.data$ColourRingLeft2 == input$Left2 &
        vlieland.data$ColourRingRight1 == input$Right1 &
        vlieland.data$ColourRingRight2 == input$Right2, 
    ]
    
    if (nrow(match) == 0) {
      showNotification("No matching bird found.", type = "error")
      matched_data(NULL)
    } else {
      matched_data(match)
    }
  })
  
  # Output the matching data as a table
  output$summary_info <- renderTable({
    matched_data()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}



  

# run with runApp() - calls ui.r and server.r
# or call ui in server script so can use run app button and run as background job for live editing
