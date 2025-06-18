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
    
    observeEvent(input$searchBird, { # should change this to as soon as a dropdown item is selected so it can update the choices for the other dropdowns
      req(input$Left1, input$Left2, input$Right1, input$Right2)
      
      matched <- vlieland.data[
        vlieland.data$ColourRingLeft1 == input$Left1 &
          vlieland.data$ColourRingLeft2 == input$Left2 &
          vlieland.data$ColourRingRight1 == input$Right1 &
          vlieland.data$ColourRingRight2 == input$Right2, 
      ]
      
      if (nrow(matched) == 0) {
        showNotification("No matching bird found.", type = "error")
      } else {
        print(matched)  # This prints the mathing row in the console, need to replace with render
      }
    })
  }
  
  

# run with runApp() - calls ui.r and server.r
# or call ui in server script so can use run app button and run as background job for live editing
