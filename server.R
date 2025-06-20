# Vlieland Shiny app
# Sever

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)

# Source files/functions -----------------------------------
source("find_ind_functions.R")

# App server ----------------------------------------------

server <- function(input, output, session) { # not sure if I'll use session?
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  
  matched_data <- findIndividualServer(input, vlieland.data, session)
  
  # Render the matched table      # this bit stays in server.r?
  output$summary_info <- renderTable({
    matched_data()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}


  

# run with runApp() - calls ui.r and server.r
# or call ui in server script so can use run app button and run as background job for live editing
