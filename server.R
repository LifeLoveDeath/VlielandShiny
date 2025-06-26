# Vlieland Shiny app
# Sever

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)

# Source files/functions -----------------------------------
source("find_ind_server_functions.R")

# App server ----------------------------------------------

server <- function(input, output, session) {
  #load data
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  
  # Find individual by colour rings - narrow dropdown options as selections made
  findIndividualServer_updateDropdowns(input, vlieland.data, session)
  
  # Find individual by colour rings - perform search
  search_results <- findIndividualServer_search(input, vlieland.data, session)
    
  # Render the matched table    
  output$summary_info <- renderTable({
    df <- search_results() 
    df[,c(1,2, 7,8)]},
    bordered = TRUE, hover = TRUE)
  # This table actually needs to be clickable so they can select the correct ind? renderDataTable?
}



# run with runApp() - calls ui.r and server.r
# or call ui in server script so can use run app button and run as background job for live editing
