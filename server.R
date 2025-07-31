# Vlieland Shiny app
# Sever

# Setup ----------------------------------------------------

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)

# Source files/functions -----------------------------------
#source("individual_search/find_ind_server_functions.R")

# Can add more files as needed:
files_to_source <- c("individual_search/find_ind_server_functions.R", "mapping/generate_map_server_functions.R")
lapply(files_to_source, source)

# App server ----------------------------------------------

server <- function(input, output, session) {
  #load data
  vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)
  
  # Find individual by colour rings - narrow dropdown options as selections made
  findIndividualServer_updateDropdowns(input, vlieland.data, session)
  
  # Find individual by colour rings - perform search
  search_results <- findIndividualServer_search(input, output, vlieland.data, session)
  
  # Generate map
  map <- gen_map(input, output, search_results, session)

}



# run with runApp() - calls ui.r and server.r
# or call ui in server script so can use run app button and run as background job for live editing

