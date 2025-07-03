# Vlieland Shiny app
# Server functions to create map for selected bird


# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)


gen_map <- function(input, output, data, selected_row) {
  observeEvent(selected_row(), {
    row <- selected_row()
    if (is.null(row)) return()
    
    selected_ind <- data[row, "RingNumber"]
  
  
}