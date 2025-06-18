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
