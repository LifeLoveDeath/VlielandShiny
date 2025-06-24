# Vlieland Shiny app
# Server functions to create individual bird lookup

## Need to organise/rename all these

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)



## Search data based on dropdowns ---------------------------
# To do:
# Needs to start search as soon as one of the drop downs is selected/narrow down options for other dropdowns
#### One way to do this might be making the started text "select a colour" an actual option"?
# Notification if none of the dropdowns is set to metal? But actually this won't be possible once the options narrow down
# Below - alternative that narrows dropdown options as selections made
# This one - no placeholder text "Select colour..." so that seems to only come from the server side function (even thought it's also in the UI side function)

findIndividualServer2 <- function(input, data, session) {
  matched_data <- reactive({
    req(input$Left1, input$Left2, input$Right1, input$Right2)
    
    match <- data[
      data$ColourRingLeft1 == input$Left1 &
        data$ColourRingLeft2 == input$Left2 &
        data$ColourRingRight1 == input$Right1 &
        data$ColourRingRight2 == input$Right2,
    ]
    
    if (nrow(match) == 0) {
      showNotification("No matching bird found.", type = "error")
      return(NULL)
    }
    
    match <- match[ c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
  })
  return(matched_data)
}



## Narrow dropdown options as selections made ----------------
# https://www.appsilon.com/post/observe-function-r-shiny

# The options for selected etc. here overide those in UI



# This changes the dropdown options based on selections made but need it to also search and return matching records
# Currently, backspace clears the selection but need a clickable "clear" option (or keep the "Select..." option)
# Also, when selection is made in one dropbox, the options for that dropbox are restricted to that value (even if only one selection has been made) - so you can't change the option (unless you clear the selection with backspace)
# Now the "Select colour..." text remains and clears the search
# Probably also need a button that clears all
# Not sure the other drop downs re-update when a selection is cleared using 'Select a colour' - maybe it is
# But when you make one selection, the selection for that dropdown becomes the selection made and " Select a colour' (to clear) and it should keep all the options availble until they're no longer possible due to other selecitions
# All options come back when you reset all dropdowns to "Select a colour" though

findIndividualServer_updateDropdowns <- function(input, data, session) {
  
  # Reactive observation - triggers output whenever one of the inputs changes
  observe({
    # Filter the data based on current selections
    filtered <- data
    
    if (input$Left1 != "" & input$Left1 != "clear") {
      filtered <- filtered[filtered$ColourRingLeft1 == input$Left1, ]
    }
    if (input$Left2 != "" & input$Left2 != "clear") {
      filtered <- filtered[filtered$ColourRingLeft2 == input$Left2, ]
    }
    if (input$Right1 != "" & input$Right1 != "clear") {
      filtered <- filtered[filtered$ColourRingRight1 == input$Right1, ]
    }
    if (input$Right2 != "" & input$Right2 != "clear") {
      filtered <- filtered[filtered$ColourRingRight2 == input$Right2, ]
      return(filtered) #think this needs to be in a reactive
    }
    
    # Extract remaining possible selections from filtered data
    options_Left1  <- unique(filtered$ColourRingLeft1)
    options_Left2  <- unique(filtered$ColourRingLeft2)
    options_Right1 <- unique(filtered$ColourRingRight1)
    options_Right2 <- unique(filtered$ColourRingRight2)
    
    # Update all dropdowns (but keep current selection)
    updateSelectInput(session, "Left1", choices = c("Select colour..." = "", "Select colour..." = "clear", sort(options_Left1)), selected = isolate(input$Left1))
    updateSelectInput(session, "Left2", choices = c("Select colour..." = "", "Select colour..." = "clear", sort(options_Left2)), selected = isolate(input$Left2))
    updateSelectInput(session, "Right1", choices = c("Select colour..." = "", "Select colour..." = "clear", sort(options_Right1)), selected = isolate(input$Right1))
    updateSelectInput(session, "Right2", choices = c("Select colour..." = "", "Select colour..." = "clear", sort(options_Right2)), selected = isolate(input$Right2))
  })
  
}




## Perform search ---------------------------------------

## For now, this return the whole table and narrows it down while they search
## Don't want them to see the whole table
## Search once they've made 2 selections?
## Or have a search button?

# Issues:
## because of the "" vs "clear" issue, if they clear a selection using "Select a colour" (="clear"), it still updates the search
# Need to fix "" vs. "clear" in other findIndividual functions


findIndividualServer_search <- function(input, data, session) {
  
  reactive({
    # Don't return table if no selections are made
    #if (all(input$Left1 == "", input$Left2 == "", input$Right1 == "", input$Right2 == "")) {
      #return(NULL)  # No input = no result
    #}
    
    # Or if only two selections are made?
    # Count how many inputs are filled (i.e. not blank)
    filled_inputs <- sum(input$Left1 != "", input$Left2 != "", input$Right1 != "", input$Right2 != "")
    
    # Only search if at least 2 are filled
    if (filled_inputs < 2) {
      return(NULL)
    }
    
    filtered <- data
    
    if (input$Left1 != "" && input$Left1 != "clear") {
      filtered <- filtered[filtered$ColourRingLeft1 == input$Left1, ]
    }
    if (input$Left2 != "" && input$Left2 != "clear") {
      filtered <- filtered[filtered$ColourRingLeft2 == input$Left2, ]
    }
    if (input$Right1 != "" && input$Right1 != "clear") {
      filtered <- filtered[filtered$ColourRingRight1 == input$Right1, ]
    }
    if (input$Right2 != "" && input$Right2 != "clear") {
      filtered <- filtered[filtered$ColourRingRight2 == input$Right2, ]
    }
    
    return(filtered)
  })
}

  
  




  
  
  
  
  