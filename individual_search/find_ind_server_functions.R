# Vlieland Shiny app
# Server functions to create individual bird lookup

## Need to organise/rename all these

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)
library(DT)
library(tidyverse)



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


# This changes the dropdown options based on selections made 
# Currently, backspace clears the selection but need a clickable "clear" option (or keep the "Select..." option) - added
# Also, when selection is made in one dropbox, the options for that dropbox are restricted to that value (even if only one selection has been made) - so you can't change the option (unless you clear the selection with backspace)
# Now the "Select colour..." text remains and clears the search
# Not sure the other drop downs re-update when a selection is cleared using 'Select a colour' - maybe they are

# Need to sort out "" vs "clear" issue and placeholder


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
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "Left1", selected = "")
    updateSelectInput(session, "Left2", selected = "")
    updateSelectInput(session, "Right1", selected = "")
    updateSelectInput(session, "Right2", selected = "")
  })
  
}




## Perform search ---------------------------------------

## For now, this return the whole table and narrows it down while they search
## Don't want them to see the whole table
## Search once they've made 2 selections?
## Or have a search button? But would be less intuitive that you could leave selections blank and still find the individual

## Or it could provide results once there are fewer than three options?
## This fixes the "" vs "clear" issue as well

# Issues:
## because of the "" vs "clear" issue, if they clear a selection using "Select a colour" (="clear"), it still updates the search (i.e. can show all data rows by setting them all back to 'Select a colour...')
# Need to fix "" vs. "clear" in other findIndividual functions
# Likely needs proper placeholder text?

# Make the table clickable: https://shiny.posit.co/r/components/outputs/table-reactable/
# This might be a better way without the tick boxes: https://stackoverflow.com/questions/69870709/r-shiny-get-data-from-selected-row


# Add action button to select the correct individual
# https://forum.posit.co/t/add-a-button-into-a-row-in-a-datatable/18651/2

findIndividualServer_search <- function(input, output, data, session) {
  
  search_results <- reactive({
    # Don't return table if no selections are made
    #if (all(input$Left1 == "", input$Left2 == "", input$Right1 == "", input$Right2 == "")) {
      #return(NULL)  # No input = no result
    #}
    
    # Or if only one selection is made? Can alter 
    # Count how many selections are made (i.e. not blank but issue of "clear")
    #filled_inputs <- sum(input$Left1 != "", input$Left2 != "", input$Right1 != "", input$Right2 != "")
    
    # Only search if at least 2 are filled
    #if (filled_inputs < 2) {
    #  return(NULL)
    #}
  
    
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
    
    # If going to return results based on number of narrowed options, add an if statement around return here
    if(nrow(filtered) < 5) {
      return(filtered) 
    }
  })
  # This section works to return the search results as a clickable dataframe but I'm changing it to avoid tickboxes
  #output$summary_info <- renderReactable({
  #  df <- search_results()
  #  if (is.null(df) || nrow(df) == 0) return(NULL)
  #  reactable(df[, c(1, 2, 7, 8)],
  #            highlight = TRUE,
  #            bordered = TRUE,
  #            selection = "single", # would like it to be clickable without this selection tick box...
   #           theme = reactableTheme(
  ##            onClick = "select",
  #              rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")))
  #}) 
  #return(search_results)
  
  #selected_row <- reactive({
  #  getReactableState("summary_info", "selected")
  #})
  ####
  
  # Function to create action buttons
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    return(inputs)
  }
  
  
  # This works to produce clickable datatable but need to get rid of search bar etc.              
  #output$summary_info <- renderDataTable(datatable({ search_results() }))
  output$summary_info <- renderDataTable({
    df <- search_results()[, c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
    
    # Add the action button column
    df$Select <- buttonInput(
      FUN = actionButton,
      len = nrow(df),
      id = "select_",
      label = "Select",
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    )
    
    datatable(
      df,
      options = list(dom = 't', ordering = FALSE),
      rownames = FALSE,
      escape = FALSE,  # allow HTML (for buttons)
      selection = "single"
    )
  })
  
  # validation text to check row selection works
  #output$text <- renderText({ toString(search_results()[input$summary_info_rows_selected, "RingNumber"]) })
  
  
  # return reactive expression for use in other functions:
  return(search_results) # clicked row
  
}

  
  




  
  
  
  
  