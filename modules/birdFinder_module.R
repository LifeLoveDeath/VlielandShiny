
# birdFinder module

# Need to amalgamate server functions
# Could still split UI and server functions into separate scripts within a birdFinder_module/ folder, then just source the folder in app.R?

# Load packages 
library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(shinyWidgets)
library(reactable)
library(DT)
library(tidyverse)


# UI function --------------------------------------------------------------

# Old name: findIndividualUI_withIcons
birdFinderUI <- function(data) {
  
  colour_rings <- get_colour_rings()
  
  tagList(
    # Old drop down
    #selectizeInput("Left1", "Left leg - top ring", 
    #               choices =  c("", colours), selected = ""
    #              ),
    
    #Formatting for icons - would be better if they were aligned to the far right?
    tags$head(tags$style(HTML("
  .picker-item {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
  }

  .picker-item .text {
    flex-grow: 1;
  }

  .picker-item .icon {
    height: 1em; /* Match the text height */
    width: auto;
    margin-left: 5px;
  }
"))),
    
    # new dropdown format - icons work but search doesn't work anymore (and issues with starting selection/placeholder text)
    # Search actually working ok but option in the dropdown containing icons is not narrowing down
    # Also, for the other format, the "Select colour..." came from somewhere else so this might interfere with the search functions
    
    pickerInput(inputId = "Left1",
                label = "Left leg - top ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour..."),
    
    pickerInput(inputId = "Left2",
                label = "Left leg - bottom ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour..."),
    
    pickerInput(inputId = "Right1",
                label = "Right leg - top ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour..."),
    
    pickerInput(inputId = "Right2",
                label = "Right leg - bottom ring",
                choices = c("", colour_rings$val),
                choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                selected = "Select colour..."),
    
    # Old dropdown format
    #selectizeInput(
    #  "Left2", "Left leg - bottom ring", 
    #  choices =  c("", colours), selected = ""
    
    #),
    
    #selectizeInput(
    #  "Right1", "Right leg - top ring", choices =  c("", colours), selected = ""
    #),
    
    #selectizeInput(
    #  "Right2", "Right leg - bottom ring", choices =  c("", colours), selected = ""
    #),
    
    actionButton("reset_filters", "Reset filters"),
    helpText(HTML("placeholder instructions text")))
}




# Server functions ---------------------------------------------------------

## Single server function 

birdFinderServer <- function(input, output, data, session) {
  
  # --- Dropdown narrowing ---
  # Reactive observation - triggers output whenever one of the inputs changes
  observe({
    
    # Functions to get colour ring options and matching icons
    colour_rings <- get_colour_rings()
    get_icons <- function(options) {
      row <- match(options, colour_rings$val)
      colour_rings$img[row]
    }
    
    # Filter options for each dropdown based on other selections (excluding it's own selection)
    options_Left1 <- unique(data[
      (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft1"
    ])
    
    options_Left2 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft2"
    ])
    
    options_Right1 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingRight1"
    ])
    
    options_Right2 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1),
      "ColourRingRight2"
    ])
    
    
    # Update the dropdowns (but keep current selection)
    updatePickerInput(session, "Left1", 
                      choices = c("", sort(options_Left1)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Left1)))),
                      selected = isolate(input$Left1))
    
    updatePickerInput(session, "Left2",
                      choices = c("", sort(options_Left2)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Left2)))),
                      selected = isolate(input$Left2))
    
    updatePickerInput(session, "Right1", 
                      choices = c("", sort(options_Right1)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Right1)))),
                      selected = isolate(input$Right1))
    
    updatePickerInput(session, "Right2", 
                      choices = c("", sort(options_Right2)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Right2)))),
                      selected = isolate(input$Right2))
  })
  
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "Left1", selected = "")
    updatePickerInput(session, "Left2", selected = "")
    updatePickerInput(session, "Right1", selected = "")
    updatePickerInput(session, "Right2", selected = "")
  })
  
  
  
  
  # --- Search logic ---
  
  # Reactive filtered search results, returns results when fewer than 5 rows
  search_results <- reactive({
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
    
    if (nrow(filtered) < 5) {
      return(filtered)
    } else {
      return(NULL)  # No results or too many results: return NULL
    }
  })
  
  # Function to generate action buttons to add to rows
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  # Render datatable with clickable rows and "Select individual" buttons
  # Clickable rows + action button might be an awkward combo
  output$summary_info <- DT::renderDataTable({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df[, c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
    
    # Add action buttons column to datatable
    df$Select <- buttonInput(
      FUN = shiny::actionButton,
      len = nrow(df),
      id = "select_",
      label = "Select individual",
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    )
    
    datatable(
      df,
      options = list(dom = 't', ordering = FALSE),
      rownames = FALSE,
      escape = FALSE,  # allow HTML for buttons
      selection = "single"
    )
  })
  
  # ReactiveVal to store selected bird RingNumber - clicking row
  selected_ring <- reactiveVal(NULL)
  
  # Update selected_ring when "Select individual" button is clicked
  observeEvent(input$select_button, {
    row_index <- as.numeric(gsub("select_", "", input$select_button))
    df <- search_results()
    if (!is.null(df) && nrow(df) >= row_index) {
      selected_ring(df[row_index, "RingNumber"])
    }
  })
  
  # Reactive to tell UI whether a bird is selected - changes conditional tabs
  output$birdSelected <- reactive({
    !is.null(selected_ring())
  })
  outputOptions(output, "birdSelected", suspendWhenHidden = FALSE)
  
  # Back to search button
  observeEvent(input$back_to_search, {
    selected_ring(NULL)
  })
  
  # Content for the bird detail tabs
  output$bird_general <- renderPrint({
    req(selected_ring())
    # Fetch and display general info for selected_ring()
    paste("General info for bird:", selected_ring())
  })
  
  output$bird_map <- leaflet::renderLeaflet({
    req(selected_ring())
    uiOutput("map_ind_ui") # don't need this because it's elsewhere?
    
  })
  
  output$bird_pedigree <- renderPlot({
    req(selected_ring())
    
  })
  
  # Return reactive expression (row number of clicked row) for use in other functions (map):
  #return(search_results)
  
  # Return select ring for use in other functions:
  return(list(search_results = search_results, selected_ring = selected_ring))
  
}






## Old:: Narrows dropdown options as selections made -----------------------------


#Old name: findIndividualServer_updateDropdowns
birdFinderDropdownsServer <- function(input, data, session) {
  
  # Reactive observation - triggers output whenever one of the inputs changes
  observe({
    
    # Functions to get colour ring options and matching icons
    colour_rings <- get_colour_rings()
    get_icons <- function(options) {
      row <- match(options, colour_rings$val)
      colour_rings$img[row]
    }
    
    # Filter options for each dropdown based on other selections (excluding it's own selection)
    options_Left1 <- unique(data[
      (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft1"
    ])
    
    options_Left2 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft2"
    ])
    
    options_Right1 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingRight1"
    ])
    
    options_Right2 <- unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1),
      "ColourRingRight2"
    ])
    
    
    # Update the dropdowns (but keep current selection)
    updatePickerInput(session, "Left1", 
                      choices = c("", sort(options_Left1)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Left1)))),
                      selected = isolate(input$Left1))
    
    updatePickerInput(session, "Left2",
                      choices = c("", sort(options_Left2)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Left2)))),
                      selected = isolate(input$Left2))
    
    updatePickerInput(session, "Right1", 
                      choices = c("", sort(options_Right1)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Right1)))),
                      selected = isolate(input$Right1))
    
    updatePickerInput(session, "Right2", 
                      choices = c("", sort(options_Right2)),
                      choicesOpt = list(content = c("Select a colour...", get_icons(sort(options_Right2)))),
                      selected = isolate(input$Right2))
    })
  
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "Left1", selected = "")
    updatePickerInput(session, "Left2", selected = "")
    updatePickerInput(session, "Right1", selected = "")
    updatePickerInput(session, "Right2", selected = "")
  })
}
  
    
   



## Old:: Perform search -----------------------------------------------------------

# New function based on conditional tabs UI
# Switches tabset when bird is selected

# Old name: findIndividualServer_search
birdFinderSearchServer <- function(input, output, data, session) {
  
  # Reactive filtered search results, returns results when fewer than 5 rows
  search_results <- reactive({
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
    
    if (nrow(filtered) < 5) {
      return(filtered)
    } else {
      return(NULL)  # No results or too many results: return NULL
    }
  })
  
  # Function to generate action buttons to add to rows
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  # Render datatable with clickable rows and "Select individual" buttons
  # Clickable rows + action button might be an awkward combo
  output$summary_info <- DT::renderDataTable({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df[, c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
    
    # Add action buttons column to datatable
    df$Select <- buttonInput(
      FUN = shiny::actionButton,
      len = nrow(df),
      id = "select_",
      label = "Select individual",
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    )
    
    datatable(
      df,
      options = list(dom = 't', ordering = FALSE),
      rownames = FALSE,
      escape = FALSE,  # allow HTML for buttons
      selection = "single"
    )
  })
  
  # ReactiveVal to store selected bird RingNumber - clicking row
  selected_ring <- reactiveVal(NULL)
  
  # Update selected_ring when "Select individual" button is clicked
  observeEvent(input$select_button, {
    row_index <- as.numeric(gsub("select_", "", input$select_button))
    df <- search_results()
    if (!is.null(df) && nrow(df) >= row_index) {
      selected_ring(df[row_index, "RingNumber"])
    }
  })
  
  # Reactive to tell UI whether a bird is selected - changes conditional tabs
  output$birdSelected <- reactive({
    !is.null(selected_ring())
  })
  outputOptions(output, "birdSelected", suspendWhenHidden = FALSE)
  
  # Back to search button
  observeEvent(input$back_to_search, {
    selected_ring(NULL)
  })
  
  # Content for the bird detail tabs
  output$bird_general <- renderPrint({
    req(selected_ring())
    # Fetch and display general info for selected_ring()
    paste("General info for bird:", selected_ring())
  })
  
  output$bird_map <- leaflet::renderLeaflet({
    req(selected_ring())
    uiOutput("map_ui")  # needs updating
    
  })
  
  output$bird_pedigree <- renderPlot({
    req(selected_ring())
    
  })
  
  # Return reactive expression (row number of clicked row) for use in other functions (map):
  return(search_results)
  
  
}








