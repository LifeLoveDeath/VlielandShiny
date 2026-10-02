# ==========================================================
# birdFinder Module
# ==========================================================
# Allows searching for birds by ring number or colour rings.
# Returns:
#   search_results: reactive filtered data table
#   selected_ring: reactive value of currently selected bird
# ----------------------------------------------------------

# Load packages - moved all to app.r
# library(shiny)
# library(leaflet)
# library(bslib)
# library(viridis)
# library(dplyr)
# library(shinyWidgets)
# library(reactable)
# library(DT)
# library(tidyverse)



# UI function --------------------------------------------------------------
birdFinderUI <- function(id) {
  
  ns <- NS(id)
  
  
  colour_rings <- get_colour_rings()
  colour_rings$val <- trimws(colour_rings$val)
  
  div(
    style = "background-color: #f2f4f5; padding: 5px;",
    
    # --- White panel container ---
    div(
      style = "
        background-color: #ffffff;
          max-width: 90vw; 
          min-height: 800px;
          margin: 0 auto;
          padding: 30px 40px;
          border-radius: 8px;
          box-shadow: 0 0 12px rgba(0,0,0,0.08);
      ",
      
      # --- Page title and instructions ---
      fluidRow(
        column(
          width = 12,
          h3("Find an individual", style = "color:#3f5262; font-weight:500;")
        )
      ),
      
      br(),
      
      # --- Search inputs (left) and results (right) ---
      fluidRow(
        # Left column: search inputs
        column(
          width = 4,
          
          # Search by COLOUR RINGS
          h4("Search using colour rings", style = "color:#3f5262; font-weight:500;"),
          helpText(HTML("
    • Select ring colours <b>top to bottom</b> on each leg<br>
    • Each bird has <b>one metal ring</b><br>
    • You can leave a dropdown blank if a ring is <b>unknown</b><br>
    • See bottom of page for an example and diagram")),
          
          tags$head(tags$style(HTML("
            .picker-item { display: flex; align-items: center; justify-content: space-between; gap: 10px; }
            .picker-item .text { flex-grow: 1; }
            .picker-item .icon { height: 1em; width: auto; margin-left: 5px;}
          "))),
          pickerInput("Yr", "Year bird seen",
                      choices = c("Select year..." = "", year(Sys.Date()):1955),
                      selected = "Select year...",
                      options = list(size = 4.6)),
          pickerInput("Sp", "Species",
                      choices = c("Select species..." = "", "Blue tit", "Great tit"),
                      selected = "Select species..."),
          pickerInput("Left1", "Left leg - top ring",
                      choices = c("", colour_rings$val),
                      choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                      selected = "Select colour..."),
          pickerInput("Left2", "Left leg - bottom ring",
                      choices = c("", colour_rings$val),
                      choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                      selected = "Select colour..."),
          pickerInput("Right1", "Right leg - top ring",
                      choices = c("", colour_rings$val),
                      choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                      selected = "Select colour..."),
          pickerInput("Right2", "Right leg - bottom ring",
                      choices = c("", colour_rings$val),
                      choicesOpt = list(content = c("Select colour...", colour_rings$img)),
                      selected = "Select colour..."),
          actionButton("reset_filters", "Reset filters", style = "margin-top: 10px;"),
          br(),
          br(),
          
          # Search by RING NUMBER
          h4("Search using ring number", style = "color:#3f5262; font-weight:500;"),
          helpText(HTML("
    • Each metal ring is marked with a unique number<br>
    • This can also be used as an alternative to colour rings to identify your bird")),
          textInput("ring_search", "",
                    placeholder = "Enter ring number"),
          actionButton("clear_ring_search", "Clear ring number"),
          br(),
          br(),
          
          # Example search text
          # BK...65998 has simple map & family tree
          # B...956635 quite complex & interesting map & family tree
          h4("Example birds", style = "color:#3f5262; font-weight:500;"),
          helpText(HTML("<b>Try searching for the following birds:</b><br><br>
                          
                          Species: <b>Great tit</b><br>
                          Left leg - top ring: <b>yellow/black</b><br>
                          Left leg - bottom ring: <b>yellow</b><br>
                          Right leg - top ring: <b>yellow/black</b><br>
                          Right leg - bottom ring: <b>metal</b><br>
                          <i>Ring number: BK...65998</i><br><br>
                          
                          Species: <b>Great tit</b><br>
                          Left leg - top ring: <b>blue</b><br>
                          Left leg - bottom ring: <b>white</b><br>
                          Right leg - top ring: <b>metal</b><br>
                          Right leg - bottom ring: <b>pink/green</b><br>
                          <i>Ring number: B...956635</i><br><br>
                          
                        ")),
          # Diagram placeholder
          helpText(HTML("<i>(Placeholder: Diagram showing order of rings on bird)</i>"))
        ),
        
        # Right column: table and map
        column(
          width = 8,
          h4("Matching individuals", style = "color:#3f5262; font-weight:500;"),
          helpText(HTML("• Search results will appear here once you select colour rings.<br>
          • The displayed birds are <b>sorted by date born</b>. Birds born more recently are more likely to be your bird.<br>
          • <b>Click on an individual</b> to see its <b>last observed location</b> on the map.<br>
          • <b>Click “See full info”</b> to explore its <b>full details</b>, a map of its nesting sites and its family tree.")),
            DT::dataTableOutput("summary_info"),
            br(),
          tags$style(type = "text/css", "#more_data{color:#01B6DC; text-align: center;}"), #8AD5E6
          uiOutput("more_data"),  
          uiOutput("map_preview_ui")
          
        )
      )
    )
  )
}





# Server functions ---------------------------------------------------------

## Single server function 

birdFinderServer <- function(input, output, data, session) {
  
  # ---- Uncaps species names in data ----
  data$Species <- str_to_sentence(data$Species)
  
  
  # ---- Cap dates bird could be sighted to sys.date ----
  data$Colour_LatestLikely <- ifelse(is.na(data$Colour_LatestLikely),
                                     yes = NA,
                                     no = ifelse(data$Colour_LatestLikely > year(Sys.Date()),
                                                 yes = year(Sys.Date()),
                                                 no = data$Colour_LatestLikely))
  
  data$Colour_EarliestStart <- ifelse(is.na(data$Colour_EarliestStart),
                                     yes = NA,
                                     no = ifelse(data$Colour_EarliestStart > year(Sys.Date()),
                                                 yes = year(Sys.Date()),
                                                 no = data$Colour_EarliestStart))
  
  
  
  # ---- Dropdown narrowing ----
  # Whenever any colour ring input changes, update available options for the other dropdowns
  # Excludes current dropdown to prevent circular filtering
  # Uses get_dropdown_options() from birdFunder_functions
  
  # Reactive observation - triggers output whenever one of the inputs changes
  observe({
    if (!is.null(input$ring_search) && input$ring_search != "") return()
    
    colour_rings <- get_colour_rings()
    colour_rings$val <- trimws(colour_rings$val)
    get_icons <- function(options) colour_rings$img[match(options, colour_rings$val)]
    
    options <- get_dropdown_options(data, input)
    
    
    # below "options" is the list "get_dropdown_options" generated in birdFinder_functions
    # the length of each "options" sub list e.g. options$sp depends on the subsetting applied by selections in the other dropdowns
    updatePickerInput(session, "Yr", choices = c("", sort(options$Yr, decreasing = T)),
                      choicesOpt = list(content = c("Select year...", sort(options$Yr, decreasing = T))),
                      selected = isolate(input$Yr))
    updatePickerInput(session, "Sp", choices = c("", sort(options$Sp)),
                      choicesOpt = list(content = c("Select species...", sort(options$Sp))),
                      selected = isolate(input$Sp))
    updatePickerInput(session, "Left1", choices = c("", sort(options$Left1)),
                      choicesOpt = list(content = c("Select colour...", get_icons(sort(options$Left1)))),
                      selected = isolate(input$Left1))
    updatePickerInput(session, "Left2", choices = c("", sort(options$Left2)),
                      choicesOpt = list(content = c("Select colour...", get_icons(sort(options$Left2)))),
                      selected = isolate(input$Left2))
    updatePickerInput(session, "Right1", choices = c("", sort(options$Right1)),
                      choicesOpt = list(content = c("Select colour...", get_icons(sort(options$Right1)))),
                      selected = isolate(input$Right1))
    updatePickerInput(session, "Right2", choices = c("", sort(options$Right2)),
                      choicesOpt = list(content = c("Select colour...", get_icons(sort(options$Right2)))),
                      selected = isolate(input$Right2))
  })
  
  # reset all to blank if button pressed
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "Yr", selected = "")
    updatePickerInput(session, "Sp", selected = "")
    updatePickerInput(session, "Left1", selected = "")
    updatePickerInput(session, "Left2", selected = "")
    updatePickerInput(session, "Right1", selected = "")
    updatePickerInput(session, "Right2", selected = "")
  })
  
  
  # ---- Clear selection ----
  # Clears ring number search or resets colour ring dropdowns when relevant buttons are clicked
  
  # Clear ring number search on clear button click
  observeEvent(input$clear_ring_search, { updateTextInput(session, "ring_search", value = "") })
  
  # Clear dropdowns when a ring number is entered:
  observeEvent(input$ring_search, {
    if (!is.null(input$ring_search) && input$ring_search != "") {
      updatePickerInput(session, "Yr", selected = "")
      updatePickerInput(session, "Sp", selected = "")
      updatePickerInput(session, "Left1", selected = "")
      updatePickerInput(session, "Left2", selected = "")
      updatePickerInput(session, "Right1", selected = "")
      updatePickerInput(session, "Right2", selected = "")
    }
  })
  
  
  # ---- Search logic ----
  # First filter by ring number if entered (ignores colour ring filters)
  # Then filter by colour rings only if no ring number entered
  # Only returns results if <5 rows to avoid cluttering UI
  # Uses filter_birds() from functions
  
  # Reactive filtered search results, returns results when fewer than 5 rows
  search_results <- reactive({
    filter_birds(data, input)
    })
  
  
  
  
  # ---- Selection and results ----
  # Render DT table of results with clickable "See full info" buttons
  # Update selected_ring() reactive when a button is clicked
  # Used by parent UI to show individual bird details
  
  selected_ring <- reactiveVal(NULL)
  selected_colours <- reactiveVal(NULL)
  
  # text shown when too any birds selected to show summary_info
  output$more_data <- renderUI({
    req(is.null(search_results()))
    HTML(paste("Over 200 individuals match your search criteria.<br>
               More information is required to narrow down potential matches."))
  })
  
  output$summary_info <- DT::renderDataTable({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df[order(df$Colour_EarliestStart, decreasing = T),]
    
    df <- df[, c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
    df$ColourRingCombo <- gsub("-", ", ", df$ColourRingCombo)
    colnames(df) <- c("Ring number", "Colour rings", "Birth year", "Species")
    
    df$Select <- make_action_buttons(nrow(df), "select_")
    
    datatable(df, options = list(dom = 't<"bottom"p>', ordering = FALSE),
              rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = 1))
  })
  
  observeEvent(input$select_button, {
    row_index <- as.numeric(gsub("select_", "", input$select_button))
    df <- search_results()
    df <- df[order(df$Colour_EarliestStart, decreasing = T),]
    if (!is.null(df) && nrow(df) >= row_index)
      {selected_ring(df[row_index, "RingNumber"])
      selected_colours(gsub(", ","-",df[row_index, "ColourRingCombo"]))}
  })
  
  return(list(search_results = search_results, selected_ring = selected_ring,
              selected_colours = selected_colours))
}