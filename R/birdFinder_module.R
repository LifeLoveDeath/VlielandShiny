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
birdFinderUI <- function(id, i18n) {
  
  ns <- NS(id)
  
  
  # colour_rings <- get_colour_rings()
  # colour_rings$val <- trimws(colour_rings$val)
  
  div(
    style = "background-color: #f2f4f5; padding: 5px;",
    
    # --- White panel container ----
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
      
      # --- Page title and instructions ----
      fluidRow(
        column(
          width = 12,
          h3("Find an individual", style = "color:#3f5262; font-weight:500;")
        )
      ),
      
      br(),
      
      # Collapsible panel with example birds to search
      # AE...85066 has simple map & family tree
      # B...956635 quite complex & interesting map & family tree
      tags$head(tags$style(HTML(".panel-title > a  {
                          color:#3f5262 !important; font-weight:500;
                          font-size: 16px; line-height: 1.1; }"))),
      tags$script(HTML('
      <script>
      $(document).on("shiny:sessioninitialized", function(event) {
        $("a[id^=\'ui-collapse\']").each(function() {
          $(this).addClass("i18n");
          $(this).attr("data-key", $(this).text());
        }); 
      });
      </script>')),
      bsCollapse(open = "eg_birds",
        bsCollapsePanel(value = "eg_birds",
          title = i18n$t("birdFinder_example_title"),
          div(style = "padding-left: 15px; padding-right: 15px;",
              renderUI("eg_heading")),
          column(width = 3, renderUI("eg_bird1")),
          column(width = 3,
                 style = "text-align: left;",
                 tags$img(src = "Illustrated_BlueTit.png",
                          alt = "Drawing of colour ringed blue tit with leg rings matching the first example bird",
                          style = "max-width: 230px; width: 100%; border-radius: 5px;")),
          column(width = 3, renderUI("eg_bird2")),
          column(width = 3, 
                 style = "text-align: left;",
                 tags$img(src = "Illustrated_BlueTit.png",
                          alt = "Drawing of colour ringed great tit with leg rings matching the second example bird",
                          style = "max-width: 230px; width: 100%; border-radius: 5px;"))
        )),
      
      # --- Search inputs (left) and results (right) ----
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
          # picker input dropdowns for bird filtering
          uiOutput("searchDropdowns"),
          actionButton("reset_filters", "Reset filters", style = "margin-top: 10px;"),
          br(),
          br(),
          
          # Search by RING NUMBER
          h4("Search using ring number", style = "color:#3f5262; font-weight:500;"),
          helpText(HTML("
    • Each metal ring is marked with a unique number<br>
    • This can also be used as an alternative to colour rings to identify your bird")),
          uiOutput("ringNumb_search"),
          actionButton("clear_ring_search", i18n$t("birdFinder_ringSearch_clear")),
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
          tags$style(type = "text/css", "#more_data{color:#01B6DC; text-align: center;}"),
          uiOutput("more_data"),  
          uiOutput("map_preview_ui")
          
        )
      )
    )
  )
}





# Server functions ---------------------------------------------------------

## Single server function 

birdFinderServer <- function(input, output, data, session, i18n) {
  
  # ---- Reactive live language change -------------------------------------
  # ReactiveVal to trigger updates when language selection changes
  Current_lang <- reactiveVal()
  observeEvent(input$lang_select,{
    Current_lang(input$lang_select)
  })
  
  
  # ---- Server side ui generation to handle live translation ---------------
  
  ## ----Example bird collapsible panel ----
  # heading
  output$eg_heading <- renderUI({helpText(HTML(i18n$t("birdFinder_example_heading")))})
  # e.g. bird 1
  output$eg_bird1 <- renderUI({helpText(HTML(i18n$t("birdFinder_example_bird1")))})
  # e.g. bird 2
  output$eg_bird2 <- renderUI({helpText(HTML(i18n$t("birdFinder_example_bird2")))})
  
  ## ----Colour ring dropdowns ----
  output$searchDropdowns <- renderUI({
    
    colour_rings <- get_colour_rings(picker_translate = picker_translate, lang = Current_lang())
    colour_rings$val <- trimws(colour_rings$val)
    
    tagList(
    tags$head(tags$style(HTML("
            .picker-item { display: flex; align-items: center; justify-content: space-between; gap: 10px; }
            .picker-item .text { flex-grow: 1; }
            .picker-item .icon { height: 1em; width: auto; margin-left: 5px;}
          "))),
    pickerInput("Yr", i18n$t("birdFinder_pickerYr_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerYr_filler")), year(Sys.Date()):1955),
                selected = i18n$t("birdFinder_pickerYr_filler"),
                options = list(size = 4.6)),
    pickerInput("Sp", i18n$t("birdFinder_pickerSp_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerSp_filler")), i18n$t("Blue_tit"), i18n$t("Great_tit")),
                selected = i18n$t("birdFinder_pickerSp_filler")),
    pickerInput("Left1", i18n$t("birdFinder_pickerL1_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$val),
                choicesOpt = list(content = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$img)),
                selected = i18n$t("birdFinder_pickerColRing_filler")),
    pickerInput("Left2", i18n$t("birdFinder_pickerL2_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$val),
                choicesOpt = list(content = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$img)),
                selected = i18n$t("birdFinder_pickerColRing_filler")),
    pickerInput("Right1", i18n$t("birdFinder_pickerR1_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$val),
                choicesOpt = list(content = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$img)),
                selected = i18n$t("birdFinder_pickerColRing_filler")),
    pickerInput("Right2", i18n$t("birdFinder_pickerR2_heading"),
                choices = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$val),
                choicesOpt = list(content = c(setNames("", i18n$t("birdFinder_pickerColRing_filler")), colour_rings$img)),
                selected = ("birdFinder_pickerColRing_filler"))
    )
  })
  
  ## ----Metal ring number entry ----
  output$ringNumb_search <- renderUI({
    textInput("ring_search", "",
              placeholder = i18n$t("birdFinder_ringSearch_filler"))
  })
  
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
    
    picker_translate <- picker_translate
    colour_rings <- get_colour_rings(picker_translate = picker_translate, lang = Current_lang())
    colour_rings$val <- trimws(colour_rings$val)
    get_icons <- function(options) colour_rings$img[match(options, colour_rings$val)]
    
    options <- get_dropdown_options(data, input, lang = Current_lang())
    
    # translate dropdown fillers for no selection
    Yr_filler <- setNames("", i18n$t("birdFinder_pickerYr_filler"))
    Sp_filler <- setNames("", i18n$t("birdFinder_pickerSp_filler"))
    ColRing_filler <- setNames("", i18n$t("birdFinder_pickerColRing_filler"))
    
    # below "options" is the list "get_dropdown_options" generated in birdFinder_functions
    # the length of each "options" sub list e.g. options$sp depends on the subsetting applied by selections in the other dropdowns
    updatePickerInput(session, "Yr",
                      choices = c(Yr_filler, sort(options$Yr, decreasing = T)),
                      choicesOpt = list(content = c(Yr_filler, sort(options$Yr, decreasing = T))),
                      selected = isolate(input$Yr))
    updatePickerInput(session, "Sp",
                      choices = c(Sp_filler, sort(options$Sp)),
                      choicesOpt = list(content = c(Sp_filler, sort(options$Sp))),
                      selected = isolate(input$Sp))
    updatePickerInput(session, "Left1", choices = c(ColRing_filler, sort(options$Left1)),
                      choicesOpt = list(content = c(ColRing_filler, get_icons(sort(options$Left1)))),
                      selected = isolate(input$Left1))
    updatePickerInput(session, "Left2", choices = c(ColRing_filler, sort(options$Left2)),
                      choicesOpt = list(content = c(ColRing_filler, get_icons(sort(options$Left2)))),
                      selected = isolate(input$Left2))
    updatePickerInput(session, "Right1", choices = c(ColRing_filler, sort(options$Right1)),
                      choicesOpt = list(content = c(ColRing_filler, get_icons(sort(options$Right1)))),
                      selected = isolate(input$Right1))
    updatePickerInput(session, "Right2", choices = c(ColRing_filler, sort(options$Right2)),
                      choicesOpt = list(content = c(ColRing_filler, get_icons(sort(options$Right2)))),
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
  
  # Clear ring number when a dropdown is selected from:
  observeEvent(list(input$Yr,input$Sp,input$Left1,input$Left2,input$Right1,input$Right2),
               {updateTextInput(session, "ring_search", value = "")})
  
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
    req(
      any(nzchar(c(
        input$Yr, input$Sp, input$Left1,
        input$Left2, input$Right1, input$Right2
      )))
    )
    filter_birds(data, input, lang = Current_lang())
  })
  
  
  
  
  # ---- Selection and results ----
  # Render DT table of results with clickable "See full info" buttons
  # Update selected_ring() reactive when a button is clicked
  # Used by parent UI to show individual bird details
  
  selected_ring <- reactiveVal(NULL)
  selected_colours <- reactiveVal(NULL)
  
  # text shown when too many birds selected to show summary_info
  output$more_data <- renderUI({
    req(is.null(search_results()),
        any(nzchar(c(
          input$Yr, input$Sp, input$Left1,
            input$Left2, input$Right1, input$Right2
        )))
        )
    HTML(paste("Over 200 individuals match your search criteria.<br>
               More information is required to narrow down potential matches."))
  })
  
  option_colours <- reactive({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- df[order(df$Colour_EarliestStart, decreasing = T),]
    df <- df[, c("RingNumber", "ColourRingCombo", "BirthYear", "Species")]
    col_opts <- df$ColourRingCombo
    col_opts
  })
  
  output$summary_info <- DT::renderDataTable({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df[order(df$Colour_EarliestStart, decreasing = T),]
    
    df <- df[, c("RingNumber", "ColourRingLeft1", "ColourRingLeft2",
                 "ColourRingRight1", "ColourRingRight2", "BirthYear", "Species")]
    colnames(df) <- c("RingNumber", "Left1", "Left2", "Right1", "Right2", "BirthYear", "Species")
  
    df$Left1 <- to_current_lang(df$Left1, picker_translate, lang = Current_lang())
    df$Left2 <- to_current_lang(df$Left2, picker_translate, lang = Current_lang())
    df$Right1 <- to_current_lang(df$Right1, picker_translate, lang = Current_lang())
    df$Right2 <- to_current_lang(df$Right2, picker_translate, lang = Current_lang())
    df$Colour_rings <- paste(df$Left1, df$Left2, df$Right1, df$Right2, sep = ", ") %>%
      {gsub("NA, ", "", .)} %>%
      {gsub(", NA", "", .)}
    
    df <- df[, c("RingNumber", "Colour_rings", "BirthYear", "Species")]
    colnames(df) <- c("Ring number", "Colour rings", "Birth year", "Species")
    df$Species <- to_current_lang(df$Species, picker_translate, lang = Current_lang())
    
    df$Select <- make_action_buttons(nrow(df), "select_", label = i18n$t("birdFinder_table_button"))
    
    datatable(df, options = list(dom = 't<"bottom"p>', ordering = FALSE),
              rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = 1))
  })
  
  observeEvent(input$select_button, {
    index <- as.numeric(gsub("select_", "", input$select_button))
    df <- search_results()
    df <- df[order(df$Colour_EarliestStart, decreasing = T),]
    if (!is.null(df) && nrow(df) >= index)
    {selected_ring(df[index, "RingNumber"])
      selected_colours(option_colours()[index])}
  })
  
  return(list(search_results = search_results, selected_ring = selected_ring,
              selected_colours = selected_colours))
}