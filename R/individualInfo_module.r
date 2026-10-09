
# ----------------------------------------------------------------------
# Individual Info Table Module
# ----------------------------------------------------------------------
#   Provides table summarizing individual information for selected bird
#
# Inputs:
#   - vlieland.data: Dataframe containing individual bird data
#   - selected_ring: Reactive value for currently selected bird RingNumber

# Load packages - moved to app.r
# library(shiny)
# library(DT)




# UI -----------------------------------------
individualInfoUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    
    # Page title
    fluidRow(
      column(
        width = 12,
        br(),
        h4(i18n$t("bird_profile"), style = "color:#3f5262; font-weight:500;"),
        p(HTML("The table below summarizes key information about the bird you have seen, 
        including ring information, early life events and breeding attempts as an adult.<br>
        <em>Hover over any term to learn more.</em>"))
      )
    ),
    
    # Formatting for table:
    tags$style(HTML("
      .dt-header-row {
        background-color: #e8eef3 !important;
        font-weight: bold !important;
        text-transform: uppercase;
        color: #3f5262 !important;
      }
    ")),
    
    # Formatting for tooltips:
    tags$style(HTML("
  [title] {
    position: relative;
  }
  [title]:hover::after {
    content: attr(title);
    position: absolute;
    background: #3f5262;
    color: white;
    padding: 6px 10px;
    border-radius: 6px;
    bottom: 100%;
    left: 50%;
    white-space: nowrap;
    z-index: 1000;
    font-size: 0.85em;
  }
")),
    
    #Table output
    uiOutput(ns("indInfoTab_ui"))
  )
}




# Server --------------------------------------

individualInfoServer <- function(id, vlieland.data, selected_ring, selected_colours) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    individual.data <- reactive({
      req(selected_ring())
      
      if (!is.na(selected_colours()))
        {df <- vlieland.data[vlieland.data$RingNumber == selected_ring() &
          vlieland.data$ColourRingCombo == selected_colours(),]}
      else {df <- vlieland.data[vlieland.data$RingNumber == selected_ring(),]}
      
      prepare_individual_data(df)  # <-- call helper
    })
    
    output$indInfoTab_ui <- renderUI({
      renderDT({
        datatable(
          individual.data(),
          escape = FALSE,
          rownames = FALSE,
          colnames = NULL,
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            createdRow = JS(
              "function(row, data, dataIndex) {",
              "  if(dataIndex === 0 || data[1] === '') {",
              "    $(row).addClass('dt-header-row');",
              "  }",
              "}"
            )
          )
        )
      })
    })
    
  })
}