
# ----------------------------------------------------------------------
# Family Tree Module
# ----------------------------------------------------------------------
#   Provides selected bird's family tree using pedigree data.
#   Shows parents, siblings (full and half) & children.
#   Colour by clutch and highlights/fades out recruits if desired.
#
# Inputs:
#   - ped.data: Dataframe containing bird pedigree (RingNumber, Mother, Father, BroodID, Sex, etc.)
#   - brood.data: Dataframe with breeding records, used to identify recruits
#   - selected_ring: Reactive value of currently selected bird RingNumber


# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/


# Load packages - moved to app.r
# library(shiny)
# library(ggpedigree)
# library(ggplot2)
# library(tidyverse)
# library(kinship2)
# library(plotly)
# library(RColorBrewer)



# UI -----------------------------------------
familyTreeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        br(),
        h4("Family tree", style = "color:#3f5262; font-weight:500;")
      )),
    
    fluidRow(
      column(
        width = 4,
        
        # Instructions text:
        helpText(HTML(#"<b>Explore the selected bird's family tree.</b><br>
        "Use the checkboxes to ....")),
        
        br(),
        
       # Add any controls here:
       # Checkboxes to show/hide recruit and half siblings
       fluidRow(
         column(
           width = 12,
           checkboxInput(ns("show_half_sibs"), "Show half siblings", value = FALSE),
           checkboxInput(ns("show_recruits"), "Highlight individuals observed breeding", value = FALSE) # Or at least they are ringed (known birds) who survived to adulthood and at least attempted to breed
         )
       ),
       
        # Placeholder further info text:
        br(),
        helpText(HTML("Placeholder further info text.<br>
                    E.g. General info or guide to interpretation of family tree"))
        
      ),
      column(
        width = 8,
        uiOutput(ns("tree_ui"))  # output for the pedigree
        
      )
    )
    
  
  )
}




# Server --------------------------------------
# Server function
familyTreeServer <- function(id, ped.data, brood.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: subset family tree for selected bird
    family_data <- reactive({
      req(selected_ring())
      get_family_subset(ped.data, selected_ring(), brood.data)
    })
    
    
    # Render pedigree UI
    output$tree_ui <- renderUI({
      req(family_data())
      plotlyOutput(ns("pedigree_plot"), height = "600px")
    })
    
    
    # Render interactive pedigree plot
    output$pedigree_plot <- renderPlotly({
      fam <- family_data() %>%
        filter_family(input$show_half_sibs) %>%
        add_plot_attributes(show_recruits = input$show_recruits)
      
      
    
      
      # Base ggPedigreeInteractive plot
      p <- ggPedigreeInteractive(
        fam,
        personID = "RingNumber",
        dadID = "Father",
        momID = "Mother",
        config = list(
          sex_color_include = FALSE,  # we want clutch colours, not sex colours
          point_size = 3,
          segment_linewidth = 0.5,
          #label_text_size = 3,
          #label_nudge_y = 0.25,
          #label_nudge_x = .1,
          #label_text_angle = -30,
          #label_include = FALSE,
          return_static = TRUE
        ),
        tooltip_columns = c("RNText", "BirthYear", "RingYear", "BroodID", "Sex_text")
      )
      
      
      # Get node data to overlay shapes etc. using ggplot
      nodes <- p$data %>%
        left_join(fam %>% select(RingNumber),
                  by = "RingNumber")
      
      # drop NAs
      nodes <- nodes %>%
        filter(sex %in% c(1, 2, 3)) %>%     # keep only valid shapes
        mutate(sex = as.character(sex))     # ensure matching to scale names
      nodes$Sex <- NULL
       
      # Remove legend
      p <- p +
        guides(shape = "none", fill = "none", colour = "none")
      
      # Separate out nodes
      focal_id <- selected_ring()
      focal_node    <- nodes %>% filter(RingNumber == focal_id)
      
      # Build plot with dynamic alpha and fill
      a <- p +
        geom_point(
          data = nodes %>% filter(sex %in% c(1, 2, 3)),
          aes(
            x = x_pos,
            y = y_pos,
            shape = factor(sex),
            text = tooltip_text,
            fill = node_fill,
            colour = border_col,
            alpha = node_alpha
          ),
          stroke = 0.3,
          size = 6
        ) +
        scale_colour_identity() +
        scale_alpha_identity() +
        geom_point(
          data = focal_node %>% filter(sex %in% c(1,2,3)),
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          fill = focal_node$node_fill,
          colour = "black",
          size = 8,
          stroke = 1,
          alpha = focal_node$node_alpha
        ) +
        scale_shape_manual(
          name = "sex",
          values = c("2" = 21, "1" = 22, "3" = 23),
          labels = c("Female", "Male", "Unknown")
        ) +
        guides(fill = "none", colour = "none", size = "none") + 
        scale_fill_identity()
      
      
      # Render interactive plot
      #a <- a + scale_fill_identity()
      ggplotly(a, tooltip = "text") %>%
        #style(showlegend = FALSE) %>%
        layout(dragmode = "pan") %>%   # sets default click-drag to pan
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "lasso2d",
            "select2d",
            "zoom2d",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines",
            "sendDataToCloud",
            "toImage"
          )
        )
      
    })
    
  })
}
