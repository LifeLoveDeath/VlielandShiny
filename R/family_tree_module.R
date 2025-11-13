
# Family tree module

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

# Old dummy data
#vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
#location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

# Real data
#IndividualDataVlieland <- read.csv('data/IndividualDataVlieland.csv', row.names = NULL)
#location.data <- read.csv("data/location_data.csv", row.names = NULL) 
#IndividualInfo <- read.csv('data/IndividualInfo.csv', row.names = NULL)




# Function to get focal individual family tree data ---------------

get_family_subset <- function(ped.data, focal_id, brood_data) {

  # Step 1: Find parents

  focal_row <- ped.data %>% filter(RingNumber == focal_id)
  focal <- ped.data %>% filter(RingNumber == focal_id)
  brood <- focal_row$BroodID
  mother <- focal_row$Mother
  father <- focal_row$Father

  
  # Step 2: Find children

  children <- ped.data %>% filter(Mother == focal_id | Father == focal_id) %>% pull(RingNumber)
  

  # Step 3: Find siblings

  # All siblings - share at least one parent = too many
  siblings <- ped.data %>%
    filter(
      ( !is.na(Mother) & Mother %in% mother ) |
        ( !is.na(Father) & Father %in% father )
    ) %>%
    pull(RingNumber)
  
  # Full siblings only (same BroodID)
  #  siblings <- ped.data %>% 
  #    filter(BroodID == brood & RingNumber != focal_id)
  # siblings <- siblings$RingNumber
  

  # Step 4: Combine IDs and subset

  ids <- unique(c(focal_id, mother, father, children, siblings))
  fam.data <- ped.data %>% filter(RingNumber %in% ids)

  
  # Step 5: Add missing parents as unique placeholders

  all_parents <- unique(c(fam.data$Mother, fam.data$Father))
  missing_parents <- setdiff(all_parents, fam.data$RingNumber)
  missing_parents <- missing_parents[!is.na(missing_parents)]
  
  if(length(missing_parents) > 0){
    # Determine Sex of each missing parent
    sex_vals <- sapply(missing_parents, function(id){
      if(id %in% fam.data$Father) return(2)    # male
      if(id %in% fam.data$Mother) return(1)    # female
      return(0)                                # unknown (should not occur)
    })
    
    # Build missing parent rows
    missing_rows <- data.frame(
      RingNumber        = missing_parents,
      Mother            = NA_character_,
      Father            = NA_character_,
      BroodID           = NA_integer_,
      Sex               = as.integer(sex_vals),
      RingYear          = NA_integer_,
      BirthYear         = NA_integer_,
      RingPopulationName= NA_character_,
      RingNestBox       = NA_character_,
      RingLatitude      = NA_real_,
      RingLongitude     = NA_real_,
      Species           = NA_character_,
      RingColour        = NA_character_,
      ColourRingLeft1   = NA_character_,
      ColourRingLeft2   = NA_character_,
      ColourRingRight1  = NA_character_,
      ColourRingRight2  = NA_character_,
      ColourRingCombo   = NA_character_,
      stringsAsFactors  = FALSE
    )
    
    fam.data <- bind_rows(fam.data, missing_rows)
  }
  

  # Step 6: Fill any remaining NA parents with unique IDs

  # Step 6: Fill NA parents with unique per-brood IDs
  fam.data <- fam.data %>%
    group_by(BroodID) %>%
    mutate(
      Mother = ifelse(is.na(Mother) & !is.na(BroodID), paste0("UnknownMother_", BroodID), Mother),
      Father = ifelse(is.na(Father) & !is.na(BroodID), paste0("UnknownFather_", BroodID), Father)
    ) %>%
    ungroup()
  
  # Add rows for these unknown parents (one per unique placeholder)
  missing_mothers <- setdiff(unique(fam.data$Mother[grepl("^UnknownMother_", fam.data$Mother)]), fam.data$RingNumber)
  missing_fathers <- setdiff(unique(fam.data$Father[grepl("^UnknownFather_", fam.data$Father)]), fam.data$RingNumber)
  
  if(length(missing_mothers) > 0){
    fam.data <- bind_rows(fam.data,
                          data.frame(
                            RingNumber = missing_mothers,
                            Mother     = NA_character_,
                            Father     = NA_character_,
                            BroodID    = NA_integer_,
                            Sex        = 1L,  # female
                            stringsAsFactors = FALSE
                          ))
  }
  
  if(length(missing_fathers) > 0){
    fam.data <- bind_rows(fam.data,
                          data.frame(
                            RingNumber = missing_fathers,
                            Mother     = NA_character_,
                            Father     = NA_character_,
                            BroodID    = NA_integer_,
                            Sex        = 2L,  # male
                            stringsAsFactors = FALSE
                          ))
  }
  

  # Step 7: Add kinship2-compatible sex columns

  fam.data <- fam.data %>%
    mutate(
      # Original Sex: 1=female, 2=male, 0=unknown
      sex = case_when(
        Sex == 2 ~ 1L,      # male -> 1
        Sex == 1 ~ 2L,      # female -> 2
        TRUE     ~ 3L        # unknown -> 3
      ),
      sex_text = case_when(
        Sex == 2 ~ "male",
        Sex == 1 ~ "female",
        TRUE     ~ "unknown"
      ),
      focal = RingNumber == focal_id
    )
  
  
  # Step 8: Are they a recruit?

  fam.data <- fam.data %>%
    mutate(
      is_recruit = RingNumber %in% unique(c(
        ped.data$Mother,
        ped.data$Father,
        brood_data$RingNumberFemale,
        brood_data$RingNumberMale
      ))
    )
  
  
  # Step 9: are they are half or full sibling?
  
  fam.data <- fam.data %>%
    mutate(
      sib_type = case_when(
        RingNumber == focal_id ~ NA_character_,  # focal itself
        !is.na(BroodID) & BroodID == focal_row$BroodID & RingNumber != focal_id ~ "full_sib",
        ( (!is.na(Mother) & Mother == focal_row$Mother & Father != focal_row$Father & !is.na(Father)) |
            (!is.na(Father) & Father == focal_row$Father & Mother != focal_row$Mother & !is.na(Mother)) ) ~ "half_sib",
        TRUE ~ NA_character_
      )
    )


  # Return cleaned family dataset
  fam.data
  
}


# Test
#family_data <- get_family_subset(ped.data, "F...999544", BroodData) # no half sibs
#family_data <- get_family_subset(IndividualDataVlieland, "AH...68076", BroodData) # this one has half sibs
#focal_id <- "AH...68076"



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
        helpText(HTML("<b>Explore the selected bird's family tree.</b><br>
        • Use the checkboxes to ....<br>
        • Show recruits.....<br>")),
        
        br(),
        
       # Add any controls here:
       # Checkboxes to show/hide recruit and half siblings
       fluidRow(
         column(
           width = 12,
           checkboxInput(ns("show_half_sibs"), "Show half siblings", value = TRUE),
           checkboxInput(ns("show_recruits"), "Colour recruits only", value = FALSE)
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
    
    # Reactive: subset family tree data for focal bird
    family_data <- reactive({
      req(selected_ring())
      get_family_subset(ped.data, selected_ring(), brood.data)
    })
    
    
    # Render the pedigree plot
    output$tree_ui <- renderUI({
      req(family_data())
      plotlyOutput(ns("pedigree_plot"), height = "600px")
    })
    
    output$pedigree_plot <- renderPlotly({
      fam <- family_data()
      
      # Apply filters from checkboxes
      # Siblings
      fam <- fam %>%
        filter(
          # Half siblings filter
          (input$show_half_sibs | is.na(sib_type) | sib_type == "full_sib")
        )
      
      
      # Define palette first
      brood_levels <- sort(unique(fam$BroodID))
      pal <- brewer.pal(n = max(3, length(brood_levels)), "Paired")[1:length(brood_levels)]
      names(pal) <- brood_levels
      
      # Assign alpha and fill based on checkbox + recruit status
      fam <- fam %>%
        mutate(
          # Alpha logic
          node_alpha = case_when(
            input$show_recruits & is_recruit ~ 1,
            input$show_recruits & !is_recruit ~ 0.8,
            TRUE ~ 1
          ),
          # Fill color logic
          node_fill = case_when(
            input$show_recruits & is_recruit ~ ifelse(!is.na(BroodID),
                                                      pal[as.character(BroodID)],
                                                      "#F0E1C6"),  # beige for non-clutch recruits
            input$show_recruits & !is_recruit ~ "#D3D3D3",         # grey for non-recruits
            TRUE ~ ifelse(!is.na(BroodID),
                          pal[as.character(BroodID)],
                          "#F0E1C6")                               # beige when not showing recruits
          )
        )
      
      
      # Add tooltip text
      fam <- fam %>%
        mutate(
          RNText = ifelse(grepl("^Unknown", RingNumber), "Unknown", RingNumber),
          tooltip_text = paste0(
            "RingNumber: ", RNText,
            ifelse(!is.na(BroodID), paste0("\nClutch: ", BroodID), ""),
            ifelse((sex != 3), paste0("\nSex: ", sex_text), ""),
            ifelse(!is.na(BirthYear), paste0("\nBirth Year: ", BirthYear), ""),
            ifelse(!is.na(RingYear), paste0("\nRing year: ", RingYear), "")
          )
        )
      
      
      # Base ggPedigreeInteractive plot
      p <- ggPedigreeInteractive(
        fam,
        personID = "RingNumber",
        dadID = "Father",
        momID = "Mother",
        sex_color_include = FALSE,  # we want clutch colours, not sex colours
        config = list(
          point_size = 6,
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
      
      
      # Overlay shapes in white so they are not visible
      p <- p +
        geom_point(
          data = nodes,
          aes(x = x_pos, y = y_pos, text = tooltip_text, shape = factor(sex)),
          size = 6, fill = "white", colour = "white",
          #alpha = other_nodes$node_alpha
          #show.legend = TRUE  # needed for shape legend
        ) +
        guides(shape = "none", fill = "none", colour = "none")
      
      
      # Separate out nodes
      focal_id <- selected_ring()
      clutch_nodes <- nodes %>% filter(!is.na(BroodID))
      other_nodes   <- nodes %>% filter(is.na(BroodID))
      focal_node    <- nodes %>% filter(RingNumber == focal_id)
      
      # Build plot with dynamic alpha and fill
      a <- p +
        geom_point(
          data = other_nodes,
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          fill = other_nodes$node_fill,
          #colour = other_nodes$node_fill,
          colour = "black",
          stroke = 0.3,
          size = 6,
          alpha = other_nodes$node_alpha
        ) +
        geom_point(
          data = clutch_nodes,
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          fill = clutch_nodes$node_fill,
          #colour = clutch_nodes$node_fill,
          colour = "black",
          stroke = 0.3,
          size = 6,
          alpha = clutch_nodes$node_alpha
        ) +
        geom_point(
          data = focal_node,
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
        guides(fill = "none", colour = "none", size = "none")
      
      
      # Render interactive plot
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
