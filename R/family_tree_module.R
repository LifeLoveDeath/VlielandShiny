
# Family tree module

# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/


# Load packages - moved to app.r
# library(shiny)
 library(ggpedigree)
 library(ggplot2)
 library(viridis)
 library(tidyverse)
 library(kinship2)
 library(plotly)
 library(RColorBrewer)

# Old dummy data
#vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
#location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

# Real data
IndividualDataVlieland <- read.csv('data/IndividualDataVlieland.csv', row.names = NULL)
location.data <- read.csv("data/location_data.csv", row.names = NULL) 
IndividualInfo <- read.csv('data/IndividualInfo.csv', row.names = NULL)

ped.data <- IndividualDataVlieland 
head(as.data.frame(ped.data))



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
  
  # Recruit flag (appears as breeder)
  fam.data <- fam.data %>%
    mutate(
  is_recruit = if (!is.null(ped.data) && !is.null(brood_data)) {
    RingNumber %in% unique(c(
      ped.data$Mother,
      ped.data$Father,
      brood_data$RingNumberFemale,
      brood_data$RingNumberMale
    ))
  } else {
    FALSE
  }
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
#family_data <- get_family_subset(ped.data, "AH...68076", BroodData) # this one has half sibs




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
       checkboxInput(ns("show_recruits"), "Show recruits", value = TRUE),
       checkboxInput(ns("show_half_sibs"), "Show half-siblings", value = TRUE),
       
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
familyTreeServer <- function(id, ped.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: subset family tree data for focal bird
    family_data <- reactive({
      req(selected_ring())
      get_family_subset(ped.data, selected_ring())
    })
    
    
    # Render the pedigree plot
    output$tree_ui <- renderUI({
      req(family_data())
      plotlyOutput(ns("pedigree_plot"), height = "600px")
    })
    
    output$pedigree_plot <- renderPlotly({
      fam <- family_data()
      
      
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
      # Remove guides - they're wrong?
      p <- p + guides(shape = "none", fill = "none", colour = "none")
      p <- p +
        guides(shape = "none", fill = "none", colour = "none") +
        scale_shape_identity() +
        scale_fill_identity() +
        scale_colour_identity()
      
      
      # Get node location data to overlay shapes etc. using ggplot
      nodes <- p$data %>%
        left_join(fam %>% select(RingNumber, focal), 
                  by = "RingNumber")
    
      # Define focal
      focal_id <- selected_ring()
      
      # Separate clutch nodes from others
      clutch_nodes <- nodes %>% filter(!is.na(BroodID))
      other_nodes   <- nodes %>% filter(is.na(BroodID))
      focal_node   <- nodes %>% filter(RingNumber == focal_id)
      
      # get palette for BroodID
      brood_levels <- sort(unique(clutch_nodes$BroodID))
      pal <- brewer.pal(n = max(3, length(brood_levels)), name = "Paired")[1:length(brood_levels)]
      
      # make a named palette
      names(pal) <- brood_levels
      
      
      a <- p +
        # other nodes in beige
        geom_point(
          data = other_nodes,
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          size = 6, fill = "#F0E1C6", colour = "#F0E1C6"#,
          #show.legend = TRUE  # needed for shape legend
        ) +
        # clutch nodes with matching fill and border
        geom_point(
          data = clutch_nodes,
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          fill = pal[as.character(clutch_nodes$BroodID)],
          colour = pal[as.character(clutch_nodes$BroodID)],
          size = 6#,
          #show.legend = FALSE
        ) +
        # focal node larger with thick border
        geom_point(
          data = focal_node,
          aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
          fill = pal[as.character(focal_node$BroodID)],
          colour = "black",
          size = 8, stroke = 1#,
          #show.legend = FALSE
        ) +
        scale_shape_manual(
          name = "sex",
          values = c("2" = 21, "1" = 22, "3" = 23),  # circle, square, diamond
          labels = c("Female", "Male", "Unknown")
        ) +
        guides( fill = "none", colour = "none", size = "none")
      
      
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









# 
# # trying different packages (continued in testing script)
# 
# # trying ggpedigreeInteractive -------
# # Very good vignettes:
# # Can colour individuals, by status, focal relatedness 
# #http://cran.r-project.org/web/packages/ggpedigree/vignettes/v00_plots.html
# 
# 
# fam.data$personID <- fam.data$RingNumber
# fam.data$personID <- gsub("RN", "", fam.data$personID)
# fam.data$personID <- as.numeric(fam.data$personID)
# fam.data$momID <- fam.data$Parent2
# fam.data$momID <- gsub("RN", "", fam.data$momID)
# fam.data$momID <- as.numeric(fam.data$momID)
# fam.data$dadID <- fam.data$Parent1
# fam.data$dadID <- gsub("RN", "", fam.data$dadID)
# fam.data$dadID <- as.numeric(fam.data$dadID)
# 
# # note on status
# data("hazard") # status column
# status_color_palette = c(color_palette_default[1], color_palette_default[2])
# status_color_affected = "black"
# status_color_unaffected = color_palette_default[2]
# 
# # add status column for clutches
# fam.data$status <- 0
# ids <- c(401, 402, 403)
# fam.data[which(fam.data$personID %in% ids), "status"] <- 1
# fam.data$status <- as.factor(fam.data$status)
# 
# ggPedigree(
#   fam.data,
#   personID = "personID",
#   #momID    = "Parent2",
#   #dadID    = "Parent1",
#   status_column = "status",
#   config = list(
#     label_include = TRUE,
#     label_column = "RingNumber",
#     point_size = 6,
#     outline_multiplier = 1.5,
#     segment_linewidth = 0.5,
#     label_text_size = 4.5,
#     label_nudge_y = 0.2,
#     label_nudge_x = 1.3,
#     label_text_angle = -30,
#     focal_fill_personID = 402,
#     focal_fill_include = TRUE,
#     focal_fill_high_color = "#d55e00",
#     focal_fill_mid_color = "pink",
#     focal_fill_low_color = "blue",
#     focal_fill_scale_midpoint = 0.9,
#     focal_fill_component = "additive",
#     focal_fill_method = "steps",
#     focal_fill_force_zero = TRUE,
#     focal_fill_na_value = "grey10",
#     sex_color_include = FALSE#,
#     #status_code_affected = 1,
#     #status_code_unaffected = 0,
#     #status_shape_affected = 4
#     ),
#   ) + 
#   theme(
#       legend.position = "none")
# 
#   
# # notes: 
# data("hazard") # status column
# status_color_palette = c(color_palette_default[1], color_palette_default[2])
# status_color_affected = "black"
# status_color_unaffected = color_palette_default[2]
# 
# # add status column for clutches
# fam.data$status <- "FALSE"
# ids <- c(401, 402, 403)
# fam.data[which(fam.data$personID %in% ids), "status"] <- "TRUE"
# 
# 
# 
# 
# 
# # when interactive add:
# tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
# 
# #%>%
#   config(
#     displaylogo = FALSE,                 # remove plotly logo and other controls
#     modeBarButtonsToRemove = c(
#       "lasso2d", "select2d",
#       "hoverClosestCartesian", "hoverCompareCartesian",
#       "toggleSpikelines",
#       "sendDataToCloud", "toImage"
#     )
#   )
# 
# 
# # Plot with ggpedigree and then wrap in ploty?
# # Create the plot
# p <- ggPedigree(
#   fam.data,
#   personID = "RingNumber",
#   momID    = "Parent2",
#   dadID    = "Parent1",
#   tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
#   config = list(segment_linewidth = .25,
#                 point_size = 2,
#   sex_color_palette = c("#440154", "#5ec962"),
#   label_include = FALSE))
# 
# # Convert to plotly and remove extra buttons
# p <- p %>%
#   config(
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = c(
#       "lasso2d","select2d",
#       "hoverClosestCartesian","hoverCompareCartesian",
#       "toggleSpikelines",
#       "sendDataToCloud","toImage"
#     )
#   )
# 
# p
# 
# 
# 
# # Using fam.id to colour cluthes
# fam.data <- fam.data %>%
#   # Combine parent IDs and birth date to define a clutch
#   mutate(
#     clutch_key = paste(Parent1, Parent2, BirthYear, BirthMonth, sep = "_")
#   ) %>%
#   # Assign a numeric clutch ID
#   group_by(clutch_key) %>%
#   mutate(
#     clutch_id = cur_group_id()
#   ) %>%
#   ungroup() %>%
#   select(-clutch_key)  # optional: remove intermediate column
# 
# #here
# # Render as static
# fam.data$clutch_id <- as.factor(fam.data$clutch_id)
# fam.data$id <- fam.data$RingNumber
# #
# 
# 
# #staticPed <-
# ggPedigreeInteractive(
#   fam.data,
#   #famID = "clutch_id",
#   personID = "id",
#   momID    = "Parent2",
#   dadID    = "Parent1",
#   tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
#   config = list(segment_linewidth = .25,
#                 point_size = 2,
#                 #overlay_column = "clutch_id",
#                 #overlay_include = TRUE,
#                 sex_color_palette = c("#440154", "#5ec962"),
#                 label_include = FALSE#,
#                 #return_static = TRUE
#                 )
#                 )
# 
# 
# staticPed
# 
# # Add static customisaion using ggplot
# # Colour by clutch?
# 
# staticPed + scale_color_viridis(
#   discrete = TRUE,
#   labels = c("Female", "Male", "Unknown"))
# 
# 
# 
# # Return to interactive
# plotly::ggplotly(staticPed,
#                          tooltip = "text")
# 
# 
# 
# 
# 
# # using kinship ----------------
# library(kinship2)
# library(visNetwork)
# head(fam.data)
# # Create pedigree object
# ped <- pedigree(id = fam.data$RingNumber,
#                 dadid = fam.data$Parent1,
#                 momid = fam.data$Parent2,
#                 sex = fam.data$sex,
#                 affected = fam.data$focal)  # or use a column if you want to highlight certain individuals
# 
# 
# plot(ped, cex = 1,
#      )  
# 
# # Create color palette for clutch_id
# clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
# names(clutch_palette) <- unique(fam.data$clutch_id)
# 
# # Map node colors
# node_colors <- clutch_palette[fam.data$clutch_id]
# 
# # Create pedigree
# ped <- pedigree(
#   id    = fam.data$RingNumber,
#   dadid = fam.data$Parent1,
#   momid = fam.data$Parent2,
#   sex   = fam.data$sex,
#   affected = fam.data$focal
# )
# 
# # Create color palette for clutch_id
# clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
# names(clutch_palette) <- unique(fam.data$clutch_id)
# 
# # Map node colors in the same order as ped$id
# node_colors <- clutch_palette[match(ped$id, fam.data$RingNumber) %>% sapply(function(i) fam.data$clutch_id[i])]
# 
# # Better way: directly match RingNumber to clutch_id
# node_colors <- clutch_palette[fam.data$clutch_id[match(ped$id, fam.data$RingNumber)]]
# 
# # Plot pedigree
# plot(
#   ped,
#   cex = 0.8,
#   col = node_colors,       # color by clutch_id
#   symbolsize = 1.5,
#   affected = fam.data$focal  # highlight focal birds
# )
#  # struggling with colouring
# 
# 
# # Using pedtools -----------
# library(pedtools)
# head(fam.data)
# head(fam.data[ ,c("sex", "Sex")])
# 
# # get clutches
# fam.data <- fam.data %>%
#   # Combine parent IDs and birth date to define a clutch
#   mutate(
#     clutch_key = paste(Parent1, Parent2, BirthYear, BirthMonth, sep = "_")
#   ) %>%
#   # Assign a numeric clutch ID
#   group_by(clutch_key) %>%
#   mutate(
#     clutch_id = cur_group_id()
#   ) %>%
#   ungroup() %>%
#   select(-clutch_key)  # optional: remove intermediate column
# 
# 
# ped <- ped(id = fam.data$RingNumber, fid =fam.data$Parent1, mid = fam.data$Parent2, sex = fam.data$sex)
# 
# 
# clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
# names(clutch_palette) <- unique(fam.data$clutch_id)
# fill_colors <- clutch_palette[fam.data$clutch_id]
# names(fill_colors) <- fam.data$RingNumber
# # Need to remove NAs
# # And make it viridis
# 
# as.data.frame(ped)
# 
# plot(ped, hatched = "RN00402")
# a <- plot(ped, hatched = "RN00402",
#      fill = fill_colors,
#      label = FALSE)
# 
# ggpedigree(a)
# 
# 
# 
# # can build up:
# singleton(id = fam.data[which(fam.data$RingNumber == "RN00402", "RingNumber")], 
#           fid = ,
#           mid = ,
#           sex = )
# 
# 
# # Could be singletone, nuclear family, extended family
# 
# 
# # use subseting
# # branch -  just gets descendents of focal individual from full dataset
# 
# ped.full <- pedtools::ped(
#   id  = ped.data$RingNumber,
#   fid = ped.data$Parent1,
#   mid = ped.data$Parent2,
#   sex = ped.data$sex
# )
# # Suppose your ID of interest is:
# focus_id <- "RN00131"
# fam_index <- sapply(ped.full, function(x) focus_id %in% x$ID)
# fam_index
# 
# ped.single <- ped.full[[which(fam_index)]]
# 
# 
# branch(ped.single, "RN00131")
#        
# 
# ancestors(ped.single, "RN00131")
# 
# 
# 
# # collapsibleTree ---------------------------------------------------------------------
# 
# library(collapsibleTree)
# # can only show one parent
# 
# # Prepare a long-format for hierarchy (Parent -> Child)
# # Combine Parent1 and Parent2 into one column
# ped_long <- fam.data %>%
#   select(RingNumber, Parent1, Parent2) %>%
#   pivot_longer(cols = c(Parent1, Parent2), names_to = "ParentType", values_to = "Parent") %>%
#   filter(!is.na(Parent))  # remove missing parents
# 
# # Plot top-down tree
# collapsibleTree(
#   ped_long,
#   hierarchy = c("Parent", "RingNumber"),  # Parent -> Child
#   #root = "Root",                          # virtual root for unconnected parents
#   direction = "tb",                        # top-down
#   width = 800,
#   height = 600,
#   nodeSize = "leafCount",
#   fontSize = 12
# )
# 
# 
# # Trying again
# # Prepare data in long format: child -> parents
# fam.long <- fam.data %>%
#   select(child = RingNumber, Parent1, Parent2) %>%
#   tidyr::pivot_longer(cols = c("Parent1", "Parent2"), names_to = "parent_type", values_to = "parent") %>%
#   filter(!is.na(parent) & parent != "")
# 
# # You need a hierarchical structure: root -> generation -> child
# # One simple way is to treat Parent1 as main lineage for tree
# tree.data <- fam.data %>%
#   select(Parent1, RingNumber) %>%
#   rename(parent = Parent1, child = RingNumber) %>%
#   filter(!is.na(parent) & parent != "")
# 
# # Plot interactive collapsible tree
# collapsibleTree(
#   tree.data,
#   hierarchy = c("parent", "child"),
#   root = "Root",
#   fill = "child"#,
#   #nodeSize = "leafCount"
# )



