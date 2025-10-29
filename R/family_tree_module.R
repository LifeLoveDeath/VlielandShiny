
# Family tree module

# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/


# Load packages - moved to app.r
# library(shiny)
# library(ggpedigree)
# library(ggplot2)
# library(viridis)
# library(tidyverse)
# library(plotly)


vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

ped.data <- vlieland.data 


# notes
# ggepedigree is interactive (plotly) but not very customisable (e.g. can't colour in the focal)
# kinship2/pedtools - notinteractive but more customisable so could be controlled using shiny tickboxes etc. to change data and redraw tree
# pedtool has these:
# Add/remove/extract individuals
#The functions below are used to modify an existing ped object by adding/removing individuals, or extracting a sub-pedigree. For details, see ?ped_modify.
#addChildren(), with special cases addSon(), addDaughter(), addChild()
#addParents()
#removeIndividuals()
#branch()
#subset()


# Function to get focal individual family tree data ---------------
get_family_subset <- function(ped.data, focal_id) {
  # Parents
  parents <- ped.data[ped.data$RingNumber == focal_id, c("Parent1", "Parent2")]
  p1 <- parents$Parent1
  p2 <- parents$Parent2
  
  # Children
  children <- ped.data$RingNumber[ped.data$Parent1 %in% focal_id | ped.data$Parent2 %in% focal_id]
  
  # Siblings (share at least one parent)
  siblings <- ped.data$RingNumber[ped.data$Parent1 %in% c(p1, p2) | ped.data$Parent2 %in% c(p1, p2)]
  
  # Combine all IDs
  ids <- unique(c(focal_id, p1, p2, children, siblings))
  
  # Subset the pedigree
  fam.data <- ped.data[ped.data$RingNumber %in% ids, ]
  
  # Add placeholder rows for missing parents
  all_parents <- unique(c(fam.data$Parent1, fam.data$Parent2))
  missing_parents <- setdiff(all_parents, fam.data$RingNumber)
  missing_parents <- missing_parents[!is.na(missing_parents)]
  
  if (length(missing_parents) > 0) {
    get_parent_sex <- function(id, df) {
      in_dad <- id %in% df$Parent1
      in_mom <- id %in% df$Parent2
      
      if (in_dad && !in_mom) return(1)   # male
      if (in_mom && !in_dad) return(2)   # female
      return(NA)                         # ambiguous or both
    }
    
    sex_vals <- vapply(missing_parents, get_parent_sex, numeric(1), df = fam.data)
    
    missing_rows <- tibble(
      RingNumber      = missing_parents,
      Parent1         = NA_character_,
      Parent2         = NA_character_,
      BirthYear       = NA_integer_,
      DeathYear       = NA_integer_,
      Sex             = NA,
      sex             = sex_vals,
      ColourRingCombo = NA,
      ColourRingLeft1 = NA,
      ColourRingLeft2 = NA,
      ColourRingRight1= NA,
      ColourRingRight2= NA,
      Species         = NA
    )
    
    fam.data <- bind_rows(fam.data, missing_rows)
  }
  
  fam.data$focal <- ifelse(fam.data$RingNumber == focal_id, TRUE, NA)
  
  fam.data
}


family_data <- get_family_subset(ped.data, "RN00402")






# UI -----------------------------------------
familyTreeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("tree_ui"))  # output for the pedigree
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
          tooltip_text = paste0(
            "RingNumber: ", RingNumber, "\n",
            "Clutch: ", clutchID, "\n",
            "Birth: ", BirthMonth, ", ", BirthYear, "\n",
            "Death: ", DeathYear
          )
        )
      
      
      
      # Base ggPedigreeInteractive plot
      p <- ggPedigreeInteractive(
        fam,
        #famID = "RingNumber",  # each individual as its own familyID here
        personID = "RingNumber",
        dadID = "Parent1",
        momID = "Parent2",
        sex_color_include = FALSE,  # we want clutch colours, not sex colours
        config = list(
          label_include = TRUE,
          label_column = "RingNumber",
          point_size = 6,
          segment_linewidth = 0.5,
          label_text_size = 3,
          label_nudge_y = 0.25,
          label_nudge_x = .1,
          label_text_angle = -30,
          return_static = TRUE
        ),
        tooltip_columns = c("RingNumber", "BirthYear", "BirthMonth", "DeathYear",  "clutchID")
      )
      
      nodes <- p$data %>%
        left_join(fam %>% select(RingNumber, clutchID, sex, focal), 
                  by = "RingNumber")
    
      
      # Separate clutch nodes from others
      clutch_nodes <- nodes %>% filter(!is.na(clutchID.x))
      other_nodes   <- nodes %>% filter(is.na(clutchID.x))
      
      # Add base plot + colouring
      a <- p +
        # all other nodes in beige
        geom_point(
          data = other_nodes,
          aes(x = x_pos, y = y_pos, shape = factor(sex.x), text = tooltip_text),
          size = 6, fill = "#F0E1C6", colour = "black"
        ) +
        # clutch nodes coloured with viridis
        geom_point(
          data = clutch_nodes,
          aes(x = x_pos, y = y_pos, fill = clutchID.x, shape = factor(sex.x), text = tooltip_text),
          size = 6, colour = "black", show.legend = FALSE
        ) +
        scale_shape_manual(
          name = "Sex",
          values = c("0" = 21, "1" = 22, "NA" = 23),
          labels = c("Female", "Male", "Unknown")
        ) +
        #scale_fill_viridis_d(option = "C", end = 0.85) 
        scale_fill_brewer(palette = "Paired")
      
      # Highlight focal bird (thick black border)
      focal_id <- selected_ring()
      a <- a +
        geom_point(
          data = nodes %>% filter(RingNumber == focal_id),
          aes(x = x_pos, y = y_pos),
          shape = 8, size = 6, fill = NA#,
          #colour = "black", stroke = 2
        )
      
      ggplotly(a, tooltip = "text") %>%
        style(showlegend = FALSE) %>%
        layout(dragmode = "pan") %>%   # <-- sets default click-drag to pan
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



