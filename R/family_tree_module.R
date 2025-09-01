
# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/

library(ggpedigree)
library(ggplot2) # ggplot2 for plotting
library(viridis) # viridis for color palettes
library(tidyverse) # for data wrangling


# Function to get the data
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
      if (in_mom && !in_dad) return(0)   # female
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


fam.data <- get_family_subset(ped.data, "RN00402")

ggPedigreeInteractive(
  fam.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
  config = list(
    #focal_fill_column = "focal",
    #focal_fill_include = TRUE,
    #focal_fill_high_color = "yellow",
    sex_color_palette = c("#440154", "#5ec962"),
    sex_colour_include = T)) %>%
  config(
    displaylogo = FALSE,                 # remove plotly logo and other controls
    modeBarButtonsToRemove = c(
      "lasso2d", "select2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines",
      "sendDataToCloud", "toImage"
    )
  )

# focal highlight not working





# visNetwork --------------------------------------------------------------------------

library(visNetwork)

nodes <- data.frame(id = ped.data$RingNumber,
                    label = ped.data$RingNumber,
                    group = ifelse(ped.data$sex == 1, "male", "female"))

edges <- data.frame(from = ped.data$Parent1, to = ped.data$RingNumber) %>%
  rbind(data.frame(from = ped.data$Parent2, to = ped.data$RingNumber))

visNetwork(nodes, edges) %>%
  visNodes(shape = "ellipse") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

# Too much data - slow

nodes <- data.frame(id = fam.data$RingNumber,
                    label = fam.data$RingNumber,
                    group = ifelse(fam.data$sex == 1, "male", "female"))

edges <- data.frame(from = fam.data$Parent1, to = fam.data$RingNumber) %>%
  rbind(data.frame(from = fam.data$Parent2, to = fam.data$RingNumber))

visNetwork(nodes, edges) %>%
  visNodes(shape = "ellipse") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

# Complicated to make it a family tree rather than a network



# collapsibleTree ---------------------------------------------------------------------


