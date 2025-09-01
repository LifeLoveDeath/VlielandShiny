
# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/

library(ggpedigree)
library(BGmisc) # helper utilities & example data
library(ggplot2) # ggplot2 for plotting
library(viridis) # viridis for color palettes
library(tidyverse) # for data wrangling
library(plotly)


vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
ped.data <- vlieland.data

# Change sex to numeric
ped.data[which(ped.data$Sex == "F"), "sex"] <- 0
ped.data[which(ped.data$Sex == "M"), "sex"] <- 1
ped.data$sex <- as.numeric(ped.data$sex)
summary(ped.data)

ggPedigree(
  data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1")

ggPedigreeInteractive(
  data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear")
  )

# Too much data - very slow!


# Get family tree data for an individual: RN00122
ind <- "RN00122"

# Get parents
p1 <- ped.data$Parent1[ped.data$RingNumber == ind]
p2 <- ped.data$Parent2[ped.data$RingNumber == ind]

# Get ind + direct children
ids.data <- ped.data[
  which(ped.data$RingNumber == ind |
          ped.data$Parent1 == ind |
          ped.data$Parent2 == ind), ]

# ids of rel individuals
ids <- unique(c(ids.data$RingNumber, ids.data$Parent1, ids.data$Parent2))

# Add siblings (share either parent)
sibs <- ped.data[
  which(ped.data$Parent1 %in% c(p1, p2) |
          ped.data$Parent2 %in% c(p1, p2)), ]

# Get all relevant data
fam.data <- ped.data[ped.data$RingNumber %in% unique(c(ids, sibs$RingNumber)), ]


# Anyone who appears in parent cols but not RingNumber needs an empty col?
# Identify all IDs that appear as parents
#all_parents <- unique(c(fam.data$Parent1, fam.data$Parent2))

# Find which of those are missing from the RingNumber list
#missing_parents <- setdiff(all_parents, fam.data$RingNumber)

# Create placeholder rows for missing parents
#sex_vals <- ifelse(
#  missing_parents %in% fam.data$Parent1, 1,
#  ifelse(missing_parents %in% fam.data$Parent2, 0, NA)
#)
#missing_rows <- data.frame(
#  RingNumber = missing_parents,
#  ColourRingCombo = NA,
#  ColourRingLeft1 = NA,
#  ColourRingLeft2 = NA,
#  ColourRingRight1 = NA,
#  ColourRingRight2 = NA,
#  Species = NA,
#  Sex = NA,
#  sex = sex_vals,     
#  BirthYear = NA,
#  DeathYear = NA,
#  Parent1 = NA,
#  Parent2 = NA
#)

# Combine with original
#fam.data <- bind_rows(fam.data, missing_rows)
#summary(fam.data)


# All parents mentioned in the family subset
all_parents <- unique(c(fam.data$Parent1, fam.data$Parent2))
all_parents <- all_parents[!is.na(all_parents)]   # drop NA

# Which are missing from the RingNumber column?
missing_parents <- setdiff(all_parents, fam.data$RingNumber)

# Skip if none missing
if (length(missing_parents) > 0) {
  
  # Helper: assign sex safely
  get_parent_sex <- function(id, df) {
    in_dad <- id %in% df$Parent1
    in_mom <- id %in% df$Parent2
    
    if (in_dad && !in_mom) return(1)   # male
    if (in_mom && !in_dad) return(0)   # female
    return(NA)                         # ambiguous or both
  }
  
  sex_vals <- vapply(missing_parents, get_parent_sex, numeric(1), df = fam.data)
  
  # Build placeholder rows with only required columns
  missing_rows <- tibble(
    RingNumber      = missing_parents,
    Parent1         = NA_character_,
    Parent2         = NA_character_,
    BirthYear       = NA_integer_,
    DeathYear       = NA_integer_,
    Sex             = NA,        # original sex col (if present)
    sex             = sex_vals,  # pedigree sex coding: 1=male, 0=female
    ColourRingCombo = NA,
    ColourRingLeft1 = NA,
    ColourRingLeft2 = NA,
    ColourRingRight1= NA,
    ColourRingRight2= NA,
    Species         = NA
  )
  
  fam.data <- bind_rows(fam.data, missing_rows)
}


# Function to get data
#getFamData <- function(x, data) {
#  ids.data <- data[which(data$RingNumber == x | data$Parent1 == x | data$Parent2 == x), ]
#  ids <- unique(c(ids.data$RingNumber, ids.data$Parent1, ids.data$Parent2))
#  fam.data <- data[which(data$RingNumber %in% ids), ]
#}
#getFamData("RN00122", ped.data)

head(fam.data)
fam.data

# Make sex a factor with meaningful labels
fam.data$sex <- factor(
  fam.data$sex,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

ggplotly(ggPedigree(
  fam.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1"))

ggPedigreeInteractive(
  fam.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
  config = list(
    sex_color_palette = c("#440154", "#5ec962"),
    sex_colour_include = T)) %>%
  config(
    displaylogo = FALSE,                 # remove plotly logo
    modeBarButtonsToRemove = c(
      "lasso2d", "select2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines",
      "sendDataToCloud", "toImage"
    )
  )






# Example from vignette
plt <- ggPedigreeInteractive(
  potter,
  famID    = "famID",
  personID = "personID",
  momID    = "momID",
  dadID    = "dadID"
) |> plotly::hide_legend()
plt



# Plot firectly with kinship?
library(kinship2)
foo <- pedigree(id = fam.data$RingNumber, dadid = fam.data$Parent1, momid = fam.data$Parent2, sex = fam.data$sex, relation = relation1)

ped <- foo['1']
plot(ped)




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


