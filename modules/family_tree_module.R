
# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/

library(ggpedigree)
library(BGmisc) # helper utilities & example data
library(ggplot2) # ggplot2 for plotting
library(viridis) # viridis for color palettes
library(tidyverse) # for data wrangling


vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
ped.data <- vlieland.data

# Change sex to numeric
ped.data[which(ped.data$Sex == "F"), "sex"] <- 0
ped.data[which(ped.data$Sex == "M"), "sex"] <- 1
ped.data$sex <- as.numeric(ped.data$sex)
summary(ped.data)

ggPedigree(
  ped.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1")

# Too much data - very slow!


# Get family tree data for an individual: RN00122
ids.data <- ped.data[which(ped.data$RingNumber == "RN00122" | ped.data$Parent1 == "RN00122" | ped.data$Parent2 == "RN00122"), ]
ids <- unique(c(ids.data$RingNumber, ids.data$Parent1, ids.data$Parent2))
fam.data <- ped.data[which(ped.data$RingNumber %in% ids), ]

# Anyone who appears in parent cols but not RingNumber needs an empty col?
# Identify all IDs that appear as parents
all_parents <- unique(c(fam.data$Parent1, fam.data$Parent2))

# Find which of those are missing from the RingNumber list
missing_parents <- setdiff(all_parents, fam.data$RingNumber)

# Create placeholder rows for missing parents
sex_vals <- ifelse(
  missing_parents %in% fam.data$Parent1, 1,
  ifelse(missing_parents %in% fam.data$Parent2, 0, NA)
)
missing_rows <- data.frame(
  RingNumber = missing_parents,
  ColourRingCombo = NA,
  ColourRingLeft1 = NA,
  ColourRingLeft2 = NA,
  ColourRingRight1 = NA,
  ColourRingRight2 = NA,
  Species = NA,
  Sex = NA,
  sex = sex_vals,     
  BirthYear = NA,
  DeathYear = NA,
  Parent1 = NA,
  Parent2 = NA
)

# Combine with original
fam.data <- bind_rows(fam.data, missing_rows)
summary(fam.data)

# Function to get data
#getFamData <- function(x, data) {
#  ids.data <- data[which(data$RingNumber == x | data$Parent1 == x | data$Parent2 == x), ]
#  ids <- unique(c(ids.data$RingNumber, ids.data$Parent1, ids.data$Parent2))
#  fam.data <- data[which(data$RingNumber %in% ids), ]
#}
#getFamData("RN00122", ped.data)

#fam.data

ggPedigree(
  fam.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1")

ggPedigreeInteractive(
  fam.data,
  #famID    = "famID",
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1")


# visNetwork --------------------------------------------------------------------------


# collapsibleTree ---------------------------------------------------------------------


