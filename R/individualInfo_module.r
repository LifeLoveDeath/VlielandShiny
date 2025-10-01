
# Individual info table module



library(shiny)
library(DT)

vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)


individual.data <- vlieland.data[which(vlieland.data$RingNumber == "RN00132"), ]
data.table(individual.data)


# Change order of vars
colnames(individual.data)
individual.data <- individual.data[ , c("RingNumber", "ColourRingCombo", "Species", "Sex", "BirthMonth", "BirthYear", "DeathYear", "Parent1", "Parent2", "Nestbox", "Lon", "Lat")]

# Change var names
colnames(individual.data) <- c("RingNumber", "Colour rings", "Species", "Sex", "Birth month", "Birth year", "Death year", "Parent1", "Parent2", "Birth nestbox", "Lon", "Lat")


# Convert to long data
individual.data.long <- data.frame(
  Variable = names(individual.data),
  Value = as.character(t(individual.data)),
  stringsAsFactors = FALSE
)


# Reformat vars
# Colour rings
individual.data.long[which(individual.data.long$Variable == "Colour rings"), "Value"] <- gsub("-", ", ", individual.data.long[which(individual.data.long$Variable == "Colour rings"), "Value"])


# Sex
# Need to change this
individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] <- if(individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] == "F") "Female" 
individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] <- if(individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] == "M") "Male"


datatable(
  individual.data.long,
  rownames = FALSE,
  options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)
