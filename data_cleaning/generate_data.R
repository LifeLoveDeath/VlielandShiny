# Create dummy data
# Ring number, year of birth, colour rings

library(tidyverse)

set.seed(123)

# Create data with ring number, colour rings, birth year etc. --------------------------------------------------------
# Define parameters
colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")
n_rows <- 100
years <- 1955:2024

# Will the colours actually be stored as numbers with a key in a separate table?
# Maybe I can format the dataframe so they are in the format used here

# Function to create colour combinations
generate_colour_rings <- function() {
  repeat {
    #length <- 4  # length always 4
    base_colours <- sample(colours, 3, replace = FALSE) # sample from colours (3 because one is metal)
    
    # metal position
    metal_pos <- sample(1:4, 1) # get position for metal (can be anywhere in sequence)
    
    # insert metal
    combo <- append(base_colours, "metal", after = metal_pos - 1) # add in metal
    
    # paste as string
    return(paste(combo, collapse = "-"))
  }
}


# Generate dataframe
data <- data.frame(
  RingNumber = sprintf("RN%05d", 1:n_rows),  # RN00001 etc.
  ColourRing = replicate(n_rows, generate_colour_rings()),
  BirthYear = sample(years, n_rows, replace = TRUE),
  stringsAsFactors = FALSE
)



# Separate colour rings into cols
colourCombCols <- str_split_fixed(data$ColourRing, '-', 4) # get columns
data <- cbind(data[,1:2], colourCombCols, data[ 3]) # add to data
colnames(data) <- c("RingNumber", "ColourRingCombo", "ColourRingLeft1", "ColourRingLeft2", "ColourRingRight1", "ColourRingRight2", "BirthYear") # rename cols



# Add colour abbreviations?


# Check duplicated ColourRing
data[which(duplicated(data$ColourRing) == TRUE), ]
# Colour combinations can be repeated - but they are in the real data as well, so practice with this
# None are actually repeated here



## Add species --------------------------------------------------------
# Sample 35 random rows for blue tits
blue_tit_rows <- sample(nrow(data), 35)

# Create a new column for species
data$Species <- "Great tit"  # default for all rows
data$Species[blue_tit_rows] <- "Blue tit" # change the random sample of 35 rows to blue tit


## Create missing data --------------------------------------------------------
missing_data_rows <- sample(nrow(data), 5) # 5 random rows to have missing data
data$BirthYear[missing_data_rows] <- NA # missing birth year data


## Add in locations --------------------------------------------------------

# read in coordinates data
boxes <- read.csv("data/Coordinates_Boxes_Vlieland.csv", row.names = NULL)

# Add nest of origin
# Add random next from boxes data
for(i in 1:nrow(data)) {
  data$OriginNestNo[i] <- boxes[sample(nrow(boxes), 1, replace = TRUE), "Nestbox"]
  data$OriginNestLon[i] <- boxes[which(boxes$Nestbox == data$OriginNestNo[i]), "Lon"]
  data$OriginNestLat[i] <- boxes[which(boxes$Nestbox == data$OriginNestNo[i]), "Lat"]
}


## Save data ------------------------------------------------------------------------------------
# See data
head(data)
summary(data)

# Save
write.csv(data, "data/DummyData.csv", row.names = FALSE)





# Location data (long) --------------------------------------------------------
# Read in ring number/colour data
colourRings <- read.csv("data/DummyData.csv")
# Select just ring number/colour/origin nest cols
colourRings <- colourRings[ , c("RingNumber", "ColourRingCombo", "BirthYear", "OriginNestNo", "OriginNestLon", "OriginNestLat")]
head(colourRings)

# Read in box location data
boxes <- read.csv("data/Coordinates_Boxes_Vlieland.csv", row.names = NULL)
head(boxes)

#Intialise empty data frame
locationData <- data.frame()

# Restructure data
for (i in 1:nrow(colourRings)) {
  ring_number <- colourRings$RingNumber[i]
  birth_year <- colourRings$BirthYear[i]

  #If BirthYear NA:
  if (is.na(birth_year)) {
    # Create row with NAs but keep RingNumber
    locationDataRow <- data.frame(
      RingNumber = ring_number,
      Event = NA,
      Year = NA,
      NestNo = NA,
      NestLon = NA,
      NestLat = NA,
      stringsAsFactors = FALSE
    )
  } else {
    # If BirthYear is not NA, add data
    locationDataRow <- data.frame(
      RingNumber = ring_number,
      Event = "birth",
      Year = birth_year,
      NestNo = colourRings$OriginNestNo[i],
      NestLon = colourRings$OriginNestLon[i],
      NestLat = colourRings$OriginNestLat[i],
      stringsAsFactors = FALSE
    )
  }
  locationData <- rbind(locationData, locationDataRow)
}


locationData

nestData <- data.frame()

for (i in 1:nrow(colourRings)) {
  ring_number <- colourRings$RingNumber[i]
  birth_year <- colourRings$BirthYear[i]
  
  # Determine years to simulate nest events
  if (is.na(birth_year)) {
    # Choose 3 consecutive years before 2025 (excluding NAs)
    valid_years <- na.omit(colourRings$BirthYear)
    start_year <- sample(min(valid_years, na.rm = TRUE):(2022), 1)
    years <- start_year:(start_year + 2)
  } else {
    max_year <- min(birth_year + 4, 2025)
    years <- (birth_year + 1):max_year
  }
  
  # Generate 1–2 nest events per year
  for (year in years) {
    n_nests <- sample(c(1, 2), size = 1, prob = c(0.7, 0.3))
    selected_boxes <- boxes %>% sample_n(n_nests)
    
    nest_events <- data.frame(
      RingNumber = rep(ring_number, n_nests),
      Event = rep("nest", n_nests),
      Year = rep(year, n_nests),
      NestNo = selected_boxes$Nestbox,
      NestLon = selected_boxes$Lon,
      NestLat = selected_boxes$Lat,
      stringsAsFactors = FALSE
    )
    
    nestData <- rbind(nestData, nest_events)
  }
}
  
nestData


nestLocationData <- rbind(locationData, nestData)

# This should have been a function

# Real data probably has months/specific dates - add these?


# Save
write.csv(nestLocationData, "data/NestLocationData.csv", row.names = FALSE)




# Real data probably has months/specific dates - add these?
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

# Months
months_vec <- c("March", "April", "May", "June", "July")

# Randomly assign month to each row
set.seed(123)
location.data$Month <- sample(months_vec, size = nrow(location.data), replace = TRUE)

# Reorder cols
location.data <- location.data[, c("RingNumber", "Event", "Month", "Year", "NestNo", "NestLon", "NestLat")]

head(location.data)

# Save
write.csv(location.data, "data/NestLocationData.csv", row.names = FALSE)






