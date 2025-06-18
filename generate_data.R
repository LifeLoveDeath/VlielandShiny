# Create dummy data
# Ring number, year of birth, colour rings


library(tidyverse)

set.seed(123)

# Define parameters
colours <- c("red", "white", "blue", "yellow/black","red/white", "blue/white", "white/blue", "white", "yellow", "orange", "green", "pink/blue", "pink/green")
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



# Add species:
# Sample 35 random rows for blue tits
blue_tit_rows <- sample(nrow(data), 35)

# Create a new column for species
data$Species <- "Great tit"  # default for all rows
data$Species[blue_tit_rows] <- "Blue tit" # change the random sample of 35 rows to blue tit


# Create missing data
missing_data_rows <- sample(nrow(data), 5) # 5 random rows to have missing data
data$BirthYear[missing_data_rows] <- NA # missing birth year data

# See data
head(data)

# Save data
write.csv(data, "data/DummyData.csv", row.names = FALSE)
