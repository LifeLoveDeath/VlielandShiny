# Create dummy data
# Ring number, year of birth, colour rings

set.seed(123)

# Define parameters
colours <- c("purple", "red", "white", "blue", "black")
n_rows <- 100
years <- 1955:2024

# Will the colours actually be stored as numbers with a key in a separate table?
# Maybe I can format the dataframe so they are in the format used here

# Function to create colour combinations
generate_colour_rings <- function() {
  repeat {
    len <- sample(4:6, 1)  # length 4 - 6
    base_colours <- sample(colours, len - 1, replace = FALSE)
    
    # metal position - final 3 but not last
    metal_pos <- sample((len-2):(len-1), 1)
    
    # insert metal
    combo <- append(base_colours, "metal", after = metal_pos - 1)
    
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

# Check duplicated ColourRing
data[which(duplicated(data$ColourRing) == TRUE), ]
# Colour combinations are repeated - but they are in the real data as well, so practice with this



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
