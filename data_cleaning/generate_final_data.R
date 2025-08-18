

# Create dummy data

library(tidyverse)

# Read in box location data
boxes <- read.csv("data/Coordinates_Boxes_Vlieland.csv", row.names = NULL)
head(boxes)


# Individuals dataset ---------------------------------------------------------------

set.seed(123)

##  Great tits ------------------------------------------------------------------------

# Parameters
n_start      <- 10
n_offspring  <- 3
n_events     <- 4
event_months <- c("March","April","May","June","July")
year_range   <- 1955:2025
target_n     <- 500

# Founding population
data <- data.frame(
  RingNumber = sprintf("RN%05d", 1:n_start),
  Sex        = sample(c("M","F"), n_start, replace = TRUE),
  BirthYear  = sample(year_range, n_start, replace = TRUE),
  BirthMonth = sample(event_months, n_start, replace = TRUE),
  DeathYear  = NA_integer_,
  Parent1    = NA_character_,
  Parent2    = NA_character_,
  sex        = NA_integer_,
  Nestbox = NA_integer_,
  Lon     = NA_integer_,
  Lat     = NA_integer_,
  stringsAsFactors = FALSE
)

data$sex <- ifelse(data$Sex == "M", 1, 2)

next_id <- n_start + 1
i <- 1

while(i <= nrow(data) && nrow(data) < target_n) {
  
  parent <- data[i, ]
  
  # Only adults reproduce
  repro_years <- parent$BirthYear + 1:3
  repro_years <- repro_years[repro_years %in% year_range]
  
  if(length(repro_years) == 0) {
    i <- i + 1
    next
  }
  
  # Pick a mate: opposite sex and already born before reproduction year
  mates <- data[data$Sex != parent$Sex & data$RingNumber != parent$RingNumber & data$BirthYear <= max(repro_years), ]
  if(nrow(mates) == 0) {
    i <- i + 1
    next
  }
  mate <- mates[sample(1:nrow(mates), 1), ]
  
  for(ev in seq_len(n_events)) {
    birth_year  <- sample(repro_years, 1)
    birth_month <- sample(event_months, 1)
    
    # Only reproduce if both parents are alive and born before birth_year
    if(birth_year <= parent$BirthYear || birth_year <= mate$BirthYear) next
    
    # Add location
    loc <- boxes[sample(1:nrow(boxes), 1), ]
    
    kids <- data.frame(
      RingNumber = sprintf("RN%05d", next_id:(next_id + n_offspring - 1)),
      Sex        = sample(c("M","F"), n_offspring, replace = TRUE),
      BirthYear  = birth_year,
      BirthMonth = birth_month,
      DeathYear  = birth_year + sample(2:5, n_offspring, replace = TRUE),
      Parent1    = rep(if(parent$Sex == "M") parent$RingNumber else mate$RingNumber, n_offspring),
      Parent2    = rep(if(parent$Sex == "F") parent$RingNumber else mate$RingNumber, n_offspring),
      sex        = NA_integer_,
      Nestbox    = rep(loc$Nestbox, n_offspring),
      Lon        = rep(loc$Lon, n_offspring),
      Lat        = rep(loc$Lat, n_offspring),
      stringsAsFactors = FALSE
    )
    kids$sex <- ifelse(kids$Sex == "M", 1, 2)
    
    data <- rbind(data, kids)
    next_id <- next_id + n_offspring
    
    if(nrow(data) >= target_n) break
  }
  
  i <- i + 1
}

rownames(data) <- NULL
summary(data$BirthYear)


# Add species
data$Species <- "Great tit"

# Rename
GreatTitData <- data



## Blue tits ----------------------------------------------------------

set.seed(123)

# Parameters
n_start      <- 10
n_offspring  <- 3
n_events     <- 4
event_months <- c("March","April","May","June","July")
year_range   <- 1955:2025
target_n     <- 300 # fewer blue tits

# Founding population
data <- data.frame(
  RingNumber = sprintf("RN%05d", 505:(504 + n_start)),
  Sex        = sample(c("M","F"), n_start, replace = TRUE),
  BirthYear  = sample(year_range, n_start, replace = TRUE),
  BirthMonth = sample(event_months, n_start, replace = TRUE),
  DeathYear  = NA_integer_,
  Parent1    = NA_character_,
  Parent2    = NA_character_,
  sex        = NA_integer_,
  Nestbox = NA_integer_,
  Lon     = NA_integer_,
  Lat     = NA_integer_,
  stringsAsFactors = FALSE
)

data$sex <- ifelse(data$Sex == "M", 1, 2)

next_id <- as.integer(sub("RN", "", tail(data$RingNumber, 1))) + 1

i <- 1

while(i <= nrow(data) && nrow(data) < target_n) {
  
  parent <- data[i, ]
  
  # Only adults reproduce (1-3 years old)
  repro_years <- parent$BirthYear + 1:3
  repro_years <- repro_years[repro_years %in% year_range]
  if(length(repro_years) == 0) {
    i <- i + 1
    next
  }
  
  # Choose a mate - opposite sex, born before reproduction years
  mates <- data[data$Sex != parent$Sex &
                  data$RingNumber != parent$RingNumber &
                  data$BirthYear <= max(repro_years), ]
  if(nrow(mates) == 0) {
    i <- i + 1
    next
  }
  mate <- mates[sample(1:nrow(mates), 1), ]
  
  for(ev in seq_len(n_events)) {
    birth_year  <- sample(repro_years, 1)
    birth_month <- sample(event_months, 1)
    
    # Only reproduce if both parents are already born
    if(birth_year <= parent$BirthYear || birth_year <= mate$BirthYear) next
    
    # Add location
    loc <- boxes[sample(1:nrow(boxes), 1), ]
    
    kids <- data.frame(
      RingNumber = sprintf("RN%05d", next_id:(next_id + n_offspring - 1)),
      Sex        = sample(c("M","F"), n_offspring, replace = TRUE),
      BirthYear  = birth_year,
      BirthMonth = birth_month,
      DeathYear  = birth_year + sample(2:5, n_offspring, replace = TRUE),
      # Male always Parent1, Female always Parent2
      Parent1    = rep(if(parent$Sex == "M") parent$RingNumber else mate$RingNumber, n_offspring),
      Parent2    = rep(if(parent$Sex == "F") parent$RingNumber else mate$RingNumber, n_offspring),
      sex        = NA_integer_,
      Nestbox    = rep(loc$Nestbox, n_offspring),
      Lon        = rep(loc$Lon, n_offspring),
      Lat        = rep(loc$Lat, n_offspring),
      stringsAsFactors = FALSE
    )
    kids$sex <- ifelse(kids$Sex == "M", 1, 2)
    
    data <- rbind(data, kids)
    next_id <- next_id + n_offspring
    
    if(nrow(data) >= target_n) break
  }
  
  i <- i + 1
}

rownames(data) <- NULL
data[1:20, ]
summary(data$BirthYear)


# Add species
data$Species <- "Blue tit"

# Rename
BlueTitData <- data


## Bind datasets ----------------------------------------------------------
data <- rbind(GreatTitData, BlueTitData)

data[which(data$DeathYear > 2025), "DeathYear"] <- NA




## Add colour rings -------------------------------------------------------

colours <- c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")

# Funtion to generate colour ring
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


# Add colour ring to dataframe
data$ColourRing = replicate(nrow(data), generate_colour_rings())


# Separate colour rings into cols
colourCombCols <- str_split_fixed(data$ColourRing, '-', 4) # get columns
data <- cbind(data, colourCombCols) # add to data
colnames(data) <- c(colnames(data)[1:12], "ColourRingCombo", "ColourRingLeft1", "ColourRingLeft2", "ColourRingRight1", "ColourRingRight2") # rename cols


##  Save -------------------------------------------------------
write.csv(data, "data/IndividualsData.csv", row.names = FALSE)



# Locations dataset long ----------------------------------------------------------



