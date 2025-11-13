
# Processing data to work with app

library(tidyverse)
library(dplyr)
library(lubridate)
library(geosphere) # distances


# Load dummy data
vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
colnames(vlieland.data)
head(vlieland.data)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)
head(location.data)



# Load real data
IndividualData <- read.csv("data/IndividualData.csv", row.names = NULL)
colnames(IndividualData)
head(IndividualData)
# Reduce to just Vlieland
IndividualDataVlieland <- IndividualData[which(IndividualData$RingPopulationName == "Vlieland"), ]


BroodData <- read.csv("data/BroodData.csv", row.names = NULL)
head(BroodData)
# FIXING ERROR - remove this step if needed <-----------
# One of these is wrong - Year in laydate is 2026 and month is october (next latest month in year is July)
BroodData <- BroodData[-which(lubridate::year(BroodData$LayDate) != BroodData$BroodYear), ]
write.csv(BroodData, "data/BroodData.csv", row.names = FALSE)



ColourNumberRings <- read.csv("data/ColourNumberRings.csv", row.names = NULL)
head(ColourNumberRings)



# Rows of IndividualDataVlieland where BroodID appears in BroodData
matches <- IndividualDataVlieland[which(IndividualDataVlieland$BroodID %in% BroodData$ID), ]
length(matches) # 18
missing <- IndividualDataVlieland %>%
  filter(!BroodID %in% BroodData$ID)


# Extract RingNumbers from both dataframes
individual_rings <- IndividualDataVlieland$RingNumber
brood_rings <- c(BroodData$RingNumberFemale, BroodData$RingNumberMale)

# Find which individual rings appear in BroodData
rings_in_broods <- intersect(individual_rings, brood_rings)

# See the result
rings_in_broods
length(rings_in_broods)  # number of rings that appear - 62



# Data prep ---------------------------------------------------------------


# Individual data ---------------------------------------------------------


## Add ring number ---------------------------------------------------------

# Just Vlieland data
IndividualDataVlieland <- IndividualData[which(IndividualData$RingPopulationName == "Vlieland"), ]
# Ensure both columns are UTF-8
IndividualDataVlieland <- IndividualDataVlieland %>%
  mutate(RingNumber = iconv(RingNumber, from = "", to = "UTF-8"))

ColourNumberRings <- ColourNumberRings %>%
  mutate(RingNumber = iconv(RingNumber, from = "", to = "UTF-8"))

# All ring number from ColourNumberRings
IndividualDataVlieland <- merge(
  IndividualDataVlieland,
  ColourNumberRings,
  by = "RingNumber",
  all.x = TRUE   # keep all rows from IndividualData
)


# Split into separate cols for each ring colour

# Define code-to-colour mapping
colour_map <- c(
  bl = "blue", bw = "blue/white", gr = "green", al = "metal", or = "orange",
  pb = "pink/blue", pg = "pink/green", re = "red", rw = "red/white",
  wh = "white", wb = "white/blue", ye = "yellow", yb = "yellow/black", pi = "pink",
  gw = "green/white"
)

# Function to parse a ring combo
parse_ring <- function(x) {
  if (is.na(x)) return(data.frame(
    ColourRingLeft1 = NA, ColourRingLeft2 = NA,
    ColourRingRight1 = NA, ColourRingRight2 = NA,
    stringsAsFactors = FALSE,
    row.names = NULL
  ))
  
  sides <- strsplit(x, " - ")[[1]]
  
  left <- strsplit(sides[1], " ")[[1]]
  right <- strsplit(sides[2], " ")[[1]]
  
  # Pad with NA if less than 2 elements
  left <- c(left, rep(NA, 2 - length(left)))
  right <- c(right, rep(NA, 2 - length(right)))
  
  # Map codes to colours
  left <- colour_map[left]
  right <- colour_map[right]
  
  data.frame(
    ColourRingLeft1 = left[1],
    ColourRingLeft2 = left[2],
    ColourRingRight1 = right[1],
    ColourRingRight2 = right[2],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Apply to dataframe
IndividualDataVlieland <- IndividualDataVlieland %>%
  rowwise() %>%
  mutate(tmp = list(parse_ring(RingColour))) %>%
  unnest_wider(tmp) %>%
  ungroup()


# Add colour ring combo column:IndividualDataVlieland <- IndividualDataVlieland %>%
IndividualDataVlieland <- IndividualDataVlieland %>%
  mutate(
    ColourRingCombo = pmap_chr(
      list(ColourRingLeft1, ColourRingLeft2, ColourRingRight1, ColourRingRight2),
      ~ paste(na.omit(c(...)), collapse = "-")
    )
  )


## Rename columns ----------------------------------------------------------

colnames(IndividualDataVlieland)
# Change Species column name
IndividualDataVlieland <- IndividualDataVlieland %>%
  rename(Species = SpeciesName)





## Save IndividualDataVlieland ---------------------------------------------

# Find duplicates
IndividualDataVlieland %>%
  count(RingNumber) %>% filter(n > 1)

IndividualDataVlieland %>%
  filter(RingNumber %in% c("AH...93589", "AK...73457"))

# Remove duplicates
IndividualDataVlieland <- IndividualDataVlieland %>% distinct()


# Save
write.csv(IndividualDataVlieland, file = "data/IndividualDataVlieland.csv", row.names = FALSE)
head(as.data.frame(IndividualDataVlieland))
head(as.data.frame(IndividualDataVlieland[!is.na(IndividualDataVlieland$RingColour), ]))






# Brood/Location data -----------------------------------------------------

# Create location dataframe that will work with mapping modules

# Should actually build from ids in IndividualDataVlieland
# ad incroporate ring location and check whether it's the same as birth location

# Ensure dates are Date objects
BroodData <- BroodData %>%
  mutate(LayDate = as.Date(LayDate))



## Get ring events ---------------------------------------------------------

ring_events <- IndividualDataVlieland %>%
  filter(!is.na(RingNumber)) %>%
  transmute(
    RingNumber = RingNumber,
    Event = "ring",
    Month = NA_character_,  # no month info for ring event
    Year = RingYear,
    NestNo = RingNestBox,
    NestLon = RingLongitude,
    NestLat = RingLatitude
  )
ring_events$LayDate <- NA
ring_events$ClutchSize <- NA

## Get birth events --------------------------------------------------------

# Join IndividualDataVlieland with BroodData using BroodID
birth_events <- IndividualDataVlieland %>%
  left_join(BroodData, by = c("BroodID" = "ID")) %>%
  mutate(
    Event = "birth",
    Year = BirthYear,
    Month = NA,                 # If you want, you could extract the month from LayDate instead
    NestNo = BroodNestBox,
    NestLon = BroodLongitude,
    NestLat = BroodLatitude,
    LayDate = as.Date(LayDate),  
    ClutchSize = ClutchSize 
  ) %>%
  select(RingNumber, Event, Year, Month, NestNo, NestLon, NestLat, LayDate)



## Get nesting events ------------------------------------------------------

# Female reproduction
female_nests <- BroodData %>%
  filter(!is.na(RingNumberFemale)) %>%
  transmute(
    RingNumber = RingNumberFemale,
    Event = "nest",
    Month = month(LayDate, label = TRUE, abbr = TRUE),
    Year = BroodYear,
    NestNo = BroodNestBox,
    NestLon = BroodLongitude,
    NestLat = BroodLatitude,
    LayDate = LayDate,
    ClutchSize = ClutchSize
  )

# Male reproduction
male_nests <- BroodData %>%
  filter(!is.na(RingNumberMale)) %>%
  transmute(
    RingNumber = RingNumberMale,
    Event = "nest",
    Month = month(LayDate, label = TRUE, abbr = TRUE),
    Year = BroodYear,
    NestNo = BroodNestBox,
    NestLon = BroodLongitude,
    NestLat = BroodLatitude,
    LayDate = LayDate,
    ClutchSize = ClutchSize
  )

reproduction_events <- bind_rows(female_nests, male_nests)



## Combine birth and nest events -------------------------------------------
# Make sure relevant columns are character/numeric
ring_events <- ring_events %>%
  mutate(
    NestNo = as.character(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )
birth_events <- birth_events %>%
  mutate(
    NestNo = as.character(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )
reproduction_events <- reproduction_events %>%
  mutate(
    NestNo = as.character(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )

location_data <- bind_rows(ring_events, birth_events, reproduction_events) %>%
  arrange(RingNumber, Year, Month)


# Check which of these ring numbers appear in IndividualDataVlieland
ids <- location_data %>%
  filter(RingNumber %in% IndividualDataVlieland$RingNumber) %>%
  distinct(RingNumber)
nrow(ids) # 4934
length(unique(IndividualDataVlieland$RingNumber)) # 4934 - all IDs 



## Add missing months for nest events --------------------------------------

location_data <- location_data %>%
  mutate(
    # Only update Month if Event = "nest" and Month is NA
    Month = if_else(
      Event == "nest" & is.na(Month) & !is.na(LayDate),
      month(LayDate, label = TRUE, abbr = FALSE),  # full month name
      Month
    )
  )




## Checking data ----------------------------------------


# Check ring and birth years
ring_birth_check <- IndividualDataVlieland %>%
  filter(!is.na(RingYear) & !is.na(BirthYear)) %>%
  mutate(RingMatchesBirth = RingYear == BirthYear)

# Filter only the ones that don't match
ring_birth_mismatches <- ring_birth_check %>%
  filter(!RingMatchesBirth) # none?

# Get birth and ring events from location_data
birth_ring_comparison <- location_data %>%
  # Keep only birth or ring events
  filter(Event %in% c("birth", "ring")) %>%
  # Select relevant columns
  select(RingNumber, Event, Year, NestNo, NestLon, NestLat) %>%
  # Pivot wider so birth and ring info are side by side
  pivot_wider(
    names_from = Event,
    values_from = c(Year, NestNo, NestLon, NestLat),
    names_glue = "{Event}_{.value}"
  ) %>%
  # Rename year columns for clarity
  rename(
    BirthYear = birth_Year,
    RingYear  = ring_Year,
    BirthNestBox = birth_NestNo,
    BirthNestLon = birth_NestLon,
    BirthNestLat = birth_NestLat,
    RingNestBox = ring_NestNo,
    RingNestLon = ring_NestLon,
    RingNestLat = ring_NestLat
  )

# They don't match



# Finding birds for whom we have a ring/birth location and at least one nest location 
# Individuals with birth or ring location
birth_or_ring <- location_data %>%
  filter(Event %in% c("birth", "ring")) %>%
  distinct(RingNumber)

# Individuals with at least one nest
with_nest <- location_data %>%
  filter(Event == "nest") %>%
  distinct(RingNumber)

# Individuals with both
individuals_with_birth_or_ring_and_nest <- intersect(birth_or_ring$RingNumber,
                                                     with_nest$RingNumber)

# Data frame with all info for these individuals
location_data_filtered <- location_data %>%
  filter(RingNumber %in% individuals_with_birth_or_ring_and_nest)

# Find which of these we have a RingColour for
# Filter the individual data to those with a RingColour
individuals_with_colour <- IndividualDataVlieland %>%
  filter(!is.na(RingColour)) %>%
  select(RingNumber, RingColour)

# Keep only those RingNumbers that are in the previous filtered set
filtered_with_colour <- individuals_with_colour %>%
  filter(RingNumber %in% individuals_with_birth_or_ring_and_nest)



# See which birds have different birth and ring years
diff_years <- IndividualDataVlieland %>%
  filter(!is.na(BirthYear), !is.na(RingYear),
         BirthYear != RingYear) # none



## Fill missing birth locations from ring locations ------------------------
# Where BirthYear == RingYear and Birth location data is missing:
# Fill Birth location from Ring Location data

# SKIP IF WRONG 

location_data <- location_data %>%
  group_by(RingNumber) %>%
  mutate(
    # find the first ring event values in this group (may be NA if none)
    ring_year = Year[Event == "ring"][1],
    ring_nest = NestNo[Event == "ring"][1],
    ring_lon  = NestLon[Event == "ring"][1],
    ring_lat  = NestLat[Event == "ring"][1],
    
    # condition: this row is a birth row AND all three location fields are NA
    birth_all_loc_missing = (Event == "birth") &
      is.na(NestNo) & is.na(NestLon) & is.na(NestLat),
    
    # condition: birth year equals ring year (and ring_year not NA)
    birth_ring_year_match = !is.na(ring_year) & (Year == ring_year),
    
    # Only when both conditions true, copy ring values into birth row fields
    NestNo = ifelse(birth_all_loc_missing & birth_ring_year_match, ring_nest, NestNo),
    NestLon = ifelse(birth_all_loc_missing & birth_ring_year_match, ring_lon, NestLon),
    NestLat = ifelse(birth_all_loc_missing & birth_ring_year_match, ring_lat, NestLat)
  ) %>%
  ungroup() %>%
  # drop helper columns if you don't want them
  select(-ring_year, -ring_nest, -ring_lon, -ring_lat,
         -birth_all_loc_missing, -birth_ring_year_match)






## Add distances travelled -------------------------------------------------

# Ensure numeric
location_data <- location_data %>%
  mutate(
    NestLat = as.numeric(NestLat),
    NestLon = as.numeric(NestLon)
  )

# Distance from previous nest
location_data <- location_data %>%
  filter(Event %in% c("birth", "nest")) %>% 
  arrange(RingNumber, Year, Month) %>%
  group_by(RingNumber) %>%
  mutate(
    # Previous nest coordinates
    PrevLon = lag(NestLon),
    PrevLat = lag(NestLat),
    # Distance from previous point in meters
    DistanceFromPrev_m = ifelse(
      is.na(PrevLon) | is.na(PrevLat), 
      NA,  # NA if no previous point
      distHaversine(cbind(PrevLon, PrevLat), cbind(NestLon, NestLat))
    )
  ) %>%
  ungroup()


## Save location_data ------------------------------------------------------
head(location_data)


write.csv(location_data, file = "data/location_data.csv", row.names = FALSE)





# Individual info ---------------------------------------------------------

# Add individual info to IndividualDataVlieland


## --- Number of nest sites used (distinct nest boxes from nest events) ------
nest_sites <- location_data %>%
  filter(Event == "nest" & !is.na(NestNo)) %>%
  group_by(RingNumber) %>%
  summarise(
    NumNestSites = n_distinct(NestNo),
    .groups = "drop"
  )


## --- Number of breeding attempts ------
breeding_attempts <- BroodData %>%
  tidyr::pivot_longer(cols = c(RingNumberFemale, RingNumberMale),
                      names_to = "ParentSex", values_to = "RingNumber") %>%
  filter(!is.na(RingNumber)) %>%
  group_by(RingNumber) %>%
  summarise(
    BreedingAttempts = n(),
    .groups = "drop"
  )

## --- First & last breeding years, breeding span ------
breeding_years <- BroodData %>%
  tidyr::pivot_longer(cols = c(RingNumberFemale, RingNumberMale),
                      names_to = "ParentSex", values_to = "RingNumber") %>%
  filter(!is.na(RingNumber)) %>%
  group_by(RingNumber) %>%
  summarise(
    FirstBreedingYear = min(BroodYear),
    LastBreedingYear  = max(BroodYear),
    BreedingSpan      = LastBreedingYear - FirstBreedingYear,
    .groups = "drop"
  )

##  --- Mean clutch size, range, total eggs produced ------
clutch_stats <- BroodData %>%
  tidyr::pivot_longer(
    cols = c(RingNumberFemale, RingNumberMale),
    names_to = "ParentSex", values_to = "RingNumber"
  ) %>%
  filter(!is.na(RingNumber)) %>%
  group_by(RingNumber) %>%
  summarise(
    MeanClutchSize = if (all(is.na(ClutchSize))) NA_real_ else mean(ClutchSize, na.rm = TRUE),
    MinClutchSize  = if (all(is.na(ClutchSize))) NA_real_ else min(ClutchSize, na.rm = TRUE),
    MaxClutchSize  = if (all(is.na(ClutchSize))) NA_real_ else max(ClutchSize, na.rm = TRUE),
    TotalEggs      = if (all(is.na(ClutchSize))) NA_real_ else sum(ClutchSize, na.rm = TRUE),
    .groups = "drop"
  )


## --- Is mother colour-ringed? ------
is_mother_color <- IndividualDataVlieland %>%
  left_join(
    IndividualDataVlieland %>%
      select(Mother = RingNumber, MotherRingColour = RingColour),
    by = c("Mother" = "Mother")
  ) %>%
  select(RingNumber, MotherRingColour)

## --- Is father colour-ringed? ------
is_father_color <- IndividualDataVlieland %>%
  left_join(
    IndividualDataVlieland %>%
      select(Father = RingNumber, FatherRingColour = RingColour),
    by = c("Father" = "Father")
  ) %>%
  select(RingNumber, FatherRingColour)


## ---  Dispersal distance: birth → first nest ------
dispersal <- location_data %>%
  filter(Event %in% c("birth", "nest")) %>%
  arrange(RingNumber, Year, Month) %>%
  group_by(RingNumber) %>%
  summarise(
    BirthLon = NestLon[Event == "birth"][1],
    BirthLat = NestLat[Event == "birth"][1],
    FirstNestLon = NestLon[Event == "nest"][1],
    FirstNestLat = NestLat[Event == "nest"][1],
    .groups = "drop"
  ) %>%
  mutate(
    DispersalDistance_m = ifelse(
      !is.na(BirthLon) & !is.na(FirstNestLon),
      distHaversine(cbind(BirthLon, BirthLat), cbind(FirstNestLon, FirstNestLat)),
      NA
    )
  ) %>%
  select(RingNumber, DispersalDistance_m)

## --- Total distance travelled --------
total_distance <- location_data %>%
  filter(Event %in% c("birth", "nest")) %>%
  arrange(RingNumber, Year, Month) %>%
  group_by(RingNumber) %>%
  mutate(
    PrevLon = lag(NestLon),
    PrevLat = lag(NestLat),
    DistanceFromPrev_m = ifelse(
      is.na(PrevLon) | is.na(PrevLat),
      0,  # 0 for first move; we'll handle single locations below
      distHaversine(cbind(PrevLon, PrevLat), cbind(NestLon, NestLat))
    )
  ) %>%
  summarise(
    TotalDistance_m = if(n() > 1) sum(DistanceFromPrev_m, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )


## Combine into one df ------
IndividualInfo <- IndividualDataVlieland %>%
  left_join(nest_sites,        by = "RingNumber") %>%
  left_join(breeding_attempts, by = "RingNumber") %>%
  left_join(breeding_years,    by = "RingNumber") %>%
  left_join(clutch_stats,      by = "RingNumber") %>%
  left_join(is_mother_color,   by = "RingNumber") %>%
  left_join(is_father_color,   by = "RingNumber") %>%
  left_join(dispersal, by = "RingNumber") %>%
  left_join(total_distance, by = "RingNumber")





## Add sex_text ------
IndividualInfo <- IndividualInfo %>%
  mutate(SexText = case_when(
    Sex == 1 ~ "Female",
    Sex == 2 ~ "Male",
    Sex == 0 ~ "Unknown",
    TRUE     ~ "Unknown"  # any other number
  ))



# Save individual info df -------------------------------------------------
# Save
write.csv(IndividualInfo, file = "data/IndividualInfo.csv", row.names = FALSE)
head(as.data.frame(IndividualInfo))
head(as.data.frame(IndividualInfo[!is.na(IndividualInfo$RingColour), ]))

