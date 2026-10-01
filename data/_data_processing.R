
# ----------------------------------------------------------------------
# Data Processing Script for Vlieland App
# ----------------------------------------------------------------------

# Create dfs analagous to dummy datasets that I built the modules with
# But there is probably a simpler way

# Load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(lubridate)
library(geosphere) # for distances
library(magrittr) # for all pipes


# Load real data ----------------------------------------------------------

## --- Individual data ----
# Read in data, "UTF-8" allows correct handling of special characters 
IndividualData.original <- read.csv("data/IndividualData.csv", row.names = NULL, fileEncoding="UTF-8")
IndividualData <- IndividualData.original
colnames(IndividualData)
head(IndividualData)

# Are there duplicate rows?
length(which(duplicated(IndividualData)))
nrow(IndividualData)
# Duplicate row removal
IndividualData <- unique(IndividualData)


### Tidy & cut to Vlieland birds --------------------------------------------

# performing this tidy here so doesn't have to be repeated when this data is used to tidy brood data

# Removal of birds that have neither been ringed nor parented on Vlieland
# These birds are unlikely ever to be present on Vlieland
# And some have been colour ringed using different systems

# List of birds ringed or found parenting on Vlieland
# Migrant parents metal ringed elsewhere have that as their RingAreaGroupName
# hence taking parental ring list from BroodData as well
Vlieland_rings <- c(BroodData$RingNumberFemale, BroodData$RingNumberMale,
                    IndividualData$RingNumber
                    [IndividualData$RingAreaGroupName == "Vlieland"]) %>%
  na.omit() %>%
  unique()

# Any parents who don't feature in IndividualData?
# Will leave them for now and see if they cause issues
Vlieland_rings[which(!Vlieland_rings %in% IndividualData$RingNumber)]

# Subset IndividualData to the Vlieland birds
IndividualData <- IndividualData[which(IndividualData$RingNumber %in% Vlieland_rings),]

# Find ring number typos in IndividualData
# ie >1 bird per ring number
# shouldn't be duplicates yet as colour codes not yet added (so is ok to remove them)
TyposIndividual <- sort(IndividualData$RingNumber[which(duplicated(IndividualData$RingNumber))])
# And remove rows in the typos list
IndividualData <- IndividualData[which(!IndividualData$RingNumber %in% TyposIndividual),]

# remove objects only used for this section
rm(Vlieland_rings, TyposIndividual)


## --- Brood data ----
BroodData.original <- read.csv("data/BroodData.csv", row.names = NULL, fileEncoding="UTF-8")
BroodData <- BroodData.original
head(BroodData)
# Check all have sensible lay dates
# Year
range(lubridate::year(BroodData$LayDate),na.rm=T)
# Month
range(lubridate::month(BroodData$LayDate),na.rm=T)
# Day
range(lubridate::day(BroodData$LayDate),na.rm=T)

# Are there duplicate rows?
length(which(duplicated(BroodData)))
nrow(BroodData)
# No duplicate rows


### --- Remove birds with sex mismatches ----
# Find birds registered as both mother & father
overlap <- unique(BroodData$RingNumberFemale[which(BroodData$RingNumberFemale %in% BroodData$RingNumberMale, )])
overlap <- overlap[!is.na(overlap)]

# Replace with NA
# In mother column
BroodData$RingNumberFemale[which(BroodData$RingNumberFemale %in% overlap)] <- NA
# In Father column
BroodData$RingNumberMale[which(BroodData$RingNumberMale %in% overlap)] <- NA

# Find & remove birds with sexes mismatched to parent status
# Male mothers
Males <- unique(IndividualData$RingNumber[IndividualData$Sex == 2])
Males <- Males[!is.na(Males)]
Male_ma <- Males[which(Males %in% BroodData$RingNumberFemale)]
# Remove male mothers
BroodData$RingNumberFemale[which(BroodData$RingNumberFemale %in% Male_ma)] <- NA

# Female fathers
Females <- unique(IndividualData$RingNumber[IndividualData$Sex == 1])
Females <- Females[!is.na(Females)]
Female_pa <- Females[which(Females %in% BroodData$RingNumberMale)]
# Remove male mothers
BroodData$RingNumberMale[which(BroodData$RingNumberMale %in% Female_pa)] <- NA


### --- Rearrange & save ----
names(BroodData)
# Arrange cols to match previous data frame layout
BroodDataApp <- subset(BroodData,
          select = c("BroodID", "BroodYear", "RingNumberFemale", "RingNumberMale",
                     "LayDate", "ClutchSize", "BroodNestBox",
                     "BroodLatitude", "BroodLongitude", "BroodAreaGroupName", "SpeciesName"))
# Re-name cols to match previous data frame layout
colnames(BroodDataApp) <- c("ID", "BroodYear", "RingNumberFemale", "RingNumberMale",
                                 "LayDate", "ClutchSize", "BroodNestBox",
                                 "BroodLatitude", "BroodLongitude",
                                 "BroodPopulationName", "SpeciesName")
# write csv
write.csv(BroodDataApp, file = "data/BroodDataApp.csv", row.names = FALSE)

rm(overlap, Males, Male_ma, Females, Female_pa)



## --- ColourNumberRings ----
ColourNumberRings.original <- read.csv("data/ColourNumberRings.csv", row.names = NULL, fileEncoding="UTF-8")
ColourNumberRings <- ColourNumberRings.original
head(ColourNumberRings)

# Are there duplicate rows?
length(which(duplicated(ColourNumberRings)))
nrow(ColourNumberRings)
# Duplicate row removal
ColourNumberRings <- unique(ColourNumberRings)


# Exploring data -----------------------------------------------------------

## --- BroodID overlap ----
# All BroodIDs need not be in both data sets
# IndividualData may have a couple more due to BroodData being cutoff at the previous calendar year
# BroodData may have others IndividualData lacks due to factors including:
#     - Broods with a clutch size of 0 still have a broodID
#     - All eggs in a brood not hatching
#     - All chicks in a brood not making it to ringing
# However if most broodIDs are not in both data sets there is likely an issue.

# Is there a similar number of BroodIDs in both data sets?
# Number of unique BroodIDs in IndividualData
length(unique(IndividualData$RingBroodID))
# Number of unique BroodIDs in BroodData
length(unique(BroodData$BroodID))
# Rows of IndividualData where RingBroodID appears in BroodData
matches <- IndividualData[which(IndividualData$RingBroodID %in% BroodData$BroodID), ]
# Number of unique broodIDs from IndividualData found in BroodData
length(unique(matches$RingBroodID))

# Exploring the BroodIDs in IndividualData but NOT BroodData
# Rows of IndividualData where RingBroodID DOES NOT appear in BroodData
missing <- IndividualData %>%
  filter(!RingBroodID %in% BroodData$BroodID)
# View the BroodIDs only in IndividualData
unique(missing$RingBroodID)
# Are they this year's chicks (BroodData may have been cut off at the previous calendar year)?
IndividualData[which(IndividualData$RingYear==2026),] %$%
  {unique(RingBroodID)} %>%
  {length(unique(c(. , unique(missing$RingBroodID))))}


## --- Ring number overlap ----
# Extract RingNumbers from both dataframes
individual_rings <- IndividualData.original$RingNumber
brood_rings <- c(BroodData$RingNumberFemale, BroodData$RingNumberMale)

# Find which individual rings appear in BroodData
rings_in_broods <- intersect(individual_rings, brood_rings)

rings_in_broods
length(rings_in_broods)  # number of rings that appear
length(unique(brood_rings))


## --- Remove objects created for testing ----
rm(brood_rings, individual_rings, rings_in_broods)
rm(matches, missing)

# Data prep ---------------------------------------------------------------


## Individual data ---------------------------------------------------------

### --- Remove birds with sex mismatches ----
# Find birds registered as both mother & father
overlap <- unique(IndividualData$Mother[which(IndividualData$Mother %in% IndividualData$Father, )])
overlap <- overlap[!is.na(overlap)]

# Replace with NA
# In mother column
IndividualData$Mother[which(IndividualData$Mother %in% overlap)] <- NA
# In Father column
IndividualData$Father[which(IndividualData$Father %in% overlap)] <- NA

# Find & remove birds with sexes mismatched to parent status
# Male mothers
Males <- unique(IndividualData$RingNumber[IndividualData$Sex == 2])
Males <- Males[!is.na(Males)]
Male_ma <- Males[which(Males %in% IndividualData$Mother)]
# Remove male mothers
IndividualData$Mother[which(IndividualData$Mother %in% Male_ma)] <- NA

# Female fathers
Females <- unique(IndividualData$RingNumber[IndividualData$Sex == 1])
Females <- Females[!is.na(Females)]
Female_pa <- Females[which(Females %in% IndividualData$Father)]
# Remove male mothers
IndividualData$Father[which(IndividualData$Father %in% Female_pa)] <- NA

rm(overlap, Males, Male_ma, Females, Female_pa)

### Add colour ring combination --------------------------------------------

# Add colour codes to IndividualData from ColourNumberRings
IndividualData <- merge(
  IndividualData,
  ColourNumberRings,
  by = "RingNumber",
  all.x = TRUE   # keep all rows from IndividualData
)

# Cleaning the codes to something process-able
# Ensure all are lowercase
IndividualData$ColourCode <- tolower(IndividualData$ColourCode) %>%
  # Reduce any multispaces back to single space
  {gsub("\\s{2,}", " ", . )}

# Keep only rows where the colour code is 13 characters long or less.
IndividualData <- IndividualData[which(nchar(IndividualData$ColourCode) <= 13 | is.na(IndividualData$ColourCode)), ]


# Split into separate cols for each ring colour

# Define code-to-colour mapping
colour_map <- c(
  bl = "blue", bw = "blue/white", gr = "green", al = "metal", or = "orange",
  pb = "pink/blue", pg = "pink/green", re = "red", rw = "red/white",
  wh = "white", wb = "white/blue", ye = "yellow", yb = "yellow/black", pi = "pink",
  gw = "green/white", tr = NA
)


# Function to parse a ring combo
parse_ring <- function(x) {
  # If the bird's colour ring combo is NA make the split version NA
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
IndividualData <- IndividualData %>%
  rowwise() %>%
  mutate(tmp = list(parse_ring(ColourCode))) %>%
  unnest_wider(tmp) %>%
  ungroup()



# Add extra column with colour ring combo decoded into full English using colour map
IndividualData <- IndividualData %>%
mutate(
  ColourRingCombo = pmap_chr(
    list(ColourRingLeft1, ColourRingLeft2, ColourRingRight1, ColourRingRight2),
    ~ paste(na.omit(c(...)), collapse = "-")
  )
)
# And enter NA in every row where the bird is not color ringed
IndividualData$ColourRingCombo[which(IndividualData$ColourRingCombo == "")] <- NA

### Add columns for date filter ---------------------------------------------
# When searching for a bird
# these columns allow birds unlikely to be alive to be filtered out

# Earliest possible date
IndividualData$Colour_EarliestStart <- ifelse(!is.na(IndividualData$BirthYear),
                                            yes = IndividualData$BirthYear + 1,
                                            no = ifelse(!is.na(IndividualData$RingYear),
                                                        yes = IndividualData$RingYear,
                                                        no = NA)
                                            )


# Latest likely date
# Oldest birds according to EURING_longevity_list_20230901.pdf
# Blue tit 16 years 7 months
# Great tit (also) 16 years 7 months!
# So I'll allow possibility of living up to 17 to give the benefit of the doubt!
IndividualData$Colour_LatestLikely <- ifelse(!is.na(IndividualData$Colour_EarliestStart),
                                             yes = IndividualData$Colour_EarliestStart + 16,
                                             no = NA)


### Rename columns ----------------------------------------------------------

colnames(IndividualData)
# Arrange cols to match previous data frame layout
IndividualDataVlieland <- subset(IndividualData,
                         select = c("RingNumber", "Mother", "Father",
                                    "RingBroodID", "Sex", "RingYear", "BirthYear",
                                    "RingAreaGroupName", "RingNestBox",
                                    "RingLatitude", "RingLongitude", "SpeciesName",
                                    "ColourRingLeft1", "ColourRingLeft2",
                                    "ColourRingRight1", "ColourRingRight2", "ColourRingCombo",
                                    "Colour_EarliestStart", "Colour_LatestLikely"))
# Re-name cols to match previous data frame layout
colnames(IndividualDataVlieland) <- c("RingNumber", "Mother", "Father",
                              "BroodID", "Sex", "RingYear", "BirthYear",
                              "RingPopulationName", "RingNestBox",
                              "RingLatitude", "RingLongitude", "Species",
                              "ColourRingLeft1", "ColourRingLeft2",
                              "ColourRingRight1", "ColourRingRight2", "ColourRingCombo",
                              "Colour_EarliestStart", "Colour_LatestLikely")




### Save IndividualData ---------------------------------------------

# Save
write.csv(IndividualDataVlieland, file = "data/IndividualDataVlieland.csv", row.names = FALSE)

# remove objects only needed for this step:
rm(colour_map, parse_ring)


## Location data -----------------------------------------------------

# Create location dataframe that will work with mapping modules
# Info on individuals birth, ringing & breeding locations are collected into data frames
# These data frames are then complied into one single long data frame
# The event column denotes if the row relates to birth, ringing or reproduction

### LayDate as date ---------------------------------------------------------
# Is LayDate stored as a date?
is.Date(BroodData$LayDate)
# Ensure dates are Date objects
BroodData <- BroodData %>%
  mutate(LayDate = as.Date(LayDate))


### Get ring events ---------------------------------------------------------

ring_events <- IndividualData %>%
  mutate(
    RingNumber = RingNumber,
    Event = "ring",
    Month = NA_character_,  # no month info for ring event
    Year = RingYear,
    NestNo = RingNestBox,
    NestLon = RingLongitude,
    NestLat = RingLatitude,
    LayDate = NA,
    ClutchSize = NA,
    .keep = "none"
  )


### Get birth events --------------------------------------------------------

# Join IndividualData with BroodData using RingBroodID
birth_events <- IndividualData %>%
  left_join(BroodData, by = c("RingBroodID" = "BroodID")) %>%
  mutate(
    RingNumber = RingNumber,
    Event = "birth",
    Month = month(LayDate, label = TRUE, abbr = TRUE),
    Year = BirthYear,
    NestNo = BroodNestBox,
    NestLon = RingLongitude,
    NestLat = RingLatitude,
    LayDate = as.Date(LayDate),  
    ClutchSize = ClutchSize ,
    .keep = "none"
  )



### Get nesting events ------------------------------------------------------

# Female reproduction
female_nests <- BroodData %>%
  filter(!is.na(RingNumberFemale)) %>%
  mutate(
    RingNumber = RingNumberFemale,
    Event = "nest",
    Month = month(LayDate, label = TRUE, abbr = TRUE),
    Year = BroodYear,
    NestNo = BroodNestBox,
    NestLon = BroodLongitude,
    NestLat = BroodLatitude,
    LayDate = LayDate,
    ClutchSize = ClutchSize,
    .keep = "none"
  )

# Male reproduction
male_nests <- BroodData %>%
  filter(!is.na(RingNumberMale)) %>%
  mutate(
    RingNumber = RingNumberMale,
    Event = "nest",
    Month = month(LayDate, label = TRUE, abbr = TRUE),
    Year = BroodYear,
    NestNo = BroodNestBox,
    NestLon = BroodLongitude,
    NestLat = BroodLatitude,
    LayDate = LayDate,
    ClutchSize = ClutchSize,
    .keep = "none"
  )

reproduction_events <- bind_rows(female_nests, male_nests)



### Combine ring, birth and nest events --------------------------------------
# Make sure relevant columns are character/numeric
ring_events <- ring_events %>%
  mutate(
    NestNo = as.numeric(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )
birth_events <- birth_events %>%
  mutate(
    NestNo = as.numeric(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )
reproduction_events <- reproduction_events %>%
  mutate(
    NestNo = as.numeric(NestNo),
    NestLon = as.numeric(NestLon),
    NestLat = as.numeric(NestLat)
  )

location_data <- bind_rows(ring_events, birth_events, reproduction_events) %>%
  arrange(RingNumber, Year, Month)


# Do most ring numbers appear in IndividualData & location_data?
length(unique(IndividualData$RingNumber))
length(unique(location_data$RingNumber))


### Add distances travelled -------------------------------------------------

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


### Save location_data ------------------------------------------------------

names(location_data)

# Select columns
location_data_trim <- subset(location_data,
                            select = c("RingNumber", "Event", "Month", "Year",
                                       "NestLon", "NestLat"))

# And save
write.csv(location_data_trim, file = "data/location_data.csv", row.names = FALSE)

# remove objects only needed for this step:
rm(birth_events, ring_events, female_nests, male_nests, reproduction_events)



## Individual info ---------------------------------------------------------

# Add individual info to IndividualData


### --- Number of nest sites used (distinct nest boxes from nest events) ------
nest_sites <- location_data %>%
  filter(Event == "nest" & !is.na(NestNo)) %>%
  group_by(RingNumber) %>%
  summarise(
    NumNestSites = n_distinct(NestNo),
    .groups = "drop"
  )

### --- Number of breeding attempts ------
breeding_attempts <- BroodData %>%
  tidyr::pivot_longer(cols = c(RingNumberFemale, RingNumberMale),
                      names_to = "ParentSex", values_to = "RingNumber") %>%
  filter(!is.na(RingNumber)) %>%
  group_by(RingNumber) %>%
  summarise(
    BreedingAttempts = n(),
    .groups = "drop"
  )

### --- First & last breeding years, breeding span ------
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

###  --- Mean clutch size, range, total eggs produced ------
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

# Round mean clutch size
clutch_stats$MeanClutchSize <- round(clutch_stats$MeanClutchSize, 1)


# Replace min & max with clutch size range
clutch_stats <- clutch_stats %>%
  rowwise() %>%
  mutate(
    ClutchSizeRange = ifelse(
      is.na(MinClutchSize) | is.na(MaxClutchSize),
      "Unknown",
      paste0(MinClutchSize, "–", MaxClutchSize)
    )
  ) %>%
  ungroup() %>%
  select(-MinClutchSize, -MaxClutchSize)


### ---  Dispersal distance: birth → first nest ------
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
  select(RingNumber, DispersalDistance_m) %>%
  mutate(
    DispersalDistance_m = round(DispersalDistance_m))

### --- Total distance travelled --------
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
  ) %>%
  mutate(
    TotalDistance_m = round(TotalDistance_m))


### Combine into one df ------
IndividualInfo <- IndividualData %>%
  left_join(nest_sites,        by = "RingNumber") %>%
  left_join(breeding_attempts, by = "RingNumber") %>%
  left_join(breeding_years,    by = "RingNumber") %>%
  left_join(clutch_stats,      by = "RingNumber") %>%
  left_join(dispersal, by = "RingNumber") %>%
  left_join(total_distance, by = "RingNumber")

### Add sex_text ------
IndividualInfo <- IndividualInfo %>%
  mutate(SexText = case_when(
    Sex == 1 ~ "Female",
    Sex == 2 ~ "Male",
    Sex == 0 ~ "Unknown",
    TRUE     ~ "Unknown"  # any other number
  ))


### Rename columns ----------------------------------------------------------

colnames(IndividualInfo)

# Arrange cols to match previous data frame layout
IndividualInfo <- subset(IndividualInfo,
                                 select = c("RingNumber", "ColourRingCombo", "SpeciesName",
                                            "SexText", "BirthYear", "RingYear",
                                            "Mother", "Father",
                                            "BreedingAttempts", "NumNestSites", 
                                            "FirstBreedingYear", "LastBreedingYear",
                                            "MeanClutchSize", "ClutchSizeRange", "TotalEggs",
                                            "DispersalDistance_m", "TotalDistance_m"))
# Re-name cols to match previous data frame layout
colnames(IndividualInfo) <- c("RingNumber", "ColourRingCombo", "Species",
                              "SexText", "BirthYear", "RingYear",
                              "Mother", "Father",
                              "BreedingAttempts", "NumNestSites",
                              "FirstBreedingYear", "LastBreedingYear",
                              "MeanClutchSize", "ClutchSizeRange", "TotalEggs",
                              "DispersalDistance_m", "TotalDistance_m")




### Save individual info df -------------------------------------------------

# Save
write.csv(IndividualInfo, file = "data/IndividualInfo.csv", row.names = FALSE)

# remove objects only needed for this step:
rm(nest_sites, breeding_attempts, breeding_years, clutch_stats, dispersal,
   total_distance)

