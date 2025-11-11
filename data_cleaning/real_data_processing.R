
# Processing data to work with app

# Load dummy data
vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
colnames(vlieland.data)
head(vlieland.data)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

# Load real data
IndividualData <- read.csv("data/IndividualData.csv", row.names = NULL)
colnames(IndividualData)
head(IndividualData)
# Reduce to just Vlieland
IndividualDataVlieland <- IndividualData[which(IndividualData$RingPopulationName == "Vlieland"), ]


BroodData <- read.csv("data/BroodData.csv", row.names = NULL)

ColourNumberRings <- read.csv("data/ColourNumberRings.csv", row.names = NULL)
head(ColourNumberRings)



# Rows of IndividualData where BroodID appears in BroodData
matches <- IndividualDataVlieland[which(IndividualDataVlieland$BroodID %in% BroodData$ID), ]


missing <- IndividualDataVlieland %>%
  filter(!BroodID %in% BroodData$ID)





# Data prep ---------------------------------------------------------------


## Individual data ---------------------------------------------------------


### Add ring number ---------------------------------------------------------

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


### Rename columns ----------------------------------------------------------

colnames(IndividualDataVlieland)
# Change Species column name
IndividualDataVlieland <- IndividualDataVlieland %>%
  rename(Species = SpeciesName)





### Save IndividualDataVlieland ---------------------------------------------

# Save
write.csv(IndividualDataVlieland, file = "data/IndividualDataVlieland.csv", row.names = FALSE)

