

# Load dummy data
vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
colnames(vlieland.data)
head(vlieland.data)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

# Load real data
IndividualData <- read.csv("data/IndividualData.csv", row.names = NULL)
colnames(IndividualData)
head(IndividualData)
BroodData <- read.csv("data/BroodData.csv", row.names = NULL)
ColourNumberRings <- read.csv("data/ColourNumberRings.csv", row.names = NULL)
head(ColourNumberRings)
IndividualDataVlieland <- IndividualData[which(IndividualData$RingPopulationName == "Vlieland"), ]


# Rows of IndividualData where BroodID appears in BroodData
matches <- IndividualDataVlieland[which(IndividualDataVlieland$BroodID %in% BroodData$ID), ]


missing <- IndividualDataVlieland %>%
  filter(!BroodID %in% BroodData$ID)



# Data prep

# Add ring number to IndividualData
# Just Vlieland data
IndividualDataVlieland <- IndividualData[which(IndividualData$RingPopulationName == "Vlieland"), ]
# Ensure both columns are UTF-8
IndividualDataVlieland <- IndividualDataVlieland %>%
  mutate(RingNumber = iconv(RingNumber, from = "", to = "UTF-8"))

ColourNumberRings <- ColourNumberRings %>%
  mutate(RingNumber = iconv(RingNumber, from = "", to = "UTF-8"))

IndividualDataVlieland <- merge(
  IndividualDataVlieland,
  ColourNumberRings,
  by = "RingNumber",
  all.x = TRUE   # keep all rows from IndividualData
)


# Add individual colour ring cols to IndividualData

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


colnames(IndividualDataVlieland)
# Change Species column name
IndividualDataVlieland <- IndividualDataVlieland %>%
  rename(Species = SpeciesName)

# Add colour ring combo column:IndividualDataVlieland <- IndividualDataVlieland %>%
IndividualDataVlieland <- IndividualDataVlieland %>%
  mutate(
    ColourRingCombo = pmap_chr(
      list(ColourRingLeft1, ColourRingLeft2, ColourRingRight1, ColourRingRight2),
      ~ paste(na.omit(c(...)), collapse = "-")
    )
  )




# Save
write.csv(IndividualDataVlieland, file = "data/IndividualDataVlieland.csv", row.names = FALSE)

