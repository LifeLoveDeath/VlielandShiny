
# ----------------------------------------------------------------------
# Individual Info Table Module - functions
# ----------------------------------------------------------------------



# Format numeric distances with meters ------------------------------------------------

format_distance <- function(x) {
  ifelse(!is.na(x), paste0(round(x), " meters"), "Unknown")
}



# Format colour ring structure --------------------------------------------

# Format colour ring string
format_colour_rings <- function(x) {
  gsub("-", ", ", x)
}



# Format individual info table  -------------------------------------------

# long format, headers, tooltips

prepare_individual_data <- function(df) {
  
  # Round & create clutch size range
  df <- df %>%
    rowwise() %>%
    mutate(
      ClutchSizeRange = ifelse(
        is.na(MinClutchSize) | is.na(MaxClutchSize),
        "Unknown",
        paste0(MinClutchSize, "–", MaxClutchSize)
      )
    ) %>%
    ungroup() %>%
    mutate(
      DispersalDistance_m = round(DispersalDistance_m),
      TotalDistance_m = round(TotalDistance_m)
    ) %>%
    select(-MinClutchSize, -MaxClutchSize)
  
  # Keep desired columns
  df <- df[, c(
    "RingNumber", "ColourRingCombo", "Species", "SexText",
    "BirthYear", "RingYear",
    "MotherRingColour", "FatherRingColour",
    "BreedingAttempts", "NumNestSites", "FirstBreedingYear", "LastBreedingYear",
    "MeanClutchSize", "ClutchSizeRange", "TotalEggs",
    "DispersalDistance_m", "TotalDistance_m"
  )]
  
  colnames(df) <- c(
    "Ring Number", "Colour rings", "Species", "Sex",
    "Birth year", "Ring year",
    "Mother", "Father",
    "Breeding attempts", "Number of nest sites", "First breeding year", "Last breeding year",
    "Mean clutch size", "Clutch size range", "Total eggs",
    "Dispersal distance", "Total distance travelled"
  )
  
  # Convert to long format
  long <- data.frame(
    Variable = names(df),
    Value = as.character(t(df)),
    stringsAsFactors = FALSE
  )
  
  # Format values
  long$Value[long$Variable == "Colour rings"] <- format_colour_rings(long$Value[long$Variable == "Colour rings"])
  long$Value[long$Variable == "Dispersal distance"] <- format_distance(as.numeric(long$Value[long$Variable == "Dispersal distance"]))
  long$Value[long$Variable == "Total distance travelled"] <- format_distance(as.numeric(long$Value[long$Variable == "Total distance travelled"]))
  
  # Replace missing with "Unknown"
  long$Value <- ifelse(is.na(long$Value) | long$Value == "", "Unknown", long$Value)
  
  # Sections
  identity_header <- data.frame(Variable = "Ring number", Value = long$Value[long$Variable == "Ring Number"], stringsAsFactors = FALSE)
  life_history <- long[long$Variable %in% c("Birth year", "Ring year", "Mother", "Father"), ]
  general <- long[long$Variable %in% c("Colour rings","Species","Sex"), ]
  reproduction <- long[long$Variable %in% c("Breeding attempts", "Number of nest sites", "First breeding year", "Last breeding year", "Mean clutch size", "Clutch size range", "Total eggs"), ]
  dispersal <- long[long$Variable %in% c("Dispersal distance", "Total distance travelled"), ]
  
  final_long <- rbind(
    identity_header,
    general,
    data.frame(Variable = "Early life", Value = "", stringsAsFactors = FALSE),
    life_history,
    data.frame(Variable = "Reproduction", Value = "", stringsAsFactors = FALSE),
    reproduction,
    data.frame(Variable = "Dispersal", Value = "", stringsAsFactors = FALSE),
    dispersal
  )
  
  # Add tooltips
  final_long <- final_long %>%
    mutate(
      Variable = case_when(
        Variable == "Birth year" ~ '<span title="Year the bird hatched">Birth year</span>',
        Variable == "Ring year" ~ '<span title="Year the bird was ringed for identification">Ring year</span>',
        Variable == "Breeding attempts" ~ '<span title="Number of breeding attempts recorded for this bird">Breeding attempts</span>',
        Variable == "Number of nest sites" ~ '<span title="Total distinct nest sites used by this bird">Number of nest sites</span>',
        Variable == "First breeding year" ~ '<span title="Year of the bird’s first recorded breeding attempt">First breeding year</span>',
        Variable == "Last breeding year" ~ '<span title="Most recent year the bird was recorded breeding">Last breeding year</span>',
        Variable == "Mean clutch size" ~ '<span title="Average number of eggs per breeding attempt">Mean clutch size</span>',
        Variable == "Clutch size range" ~ '<span title="Smallest to largest clutch sizes recorded">Clutch size range</span>',
        Variable == "Dispersal distance" ~ '<span title="Distance from the bird’s birth nest to its first breeding site">Dispersal distance</span>',
        Variable == "Total distance travelled" ~ '<span title="Cumulative distance between all known nest sites">Total distance travelled</span>',
        TRUE ~ Variable
      )
    )
  
  final_long
}

