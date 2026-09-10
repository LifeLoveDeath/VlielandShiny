
# ----------------------------------------------------------------------
# Family Tree Module - functions
# ----------------------------------------------------------------------



# Subset individual's family data -----------------------------------------

#   Subsets the pedigree dataframe for a focal individual.
#   Adds parents, siblings (full & half), children, and placeholder parents if missing.
#   Adds kinship2-compatible sex columns, focal flag, recruit flag, and sibling type.
# ----------------------------------------------------------------------
get_family_subset <- function(ped.data, focal_id, brood_data) {
  
  # Ensure no duplicate ring numbers
  ped.data <- subset(ped.data,
                     select = c("RingNumber", "Mother", "Father",
                                "BroodID", "Sex", "RingYear", "BirthYear",
                                "RingPopulationName", "RingNestBox",
                                "RingLatitude", "RingLongitude",
                                "Species")) %>% unique()
  
  # Step 1: Find parents
  
  focal_row <- ped.data %>% filter(RingNumber == focal_id)
  focal <- ped.data %>% filter(RingNumber == focal_id)
  brood <- focal_row$BroodID
  mother <- focal_row$Mother
  father <- focal_row$Father
  
  
  # Step 2: Find children
  
  children <- ped.data %>% filter(Mother == focal_id | Father == focal_id) %>% pull(RingNumber)
  
  
  # Step 3: Find siblings
  
  # All siblings - share at least one parent = too many
  siblings <- ped.data %>%
    filter(
      ( !is.na(Mother) & Mother %in% mother ) |
        ( !is.na(Father) & Father %in% father )
    ) %>%
    pull(RingNumber)
  
  # Full siblings only (same BroodID)
  #  siblings <- ped.data %>% 
  #    filter(BroodID == brood & RingNumber != focal_id)
  # siblings <- siblings$RingNumber
  
  
  # Step 4: Combine IDs and subset
  
  ids <- unique(c(focal_id, mother, father, children, siblings))
  fam.data <- ped.data %>% filter(RingNumber %in% ids)
  
  
  # Step 5: Add missing parents as unique placeholders
  
  all_parents <- unique(c(fam.data$Mother, fam.data$Father))
  missing_parents <- setdiff(all_parents, fam.data$RingNumber)
  missing_parents <- missing_parents[!is.na(missing_parents)]
  
  if(length(missing_parents) > 0){
    # Determine Sex of each missing parent
    sex_vals <- sapply(missing_parents, function(id){
      if(id %in% fam.data$Father) return(2)    # male
      if(id %in% fam.data$Mother) return(1)    # female
      return(0)                                # unknown (should not occur)
    })
    
    # Build missing parent rows
    missing_rows <- data.frame(
      RingNumber        = missing_parents,
      Mother            = NA_character_,
      Father            = NA_character_,
      BroodID           = NA_integer_,
      Sex               = as.integer(sex_vals),
      RingYear          = NA_integer_,
      BirthYear         = NA_integer_,
      RingPopulationName= NA_character_,
      RingNestBox       = NA_integer_,
      RingLatitude      = NA_real_,
      RingLongitude     = NA_real_,
      Species           = NA_character_,
      RingColour        = NA_character_,
      ColourRingLeft1   = NA_character_,
      ColourRingLeft2   = NA_character_,
      ColourRingRight1  = NA_character_,
      ColourRingRight2  = NA_character_,
      ColourRingCombo   = NA_character_,
      stringsAsFactors  = FALSE
    )
    
    fam.data <- bind_rows(fam.data, missing_rows)
  }
  
  
  # Step 6: Fill any remaining NA parents with unique IDs
  
  # Step 6: Fill NA parents with unique per-brood IDs
  fam.data <- fam.data %>%
    group_by(BroodID) %>%
    mutate(
      Mother = ifelse(is.na(Mother) & !is.na(BroodID), paste0("UnknownMother_", BroodID), Mother),
      Father = ifelse(is.na(Father) & !is.na(BroodID), paste0("UnknownFather_", BroodID), Father)
    ) %>%
    ungroup()
  
  # Add rows for these unknown parents (one per unique placeholder)
  missing_mothers <- setdiff(unique(fam.data$Mother[grepl("^UnknownMother_", fam.data$Mother)]), fam.data$RingNumber)
  missing_fathers <- setdiff(unique(fam.data$Father[grepl("^UnknownFather_", fam.data$Father)]), fam.data$RingNumber)
  
  if(length(missing_mothers) > 0){
    fam.data <- bind_rows(fam.data,
                          data.frame(
                            RingNumber = missing_mothers,
                            Mother     = NA_character_,
                            Father     = NA_character_,
                            BroodID    = NA_integer_,
                            Sex        = 1L,  # female
                            stringsAsFactors = FALSE
                          ))
  }
  
  if(length(missing_fathers) > 0){
    fam.data <- bind_rows(fam.data,
                          data.frame(
                            RingNumber = missing_fathers,
                            Mother     = NA_character_,
                            Father     = NA_character_,
                            BroodID    = NA_integer_,
                            Sex        = 2L,  # male
                            stringsAsFactors = FALSE
                          ))
  }
  
  
  # Step 7: Add kinship2-compatible sex columns
  
  fam.data <- fam.data %>%
    mutate(
      # Original Sex: 1=female, 2=male, 0=unknown
      sex = case_when(
        Sex == 2 ~ 1L,      # male -> 1
        Sex == 1 ~ 2L,      # female -> 2
        TRUE     ~ 3L        # unknown -> 3
      ),
      sex_text = case_when(
        Sex == 2 ~ "male",
        Sex == 1 ~ "female",
        TRUE     ~ "unknown"
      ),
      focal = RingNumber == focal_id
    )
  
  
  # Step 8: Are they a recruit?
  
  fam.data <- fam.data %>%
    mutate(
      is_recruit = RingNumber %in% unique(c(
        ped.data$Mother,
        ped.data$Father,
        brood_data$RingNumberFemale,
        brood_data$RingNumberMale
      ))
    )
  
  
  # Step 9: are they are half or full sibling?
  
  fam.data <- fam.data %>%
    mutate(
      sib_type = case_when(
        RingNumber == focal_id ~ NA_character_,  # focal itself
        !is.na(BroodID) & BroodID == focal_row$BroodID & RingNumber != focal_id ~ "full_sib",
        ( (!is.na(Mother) & Mother == focal_row$Mother & Father != focal_row$Father & !is.na(Father)) |
            (!is.na(Father) & Father == focal_row$Father & Mother != focal_row$Mother & !is.na(Mother)) ) ~ "half_sib",
        TRUE ~ NA_character_
      )
    )
  
  
  # Return cleaned family dataset
  fam.data
  
}


# Test
#family_data <- get_family_subset(ped.data, "F...999544", BroodData) # no half sibs
#family_data <- get_family_subset(IndividualDataVlieland, "AH...68076", BroodData) # this one has half sibs
#focal_id <- "AH...68076"




# Add plot attributes -----------------------------------------------------

#   Adds dynamic plotting attributes for ggPedigreeInteractive plots.
#   Calculates node fill, alpha, border colour, and tooltip text.
# Inputs:
#   - fam: family data from get_family_subset
#   - show_recruits: logical, colour only recruits if TRUE
# Outputs:
#   - fam dataframe with new columns: node_fill, node_alpha, border_col, tooltip_text


add_plot_attributes <- function(fam, show_recruits = FALSE) {
  
  # Palette by BroodID
  brood_levels <- sort(unique(fam$BroodID))
  pal <- RColorBrewer::brewer.pal(n = max(3, length(brood_levels)), "Paired")[1:length(brood_levels)]
  names(pal) <- brood_levels
  
  fam <- fam %>%
    mutate(
      node_alpha = case_when(
        show_recruits & is_recruit ~ 1,
        show_recruits & !is_recruit ~ 0.4,
        TRUE ~ 1
      ),
      node_fill = case_when(
        show_recruits & is_recruit ~ ifelse(!is.na(BroodID), pal[as.character(BroodID)], "#F0E1C6"),
        show_recruits & !is_recruit ~ "#D3D3D3",
        TRUE ~ ifelse(!is.na(BroodID), pal[as.character(BroodID)], "#F0E1C6")
      ),
      border_col = "black",
      RNText = ifelse(grepl("^Unknown", RingNumber), "Unknown", RingNumber),
      tooltip_text = paste0(
        "RingNumber: ", RNText,
        ifelse(!is.na(BroodID), paste0("\nClutch: ", BroodID), ""),
        ifelse((sex != 3), paste0("\nSex: ", sex_text), ""),
        ifelse(!is.na(BirthYear), paste0("\nBirth Year: ", BirthYear), ""),
        ifelse(!is.na(RingYear), paste0("\nRing year: ", RingYear), "")
      )
    )
  
  return(fam)
}


# Apply checkbox filters --------------------------------------------------

#   Applies filters for checkboxes (e.g., show_half_sibs).
# Inputs:
#   - fam: family data
#   - show_half_sibs: logical, show half siblings if TRUE
# Outputs:
#   - filtered fam dataframe

filter_family <- function(fam, show_half_sibs = TRUE) {
  fam %>%
    filter(
      show_half_sibs | is.na(sib_type) | sib_type == "full_sib"
    )
}

