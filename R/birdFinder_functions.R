
# ==========================================================
# birdFinder Module - functions
# ==========================================================


# Build table of dropdown picker option translations -----------------------------
# To generate up-to-date lists of the different translations use the following code
# Set LANG to the output language you would like
# copy paste output from the console
## LIST GENERATION CODE ##
# LANG <- "nl"
# i18n$.__enclos_env__$private$raw_translate(c("Blue_tit", "Great_tit"), LANG ) %>%
#   #{sub(" ", "", .)} %>% # Keep the leading spaces! They are present in the translations that will come through
#   paste0(. , collapse = '", "') %>%
#   {paste0('"', . , '"')} %>%
#   cat()
## LIST GENERATION CODE ##
# Build table
tag_opts <- c(" Blue_tit", " Great_tit")
nl_opts <- c(" TRANSLATE: Blue tit", " TRANSLATE: Great tit")
en_opts <- c(" Blue tit", " Great tit")
en_use <- c("Blue tit", "Great tit")
picker_translate <- data.frame(tag = tag_opts,
                               nl = nl_opts,
                               en = en_opts,
                               code = en_use)


# Dropdown picker - translation to static ref -----------------------------
# Takes current picker input or list and converts to the English used in data frame
to_static_lang <- function(picker_list, picker_translate) {
  lang <- i18n$get_translation_language()
  df <- picker_translate
  list_in <- picker_list
  
  list_out <- df$code[which(df[[lang]] %in% list_in)]
  list_out
}

# Takes English used in data frame and converts to current selected language
to_current_lang <- function(picker_list, picker_translate) {
  lang <- i18n$get_translation_language()
  df <- picker_translate
  list_in <- picker_list
  
  list_out <- df[[lang]][which(df$code %in% list_in)]
  list_out
}



# Get table of colour rings -----------------------------------------------
get_colour_rings <- function() {
  val <- c("blue", "blue/white", "green", "green/white", "metal", "orange",
           "pink", "pink/blue", "pink/green", "red", "red/white",
           "white", "white/blue", "yellow", "yellow/black")
  
  img <- sprintf(
    "<div class='picker-item'>
       <span class='text'>%s</span>
       <img src='%s.png' class='icon'>
     </div>",
    val,
    gsub("/", "_", val)
  )
  
  data.frame(val = val, img = img, stringsAsFactors = FALSE)
}



# Establish vector of date options for dropdown options ---------------------

date_options <- function(data, input) {
  
  
  
  # Birds selected by all other filters
  filtered_birds <- data[
    which((!is.na(data$Colour_EarliestStart)) &
      (input$Sp == "" | data$Species == input$Sp) &
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
      (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
      (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
      (input$Right2 == "" | data$ColourRingRight2 == input$Right2)),
  ]
  
  # If there are no birds that match the search return null
  if (nrow(filtered_birds) == 0) {return(NULL)}
  
  # Simplify data (reduce to required cols only and then remove duplicate date pairings)
  filtered_reduced <- unique(filtered_birds[,c("Colour_EarliestStart", "Colour_LatestLikely")])
  
  # If only 1 bird matches the search result return a vector from max:min
  if (nrow(filtered_reduced) == 1) {return(filtered_reduced$Colour_LatestLikely :
                                             filtered_reduced$Colour_EarliestStart)}
  
  # Sort ranges by their starting year
  ranges <- filtered_reduced[
    order(filtered_reduced$Colour_EarliestStart),]
  
  # Find the continuous ranges
  merged_ranges <- list()
  
  current_start <- ranges$Colour_EarliestStart[1]
  current_end <- ranges$Colour_LatestLikely[1]
  
  for (i in 2:nrow(ranges)) {
    
    next_start <- ranges$Colour_EarliestStart[i]
    next_end <- ranges$Colour_LatestLikely[i]
    
    # Does this range overlap or directly touch the current range?
    if (next_start <= current_end + 1) {
      
      # Extend current range if necessary
      current_end <- max(current_end, next_end)
      
    } else {
      
      # Gap found — save the current range
      merged_ranges[[length(merged_ranges) + 1]] <- 
        current_start:current_end
      
      # Start the new range
      current_start <- next_start
      current_end <- next_end
    }
  }
  
  # Save final range
  merged_ranges[[length(merged_ranges) + 1]] <- 
    current_start:current_end
  
  # Combine all continuous ranges
  sort(unique(unlist(merged_ranges)))
  
}


# Filtering colour ring dropdown options ----------------------------------

get_dropdown_options <- function(data, input) {
  
  list(
    Yr = date_options(data = data, input = input),
    Sp = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "Species"
    ]),
    Left1 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input$Sp == "" | data$Species == input$Sp) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft1"
    ]),
    Left2 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input$Sp == "" | data$Species == input$Sp) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft2"
    ]),
    Right1 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input$Sp == "" | data$Species == input$Sp) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingRight1"
    ]),
    Right2 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input$Sp == "" | data$Species == input$Sp) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1),
      "ColourRingRight2"
    ])
  )
}





# Filtering the dataset ---------------------------------------------------

filter_birds <- function(data, input) {
  
  filtered <- data
  
  # Ring number search overrides colour rings
  if (!is.null(input$ring_search) && input$ring_search != "") {
    term <- trimws(tolower(gsub("\\.", "", input$ring_search)))
    filtered <- filtered[
      grepl(term, gsub("\\.", "", tolower(filtered$RingNumber))),
    ]
    if (nrow(filtered) < 200) return(filtered)
    return(NULL)
  }
  
  # Colour ring filtering
  if (input$Yr != "" && !is.na(input$Yr)) filtered <- filtered[!is.na(filtered$Colour_EarliestStart) & filtered$Colour_EarliestStart <= input$Yr & filtered$Colour_LatestLikely >= input$Yr, ]
  if (input$Sp != "" && !is.na(input$Sp)) filtered <- filtered[!is.na(filtered$Species) & filtered$Species == input$Sp, ]
  if (input$Left1 != "" && !is.na(input$Left1)) filtered <- filtered[!is.na(filtered$ColourRingLeft1) & filtered$ColourRingLeft1 == input$Left1, ]
  if (input$Left2 != "" && !is.na(input$Left2)) filtered <- filtered[!is.na(filtered$ColourRingLeft2) & filtered$ColourRingLeft2 == input$Left2, ]
  if (input$Right1 != "" && !is.na(input$Right1)) filtered <- filtered[!is.na(filtered$ColourRingRight1) & filtered$ColourRingRight1 == input$Right1, ]
  if (input$Right2 != "" && !is.na(input$Right2)) filtered <- filtered[!is.na(filtered$ColourRingRight2) & filtered$ColourRingRight2 == input$Right2, ]
  
  if (nrow(filtered) < 200) return(filtered)
  return(NULL)
}



# Render action buttons in results datatable ------------------------------

make_action_buttons <- function(len, id_prefix, label = "See full info") {
  vapply(seq(from = 1, to = len, length.out = len), function(i) {
    as.character(shiny::actionButton(
      inputId = paste0(id_prefix, i),
      label = label,
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    ))
  }, character(1))
}