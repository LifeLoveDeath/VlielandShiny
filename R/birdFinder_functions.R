
# ==========================================================
# birdFinder Module - functions
# ==========================================================


# Build table of dropdown picker option translations -----------------------------
# To generate up-to-date lists of the different translations use the following code
# Set LANG to the output language you would like
# Run the tag_opts object below
# run the piped code to get your translation
# copy paste output from the console
## LIST GENERATION CODE ##
# i18n <- Translator$new(translation_csvs_path = "./translation_data/")
# LANG <- "en"
# i18n$.__enclos_env__$private$raw_translate(tag_opts, LANG ) %>%
#   {sub(" ", "", .)} %>%
#   paste0(. , collapse = '", "') %>%
#   {paste0('"', . , '"')} %>%
#   cat()
## LIST GENERATION CODE ##
# Build table
tag_opts <- c("Blue_tit", "Great_tit",
              "birdFinder_Col_blue", "birdFinder_Col_blue/white",
              "birdFinder_Col_green", "birdFinder_Col_green/white",
              "birdFinder_Col_metal", "birdFinder_Col_orange",
              "birdFinder_Col_pink", "birdFinder_Col_pink/blue", "birdFinder_Col_pink/green",
              "birdFinder_Col_red", "birdFinder_Col_red/white",
              "birdFinder_Col_white", "birdFinder_Col_white/blue",
              "birdFinder_Col_yellow", "birdFinder_Col_yellow/black")
nl_opts <- c("TRANSLATE: Blue tit", "TRANSLATE: Great tit",
             "TRANSLATE: blue", "TRANSLATE: blue/white",
             "TRANSLATE: green", "TRANSLATE: green/white",
             "TRANSLATE: metal", "TRANSLATE: orange",
             "TRANSLATE: pink", "TRANSLATE: pink/blue","TRANSLATE: pink/green",
             "TRANSLATE: red", "TRANSLATE: red/white",
             "TRANSLATE: white", "TRANSLATE: white/blue",
             "TRANSLATE: yellow", "TRANSLATE: yellow/black")
en_opts <- c("Blue tit", "Great tit",
             "blue", "blue/white", "green", "green/white", "metal", "orange",
             "pink", "pink/blue", "pink/green", "red", "red/white",
             "white", "white/blue", "yellow", "yellow/black")
en_use <- c("Blue tit", "Great tit",
            "blue", "blue/white", "green", "green/white", "metal", "orange",
            "pink", "pink/blue", "pink/green", "red", "red/white",
            "white", "white/blue", "yellow", "yellow/black")
picker_translate <- data.frame(tag = tag_opts,
                               nl = nl_opts,
                               en = en_opts,
                               code = en_use)


# Dropdown picker - translation to static ref -----------------------------
# Takes current picker input or list and converts to the English used in data frame
to_static_lang <- function(picker_list, picker_translate, lang) {
  df <- picker_translate
  list_in <- picker_list
  
  list_out <- recode_values(list_in, from = df[[lang]], to = df$code)
  list_out
}

# Takes English used in data frame and converts to current selected language
to_current_lang <- function(picker_list, picker_translate, lang) {
  df <- picker_translate
  list_in <- picker_list
  
  list_out <- recode_values(list_in, from = df$code, to = df[[lang]])
  list_out
}



# Get table of colour rings -----------------------------------------------
get_colour_rings <- function(picker_translate, lang) {
  val <- c("blue", "blue/white", "green", "green/white", "metal", "orange",
           "pink", "pink/blue", "pink/green", "red", "red/white",
           "white", "white/blue", "yellow", "yellow/black")
  val_lang <- to_current_lang(picker_list = val, picker_translate, lang)

  img <- sprintf(
    "<div class='picker-item'>
       <span class='text'>%s</span>
       <img src='%s.png' class='icon'>
     </div>",
    val_lang,
    gsub("/", "_", val)
  )

  data.frame(val = val_lang, img = img, stringsAsFactors = FALSE)
}


# Establish vector of date options for dropdown options ---------------------

date_options <- function(data, input, lang = lang) {
  
  # Stabilise translated inputs to app internal terms
  input_Sp <- ifelse(input$Sp == "", "", to_static_lang(input$Sp, picker_translate, lang))
  input_Left1 <- ifelse(input$Left1 == "", "", to_static_lang(input$Left1, picker_translate, lang))
  input_Left2 <- ifelse(input$Left2 == "", "", to_static_lang(input$Left2, picker_translate, lang))
  input_Right1 <- ifelse(input$Right1 == "", "", to_static_lang(input$Right1, picker_translate, lang))
  input_Right2 <- ifelse(input$Right2 == "", "", to_static_lang(input$Right2, picker_translate, lang))
  
  # Birds selected by all other filters
  filtered_birds <- data[
    which((!is.na(data$Colour_EarliestStart)) &
      (input_Sp == "" | data$Species == input_Sp) &
      (input_Left1 == "" | data$ColourRingLeft1 == input_Left1) &
      (input_Left2 == "" | data$ColourRingLeft2 == input_Left2) &
      (input_Right1 == "" | data$ColourRingRight1 == input_Right1) &
      (input_Right2 == "" | data$ColourRingRight2 == input_Right2)),
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

get_dropdown_options <- function(data, input, lang) {
  
  # Stabilise translated inputs to app internal terms
  input_Sp <- ifelse(input$Sp == "", "", to_static_lang(input$Sp, picker_translate, lang))
  input_Left1 <- ifelse(input$Left1 == "", "", to_static_lang(input$Left1, picker_translate, lang))
  input_Left2 <- ifelse(input$Left2 == "", "", to_static_lang(input$Left2, picker_translate, lang))
  input_Right1 <- ifelse(input$Right1 == "", "", to_static_lang(input$Right1, picker_translate, lang))
  input_Right2 <- ifelse(input$Right2 == "", "", to_static_lang(input$Right2, picker_translate, lang))
  
  list(
    Yr = date_options(data = data, input = input, lang = lang),
    Sp = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input_Left1 == "" | data$ColourRingLeft1 == input_Left1) &
        (input_Left2 == "" | data$ColourRingLeft2 == input_Left2) &
        (input_Right1 == "" | data$ColourRingRight1 == input_Right1) &
        (input_Right2 == "" | data$ColourRingRight2 == input_Right2),
      "Species"
    ]) %>% to_current_lang(picker_translate, lang
                           ),
    Left1 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input_Sp == "" | data$Species == input_Sp) &
        (input_Left2 == "" | data$ColourRingLeft2 == input_Left2) &
        (input_Right1 == "" | data$ColourRingRight1 == input_Right1) &
        (input_Right2 == "" | data$ColourRingRight2 == input_Right2),
      "ColourRingLeft1"
    ])%>% to_current_lang(picker_translate, lang
                          ),
    Left2 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input_Sp == "" | data$Species == input_Sp) &
        (input_Left1 == "" | data$ColourRingLeft1 == input_Left1) &
        (input_Right1 == "" | data$ColourRingRight1 == input_Right1) &
        (input_Right2 == "" | data$ColourRingRight2 == input_Right2),
      "ColourRingLeft2"
    ])%>% to_current_lang(picker_translate, lang
                          ),
    Right1 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input_Sp == "" | data$Species == input_Sp) &
        (input_Left1 == "" | data$ColourRingLeft1 == input_Left1) &
        (input_Left2 == "" | data$ColourRingLeft2 == input_Left2) &
        (input_Right2 == "" | data$ColourRingRight2 == input_Right2),
      "ColourRingRight1"
    ])%>% to_current_lang(picker_translate, lang
                          ),
    Right2 = unique(data[
      (input$Yr == "" | (data$Colour_EarliestStart <= input$Yr & data$Colour_LatestLikely >= input$Yr)) &
        (input_Sp == "" | data$Species == input_Sp) &
        (input_Left1 == "" | data$ColourRingLeft1 == input_Left1) &
        (input_Left2 == "" | data$ColourRingLeft2 == input_Left2) &
        (input_Right1 == "" | data$ColourRingRight1 == input_Right1),
      "ColourRingRight2"
    ])%>% to_current_lang(picker_translate, lang
                          )
  )
}





# Filtering the dataset ---------------------------------------------------

filter_birds <- function(data, input, lang) {
  
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
  
  # Stabilise translated inputs to app internal terms
  input_Sp <- ifelse(input$Sp == "", "", to_static_lang(input$Sp, picker_translate, lang))
  input_Left1 <- ifelse(input$Left1 == "", "", to_static_lang(input$Left1, picker_translate, lang))
  input_Left2 <- ifelse(input$Left2 == "", "", to_static_lang(input$Left2, picker_translate, lang))
  input_Right1 <- ifelse(input$Right1 == "", "", to_static_lang(input$Right1, picker_translate, lang))
  input_Right2 <- ifelse(input$Right2 == "", "", to_static_lang(input$Right2, picker_translate, lang))
  
  # Colour ring filtering
  if (input$Yr != "" && !is.na(input$Yr)) filtered <- filtered[!is.na(filtered$Colour_EarliestStart) & filtered$Colour_EarliestStart <= input$Yr & filtered$Colour_LatestLikely >= input$Yr, ]
  if (input_Sp != "" && !is.na(input_Sp)) filtered <- filtered[!is.na(filtered$Species) & filtered$Species == input_Sp, ]
  if (input_Left1 != "" && !is.na(input_Left1)) filtered <- filtered[!is.na(filtered$ColourRingLeft1) & filtered$ColourRingLeft1 == input_Left1, ]
  if (input_Left2 != "" && !is.na(input_Left2)) filtered <- filtered[!is.na(filtered$ColourRingLeft2) & filtered$ColourRingLeft2 == input_Left2, ]
  if (input_Right1 != "" && !is.na(input_Right1)) filtered <- filtered[!is.na(filtered$ColourRingRight1) & filtered$ColourRingRight1 == input_Right1, ]
  if (input_Right2 != "" && !is.na(input_Right2)) filtered <- filtered[!is.na(filtered$ColourRingRight2) & filtered$ColourRingRight2 == input_Right2, ]
  
  if (nrow(filtered) < 200) return(filtered)
  return(NULL)
}



# Render action buttons in results datatable ------------------------------

make_action_buttons <- function(len, id_prefix, label) {
  vapply(seq(from = 1, to = len, length.out = len), function(i) {
    as.character(shiny::actionButton(
      inputId = paste0(id_prefix, i),
      label = label,
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    ))
  }, character(1))
}