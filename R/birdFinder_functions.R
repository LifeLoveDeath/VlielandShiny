
# ==========================================================
# birdFinder Module - functions
# ==========================================================


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

# Filtering colour ring dropdown options ----------------------------------

get_dropdown_options <- function(data, input) {
  list(
    #Yr = min(data[
      #(input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        #(input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        #(input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        #(input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      #"Colour_EarliestStart"
    #]) : year(Sys.Date()),
    Sp = unique(data[
      (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "Species"
    ]),
    Left1 = unique(data[
      (input$Sp == "" | data$Species == input$Sp) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft1"
    ]),
    Left2 = unique(data[
      (input$Sp == "" | data$Species == input$Sp) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Right1 == "" | data$ColourRingRight1 == input$Right1) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingLeft2"
    ]),
    Right1 = unique(data[
      (input$Sp == "" | data$Species == input$Sp) &
        (input$Left1 == "" | data$ColourRingLeft1 == input$Left1) &
        (input$Left2 == "" | data$ColourRingLeft2 == input$Left2) &
        (input$Right2 == "" | data$ColourRingRight2 == input$Right2),
      "ColourRingRight1"
    ]),
    Right2 = unique(data[
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
    if (nrow(filtered) < 5) return(filtered)
    return(NULL)
  }
  
  # Colour ring filtering
  if (input$Sp != "" && !is.na(input$Sp)) filtered <- filtered[!is.na(filtered$Species) & filtered$Species == input$Sp, ]
  if (input$Left1 != "" && !is.na(input$Left1)) filtered <- filtered[!is.na(filtered$ColourRingLeft1) & filtered$ColourRingLeft1 == input$Left1, ]
  if (input$Left2 != "" && !is.na(input$Left2)) filtered <- filtered[!is.na(filtered$ColourRingLeft2) & filtered$ColourRingLeft2 == input$Left2, ]
  if (input$Right1 != "" && !is.na(input$Right1)) filtered <- filtered[!is.na(filtered$ColourRingRight1) & filtered$ColourRingRight1 == input$Right1, ]
  if (input$Right2 != "" && !is.na(input$Right2)) filtered <- filtered[!is.na(filtered$ColourRingRight2) & filtered$ColourRingRight2 == input$Right2, ]
  
  if (nrow(filtered) < 5) return(filtered)
  return(NULL)
}



# Render action buttons in results datatable ------------------------------

make_action_buttons <- function(len, id_prefix, label = "See full info") {
  vapply(seq_len(len), function(i) {
    as.character(shiny::actionButton(
      inputId = paste0(id_prefix, i),
      label = label,
      onclick = 'Shiny.setInputValue("select_button", this.id, {priority: "event"})'
    ))
  }, character(1))
}

