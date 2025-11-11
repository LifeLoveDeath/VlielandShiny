
# Individual info table module

# Load packages - moved to app.r
# library(shiny)
# library(DT)

#vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
#location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)


# Functions ----------------------------------
# Function for getting data



# UI -----------------------------------------
individualInfoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Page title
    fluidRow(
      column(
        width = 12,
        br(),
        h4("Bird profile", style = "color:#3f5262; font-weight:500;"),
        p("The table below summarizes the key information about this bird including...")
      )
    ),
    
    # Formatting for table:
    tags$style(HTML("
      .dt-header-row {
        background-color: #e8eef3 !important;
        font-weight: bold !important;
        text-transform: uppercase;
        color: #3f5262 !important;
      }
    ")),
    
    #Table output
    uiOutput(ns("indInfoTab_ui"))
  )
}




# Server --------------------------------------

individualInfoServer <- function(id, vlieland.data, selected_ring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: data for individual bird
    individual.data <- reactive({
  req(selected_ring())
  
  df <- vlieland.data[vlieland.data$RingNumber == selected_ring(), ]
  
  # Edit cols
  df <- df %>%
    # round mean to nearest whole number
    mutate(`MeanClutchSize` = round(`MeanClutchSize`)) %>%
    
    # create a clutch–size range column
    rowwise() %>%
    mutate(`ClutchSizeRange` = paste0(`MinClutchSize`, "–", `MaxClutchSize`)) %>%
    ungroup() %>%
    
    mutate(
      DispersalDistance_m = round(DispersalDistance_m),
      TotalDistance_m = round(TotalDistance_m)
    ) %>%
    
    # remove min/max columns if you no longer want them
    select(-MinClutchSize, -MaxClutchSize)
  
  
  df <- df[, c("RingNumber", "ColourRingCombo", "Species", "SexText",
               "BirthYear", "RingYear",
               "MotherRingColour", "FatherRingColour",
               "BreedingAttempts", "NumNestSites", "FirstBreedingYear", "LastBreedingYear",
               "MeanClutchSize", "ClutchSizeRange", "TotalEggs",
               "DispersalDistance_m", "TotalDistance_m")]
  
  colnames(df) <- c("Ring Number", "Colour rings", "Species", "Sex",
                    "Birth year", "Ring year", 
                    "Mother", "Father",
                    "Breeding attempts", "Number of nest sites", "First breeding year", "Last breeding year",
                    "Mean clutch size", "Clutch size range", "Total eggs",
                    "Dispersal distance", "Total distance travelled")
  
  # Convert to long
  long <- data.frame(
    Variable = names(df),
    Value = as.character(t(df)),
    stringsAsFactors = FALSE
  )
  
  # Format colour rings
  long[long$Variable == "Colour rings", "Value"] <- 
    gsub("-", ", ", long[long$Variable == "Colour rings", "Value"])
  
  
  # Format distances
  long$Value[long$Variable == "Dispersal distance" & !is.na(long$Value)] <- 
    paste0(long$Value[long$Variable == "Dispersal distance" & !is.na(long$Value)], " meters")
  
  long$Value[long$Variable == "Total distance travelled" & !is.na(long$Value)] <- 
    paste0(long$Value[long$Variable == "Total distance travelled" & !is.na(long$Value)], " meters")
  
  
  # Replace any missing data with "unknown"
  long$Value <- ifelse(
    is.na(long$Value) | long$Value == "",
    "Unknown",
    long$Value
  )
  
  
  # --- Section headers ---
  
  # Identity header from RingNumber
  identity_header <- data.frame(
    Variable = "Ring number",
    Value = long$Value[long$Variable == "Ring Number"],
    stringsAsFactors = FALSE
  )
  
  # Other sections
  life_history <- long[long$Variable %in% c("Birth year", "Ring year", "Mother", "Father"), ]
  general <- long[long$Variable %in% c("Colour rings","Species","Sex"), ]
  reproduction <- long[long$Variable %in% c("Breeding attempts", "Number of nest sites", "First breeding year", "Last breeding year", "Mean clutch size", "Clutch size range", "Total eggs"), ]
  dispersal <- long[long$Variable %in% c("Dispersal distance", "Total distance travelled"), ]
  
  # Combine with section headers
  final_long <- rbind(
    identity_header,
    general,
    data.frame(Variable = "Life history", Value = "", stringsAsFactors = FALSE),
    life_history,
    data.frame(Variable = "Reproduction", Value = "", stringsAsFactors = FALSE),
    reproduction,
    data.frame(Variable = "Dispersal", Value = "", stringsAsFactors = FALSE),
    dispersal
  )
  
  final_long
})
    
    
    # Create data table
    output$indInfoTab_ui <- renderUI({
      renderDT({
        datatable(
          individual.data(),
          rownames = FALSE,
          colnames = NULL,  # remove "Variable" / "Value"
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = FALSE,
            createdRow = JS(
              "function(row, data, dataIndex) {",
              "  // Highlight section headers (Value is empty or first row)",
              "  if(dataIndex === 0 || data[1] === '') {",
              "    $(row).addClass('dt-header-row');",
              "  }",
              "}"
            )
          )
        )
      })
    })
    
  }) 
} 






# individual.data <- vlieland.data[which(vlieland.data$RingNumber == "RN00132"), ]
# data.table(individual.data)
# 
# 
# # Change order of vars
# colnames(individual.data)
# individual.data <- individual.data[ , c("RingNumber", "ColourRingCombo", "Species", "Sex", "BirthMonth", "BirthYear", "DeathYear", "Parent1", "Parent2", "Nestbox", "Lon", "Lat")]
# 
# # Change var names
# colnames(individual.data) <- c("RingNumber", "Colour rings", "Species", "Sex", "Birth month", "Birth year", "Death year", "Parent1", "Parent2", "Birth nestbox", "Lon", "Lat")
# 
# 
# # Convert to long data
# individual.data.long <- data.frame(
#   Variable = names(individual.data),
#   Value = as.character(t(individual.data)),
#   stringsAsFactors = FALSE
# )
# 
# 
# # Reformat vars
# # Colour rings
# individual.data.long[which(individual.data.long$Variable == "Colour rings"), "Value"] <- gsub("-", ", ", individual.data.long[which(individual.data.long$Variable == "Colour rings"), "Value"])
# 
# 
# # Sex
# # Need to change this
# individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] <- if(individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] == "F") "Female" 
# individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] <- if(individual.data.long[which(individual.data.long$Variable == "Sex"), "Value"] == "M") "Male"
# 
# 
# datatable(
#   individual.data.long,
#   rownames = FALSE,
#   options = list(
#     paging = FALSE,
#     searching = FALSE,
#     info = FALSE,
#     ordering = FALSE
#   )
# )
