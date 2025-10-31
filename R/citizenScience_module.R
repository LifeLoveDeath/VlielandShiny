
# citizenScience module

# UI function --------------------------------------------------------------


citizenScienceUI <- function(data) {
  
  colour_rings <- get_colour_rings()
  
  div(
    style = "background-color: #f2f4f5; padding: 5px;",
    
    # --- White panel container ---
    div(
      style = "
        background-color: #ffffff;
          max-width: 90vw; 
          min-height: 800px;
          margin: 0 auto;
          padding: 30px 40px;
          border-radius: 8px;
          box-shadow: 0 0 12px rgba(0,0,0,0.08);
      ",
      
      # --- Page title and instructions ---
      fluidRow(
        column(
          width = 12,
          h3("Citizen Science", style = "color:#3f5262; font-weight:500;")
        )
      )
    ))}





# Server function --------------------------------------------------------------