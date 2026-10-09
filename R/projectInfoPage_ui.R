
# ----------------------------------------------------------------------
# Project Info Page Module
# ----------------------------------------------------------------------
# Placeholder for the homepage - info about project and app
# Images and placeholder text taken from website
# Images must be /www folder


# Translation ------------------------------------------------


projectInfoUI <- function(id, i18n) {
    ns <- NS(id)
  
  tagList(
  
  tags$style(HTML("
      /* overall light grey page background */
      body, .content-wrapper {
        background-color: #f2f4f5 !important;
      }
    
      /* central white panel */
      .inner-panel {
        background-color: #ffffff;
        max-width: 1000px;          /* change width to taste */
        margin: 0 auto;             /* center horizontally */
        padding: 30px 40px;
        border-radius: 8px;
        box-shadow: 0 0 12px rgba(0,0,0,0.08);
      }
    ")),
  
  #### ---- Page wrapper ----------------------------------------
  div(style = "padding: 20px; background-color: #f2f4f5;",
      
      
      
      #### --- Top section project info and image ----------------------
      
      div(
        style = "
    width: 100%;
    height: 400px;
    background-image: url('banner3.jpg');
    background-size: cover;
    background-position: center;
    position: relative;
    margin: -20px 0 20px 0;
  ",
        
        # Centered container with same max width as inner-panel
        div(
          style = "
      max-width: 1000px;    /* same as .inner-panel */
      margin: 0 auto;
      height: 100%;
      display: flex;         /* allows horizontal layout if needed */
      justify-content: flex-start;  /* align items to left */
      align-items: flex-start;     /* top-aligned for when aligned to the left */
      padding-top: 40px;
      #align-items: flex-end; /* align item to bottom of image background */
      #padding-bottom: 80px;
    ",
          
          # The overlay card
          div(
            style = "
        background-color: rgba(255, 255, 255, 0.85);
        padding: 20px; /* when aligned to left */
        #padding: 20px 5%; /* when aligned to bottom */
        border-radius: 8px;
        max-width: 400px;  /* when aligned to left */
        #width: 95%;       /* when aligned bottom - full width of container */
        margin: 0 0px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.3);
      ",
            h2(i18n$t("about_the_project_heading"), style = "color: #3f5262;"),
            p(
              i18n$t("about_the_project_text"),
              style = "font-size:16px; line-height:1.6; color:#3f5262;"
            ),
            # Copyright text
            tags$div(
              "© 2025 Erik Postma ",
              style = "
      position: absolute;
      top: 0px;
      right: 0px;
      font-size: 12px;
      color: white;
      background-color: rgba(63,82,98,0.7);
      z-index: 10;
    "
            )
          )
        )
      ),
      
      
      #### ---- Centre panel --------------------------------
      
      div(class = "inner-panel",
          style = "
      position: relative;    /* allows overlap over previous section */
      margin-top: -70px;     /* pull panel up over hero image */
      z-index: 2;            /* ensures it sits on top of the image */
      padding: 30px 40px;
      border-radius: 8px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.15);
    ",          
          
          
          #### --- What the App Does section ----
          
          fluidRow(
            column(
              width = 12,
              h2(i18n$t("about_the_app_heading"), style = "color: #3f5262; margin-top: 30px;"),
              p(i18n$t("about_the_app_text"), 
                style = "font-size: 16px; color = #3f5262; line-height: 1.6;")
            )
          ),
          
          #### --- Features / cards section ----
          
          
          
          fluidRow(
            # watch the page, if a card is clicked perform the following function
            # store the data-tab value for the clicked card
            # tell shiny to set the card_clicked value as the stored data-tab value
            # ensure Shiny resets the card_clicked value every time a card is clicked
            tags$script(HTML("
                  $(document).on('click', '.clickable-card', function() {
                  var tab = $(this).data('tab');
                  Shiny.setInputValue(
                  'card_clicked',
                  tab,
                  {priority: 'event'}
                  );
                  });
                 ")),
            column(
              width = 4,
              wellPanel(
                id = "individual_search_card",
                class = "clickable-card",
                'data-tab' = "individual_search",
                h4(i18n$t("find_an_individual_heading"), style = "color: white"),
                p(i18n$t("find_an_individual_text"), style = "color: white"),
                style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     #background-color: #f8f9fa;
                     background-color: #004b84;
                     height: 170px;
                     border-radius: 8px;"
                ),
            ),
            column(
              width = 4,
              wellPanel(
                id = "pop_trends_card",
                class = "clickable-card",
                'data-tab' = "pop_trends",
                h4(i18n$t("population_trends_heading"), style = "color: white"),
                p(i18n$t("population_trends_text"), style = "color: white"),
                style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     background-color: #004b84;
                     height: 170px;
                     border-radius: 8px;"
              )
            ),
            column(
              width = 4,
              wellPanel(
                id = "citizen_sci_card",
                class = "clickable-card",
                'data-tab' = "citizen_sci",
                h4(i18n$t("contribute_your_observations_heading"), style = "color: white"),
                p(i18n$t("contribute_your_observations_text"), style = "color: white"),
                style = "display: flex;
                     flex-direction: column;
                     justify-content: center;  /* vertical centering */
                     align-items: center;      /* horizontal centering */
                     text-align: center;
                     background-color: #004b84;
                     height: 170px;
                     border-radius: 8px;"
              )
            )
          ),
          
          
          #### --- Species info ----
          fluidRow(
            column(
              width = 12,
              h2(i18n$t("research_heading"),
                 style = "color: #3f5262; margin-top: 30px;")
            )
          ),
          
          fluidRow(
            column(
              width = 8,
              p(i18n$t("research_text"),
                style = "font-size: 16px; color: #3f5262; line-height: 1.6;")
            ),
            column(
              width = 4,
              style = "text-align: center;",
              tags$img(
                src = "greattit.jpg",
                alt = "Image",
                style = "max-width: 600px; width: 100%; border-radius: 5px;"
              )
            )
          ),
          
          #### --- Footer / contact section ----
          fluidRow(
            column(
              width = 12,
              style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 8px;",
              h4(i18n$t("contact_links_heading")),
              p(i18n$t("contact_links_text"), style = "font-size: 14px;")
            )
          )
      )
  ))
}
