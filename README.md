# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                              # contains app ui and server

|- data/                              # contains data (real raw and generated dataset + dummy datasets the app was built with)

|- data_cleaning/                     # scripts for cleaning real data and generating original dummy data

|- R/                                 # Is sourced by app.R automatically
    |- appFormatting.R                # Formatting for the app
    |- projectInfoPage_ui.R           # UI for project info (home) page
    |- birdFinder_module.R            # contains birdFinderUI and birdFinderServer
    |- mapping_module.R
    |- mappingPreview_module.R
    |- family_tree_module.R
    |- populationTrends_module.R       
    |- citizenScience_module.R        # Placeholder      
    |- functions.R                    # Functions
    |- helpers/
        |- creating_coloured_icons.R  # Generating colour ring icons
        |- colour_ring_data_func. R   # Function for getting table of colour rings and link to icon
        
|- www/                               # static items (images etc.)
    | - colouredIcons                 # coloured icons saved here, as well as images for project info home page
    
|- mini_mapping_app/                  # standalone app - maps nextboxes
    |- data
    |- miniMappingApp.R
    
| testing/                            # practice code
    
