# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                                # contains app ui and server

|- data/                                # contains data (real raw and generated dataset + dummy datasets the app was built with)
    |- BroodData.csv                    # IN APP (raw real data modified to remove 1 entry with laydate in Oct 2026)
    |- ColourNumberRings.csv            # Ring number & colour ring combination reference list (real raw data)
    |- Coordinates_Boxes_Vlieland.csv   
    |- Coordinates_Boxes_Vlieland.xlsx
    |- DummyData.csv
    |- IndividualData.csv               # (real raw data)
    |- IndividualDataVlieland.csv       # IN APP - for colour ring search (real generated data)
    |- Individuallnfo.csv               # IN APP - for individual info table (real generated data)
    |- IndividualsData.csv              # dummy data
    |- location_data.csv                # IN APP - for mapping (real generated data)
    |- NestLocationData.csv             # dummy data

|- data_cleaning/                       # scripts for cleaning real data and generating original dummy data

|- R/                                   # Is sourced by app.R automatically
    |- appFormatting.R                  # Formatting for the app
    |- projectInfoPage_ui.R             # UI for project info (home) page
    |- birdFinder_module.R              # contains birdFinderUI and birdFinderServer
    |- mapping_module.R
    |- mappingPreview_module.R
    |- family_tree_module.R
    |- populationTrends_module.R       
    |- citizenScience_module.R          # Placeholder      
    |- functions.R                      # Functions
    |- helpers/
        |- creating_coloured_icons.R    # Generating colour ring icons
        |- colour_ring_data_func. R     # Function for getting table of colour rings and link to icon
        
|- www/                                 # static items (images etc.)
    | - colouredIcons                   # coloured icons saved here, as well as images for project info home page
    
|- mini_mapping_app/                    # standalone app - maps nextboxes
    |- data
    |- miniMappingApp.R
    
| testing/                              # practice code
    
