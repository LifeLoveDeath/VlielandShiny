# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                                # contains app ui and server

|- data/                                # Initial data files for "_data_processing.R" & reconfigured versions for use in app
    |- _data_processing.R               # Reconfiguring the initial data files to suit app needs
    |- BroodData.csv                    # Initial data  Brood info for blue tits & great tits hatched or parenting on Vlieland during or before 2025 (date may be updated over time)
    |- BroodDataApp.rds                 # IN APP        Family tree & population trends - Reduced version of BroodData.csv
    |- ColourNumberRings.csv            # Initial data  Ring number & colour code reference list
    |- IndividualData.csv               # Initial data  Individual bird info for blue tits & great tits hatched or parenting on Vlieland during or before 2025 (date may be updated over time)
    |- IndividualDataVlieland.rds       # IN APP        Bird search & family tree - IndividualData.csv plus bird colour codes
    |- Individuallnfo.csv               # IN APP        Individual info table - Range of summary stats for each bird
    |- location_data.csv                # IN APP        Preview & main maps - "event" denotes if a row is the hatching/ringing/nesting location. Per bird, distance between each event given.

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
    
