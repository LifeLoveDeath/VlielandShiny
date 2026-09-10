# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                                # contains app ui and server

|- data/                                # Initial data files for "_data_processing.R" & reconfigured versions for use in app
    |- _data_processing.R               # Reconfiguring the initial data files to suit app needs
    |- BroodData.csv                    # Initial data - Brood info for blue tits & great tits hatched or parenting on Vlieland during or before 2025 (date may be updated over time)
    |- BroodDataApp.csv                 # IN APP___For family tree___BroodData.csv with col names altered to match previous col names used in app
    |- ColourNumberRings.csv            # Initial data - Ring number & colour code reference list
    |- IndividualData.csv               # Initial data - Individual bird info for blue tits & great tits hatched or parenting on Vlieland during or before 2025 (date may be updated over time)
    |- IndividualDataVlieland.csv       # IN APP___For bird search___IndividualData.csv plus colour codes & col names altered to match previous col names used in app
    |- Individuallnfo.csv               # IN APP___For individual info table___IndividualData.csv plus a range of summary stats for each bird
    |- location_data.csv                # IN APP___For mapping___"event" denotes if a row is the hatching/ringing/nesting location. Per bird, distance between each event given.

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
    
