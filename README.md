# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                              # contains app ui and server
|- data/
|- data_cleaning/
|- R/                                 # Is sourced by app.R automatically
|   |- birdFinder_module.R            # contains birdFinderUI and birdFinderServer
    |- mapping_module.R
    |- mappingPreview_module.R
    |- family_tree_module.R
    |- functions_module.R             # Functions
|- helpers/
    |- creating_coloured_icons.R
    |- 
|- www/                               # static items (images etc.)
    | - colouredIcons
|- mini_mapping_app/                  # standalone app - maps nextboxes
    |- data
    |- miniMappingApp.R
| testing/                            # practice code
    
