# VlielandShiny
Shiny app for Vlieland great tit project

VlielandShiny.Rproj
|- app.R                              # currently separate ui.R and server.R
|- data/
|- data_cleaning/
|- modules/
|   |- birdFinder_module.R            # contains birdFinderUI and birdFinderServer
    |- mapping_module.R
|- helpers/
    |- creating_coloured_icons.R
|- www/                               # static items (images etc.)
    | - colouredIcons
|- mini_mapping_app/                  # standalone app - maps nextboxes
    |- data
    |- miniMappingApp.R
