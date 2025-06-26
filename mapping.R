# Creating maps

# Load data
vlieland.data <- read.csv("data/DummyData.csv", row.names = NULL)


# Load packages
library(tidyverse)
library(osmdata)
library(leaflet)


library(leaflet)
m <- leaflet() %>% addTiles() %>% # adds default OpenStreetMap map tiles 
  setView(lng = 4.960574, lat = 53.264568, zoom = 11) # got long and lat from google maps - sets the view to be on Vlieland
# fitBounds() fits the map within a specified rectangle of long and lat - won't need to be beyond vlieland so could try and do this?

m # see map
