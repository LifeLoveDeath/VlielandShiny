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

# fitBounds() fits the map within a specified rectangle of long and lat - won't need to be beyond vlieland so could try and do this? but need long and lat for all 4 boundaries, not just the centre
m2 <- leaflet() %>% addTiles() %>% # adds default OpenStreetMap map tiles 
  fitBounds(lng1 = 4.840239, lat1 = 53.201582, lng2 = 5.103568, lat2 = 53.318848)
#53.318848, 5.103568 # from google maps
#53.201582, 4.840239 # from google maps
# more zoomed out that i thought it'd be
# add you can still zoom out further
# so use setView

m




# Function to map specified nestboxes
input <- 629
mapBoxes <- function(input, data, session) {
  
}
