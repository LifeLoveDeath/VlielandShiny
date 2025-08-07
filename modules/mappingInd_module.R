
# Mapping individuals module



library(shiny)
library(leaflet)
library(bslib)
library(viridis)
library(dplyr)
library(reactable)

# UI ----------------------------------------------------

# Server ------------------------------------------------

# this could also be where they were last seen?

mapIndServer <- function(input, output, search_results, session) { # data intput is search_results from the individual search server function. Is it right to do it like this or should it still be data?