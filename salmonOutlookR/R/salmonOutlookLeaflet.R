# test script to use leaflet with salmon maps
# unfinished
# https://rstudio.github.io/leaflet/map_widget.html

library(leaflet)
library(htmltools)
library(sf)
library(dplyr)

species <- "Chinook"
outlookYear <- 2018

# read in feature class for conservation unit polygons
gdbase <- paste0( getwd(), "/Salmon_Outlook_Maps.gdb")

# choose polygons for outlook groups
if (toupper(species) == "PINK") {
  olPolygons <- paste0(tolower(species), yearType, "OL")
} else {
  olPolygons <- paste0(tolower(species), "OL")
}

# read feature class as data frames
fc <- st_read(gdbase, olPolygons)

# transform to same coordinates as OSM maps
fc <- st_transform(fc, "+init=epsg:4326")

# # make outlook group factor for fill
# fc$OL <- as.factor(fc$OL)

# simple polygon for testing
fcSimple <- filter(fc, OL == 45)

#make leaflet map object
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = fcSimple, fillColor = fc$OL)
m

# addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#             opacity = 1.0, fillOpacity = 0.5,
#             fillColor = ~OL,
#             highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                 bringToFront = TRUE))
#   addMarkers(~SpeciesLocated$long, ~SpeciesLocated$lat,
#              popup = ~htmlEscape(as.character(SpeciesLocated$OL))) %>%
#   addPolygons()
#   addLabelOnlyMarkers(~SpeciesLocated$long, ~SpeciesLocated$lat,
#                       label = ~htmlEscape(SpeciesLocated$OL))
#
# m  # Print the map
