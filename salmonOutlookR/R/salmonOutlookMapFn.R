# test script to find other basemaps since issues with google
# basemap from R packages maps or mapdata

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)

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

## transform to same coordinates as google maps and almost works with both labels and polygons
#fc <- st_transform(fc, "+proj=longlat +datum=WGS84")

# transform to same coordinates as OSM maps and  works in leaflet
fc <- st_transform(fc, "+init=epsg:4326")

## transform to same coordinates as google maps, works with polygons with bounding box function exactly
fc <- st_transform(fc, 3857)

# make outlook group factor for fill
fc$OL <- as.factor(fc$OL)

  #get basic BC basemap from R package
  bcMap <- map("worldHires", "Canada", xlim = c(-130, -115),
      ylim = c(48, 56), col = "gray90", fill = TRUE)

  #add USA to map
  usMap <- map("worldHires","usa", xlim=c(-140,-110), ylim=c(48,64),
      col="gray95", fill=TRUE)

  # make map data readable for ggplot2
  canada <- map_data("world2", "Canada")
  us <- map_data("usa")


  #box()

  plot <- ggplot() +
    geom_polygon(bcMap, aes(x = long, y = lat, group = group))
  plot




