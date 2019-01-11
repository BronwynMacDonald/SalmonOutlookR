# test script to use another R package instead of ggmap
# use sf package to plot instead of ggmap


library(ggplot2)
library(ggmap)
library(sf)

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

# transform to same coordinates as google maps, works with polygons with bbbox fn
fc <- st_transform(fc, 3857)

# make outlook group factor for fill
fc$OL <- as.factor(fc$OL)

#load base maps from file
load("basemaps/bc_map.RData")
load("basemaps/ykn_map.RData")
load("basemaps/south_bc_map.RData")

# choose basemap accorting to user input
if (toupper(province) == "BC") {basemap <- bc_map}
if (toupper(province) == "YUKON") {basemap <- ykn_map}
if (province == "southBC") {basemap <- south_bc_map}

# plot basemap with outlook polygons assuming a coordinate refence system of 3857
baseplot <- plot(st_transform(fc, crs = 3857)[1], bgMap = basemap,
                 main = title)

# convert to graphic to use with ggmap
baseplot2 <- st_as_grob(baseplot)

outlookMap2 <- ggplot() +
  geom_sf(data = baseplot) +
  #geom_image(data = SpeciesLocated, aes(x = long, y = lat, image = symbol)) +
  annotate("text", x = SpeciesLocated$long, y = SpeciesLocated$lat,
           label = SpeciesLocated$OL) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = createDate) +
  labs(x = "", y = "") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0, 0, -1), 'lines'),
        legend.position = "none")
