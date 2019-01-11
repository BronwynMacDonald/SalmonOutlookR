# setup.R

# libraries
library(dplyr) # formatting and cleaning data
library(ggmap) # downloading google maps
library(lubridate) # manipulating dates
library(magrittr) # manipulating data
library(stringr) # trimming white space
library(ggimage) # using custom symbols on map
library(sf) # use to load spatial data as data frame
library(purrr) # use to loop over species and province combination
library(magick) # use to layer symbols and polygons

# parameters
species <- "Chinook"
#species <- "Pink"
outlookYear <- 2018
province <- "BC"

#save base maps as Rdata files
save(bc_map, file = "bc_map.RData")
save(ykn_map, file = "ykn_map.RData")

#load base maps from file
load("bc_map.RData")
load("ykn_map.RData")


#http://rstudio-pubs-static.s3.amazonaws.com/16660_7d1ab1b355344578bbacb0747fd485c8.html

library(rgdal) # For reading shapefiles and feature classes
library(ggplot2)
library(raster)
library(dismo)
library(ggsn) # scale bars and north arrows
library(rgeos) # for gbuffer
library(broom) # to tidy shapefile to plot with ggmap

##Old code

# get original bounding box from basemap to return to for annotation & ggimage
bboxOriginal <- setNames(unlist(attr(basemap, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

# get bounding box from modified basemap for interest
bboxModified <- setNames(unlist(attr(basemap2, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

# plot basemap & polygon layer together
outlookMap <- ggmap(basemap2) +
  geom_sf(data = fc, aes(fill = OL), inherit.aes = FALSE, alpha = 0.5)

# plot original basemap to see what attributes should be for ggplot2 to work
basemapUnmodified <- ggmap(basemap)

# get attributes from original to write back
longOriginal <- basemapUnmodified[["data"]]["lon"]
latOriginal <- basemapUnmodified[["data"]]["lat"]
lonDimOrig <- attr(basemapUnmodified[["data"]], "out.attrs")$dimnames$lon
latDimOrig <- attr(basemapUnmodified[["data"]], "out.attrs")$dimnames$lat

#overwrite combined plot with original bbox values
outlookMap[["data"]]["lon"] <- longOriginal
outlookMap[["data"]]["lat"] <- latOriginal
attr(outlookMap[["data"]], "out.attrs")$dimnames$lon <- lonDimOrig
attr(outlookMap[["data"]], "out.attrs")$dimnames$lat <- latDimOrig

geom_sf(data = centroids, aes(),inherit.aes = FALSE) +

centroids <- st_read(gdbase, "Chinook_centroids_2018_1")
centroids <- st_transform(centroids, 3857)

rgdal
#read the feature class using rgdal
fc <- readOGR(dsn = gdbase,layer = "ChinookOL")

# reproject to match google mapsusing rgdal
fc <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))

# tidy feature class to plot with ggmap
fctidy <- fortify(fc)

geom_polygon(aes(x = long, y = lat, group = group),
             data = fctidy)

# plot raster data
ggplot(data = myDataFrame, aes_string(y = "Latitude", x = "Longitude")) +
  geom_raster(aes(fill = Values)) +

# create extent vector to get basemap
extent <- extent(-130, -119, 48.5, 57)

# get map using dismo to prevent projection issues
basemap <- gmap(extent, type = c("terrain"), lonlat = FALSE,
                path = "&style=feature:all|element:labels|visibility:off&style=feature:administrative.country|element:geometry.stroke|visibility:off")

# reproject
basemap_proj <- projectRaster(basemap, crs = "+proj=utm +zone=10 +datum=WGS84")

# convert raster to points to use ggplot
myPoints <- raster::rasterToPoints(basemap)
myDataFrame <- data.frame(myPoints)
colnames(myDataFrame) <- c("Longitude", "Latitude", "Values")

library(dismo)
e = extent(-14 , 58 , 28 , 64)
mapImageData2 <- gmap(e, type = c("terrain"), lonlat = TRUE,
                      path = "&style=feature:all|element:labels|visibility:off&style=feature:administrative.country|element:geometry.stroke|visibility:off")

mapImageData2_proj <- projectExtent(mapImageData2, crs = "+proj=utm +zone=31 +datum=WGS84")

mapImageData2_proj <- projectRaster(mapImageData2, crs = "+proj=utm +zone=31 +datum=WGS84")
plot(mapImageData2_proj)

myPoints <- raster::rasterToPoints(mapImageData2)
myDataFrame <- data.frame(myPoints)
colnames(myDataFrame) <- c("Longitude", "Latitude", "Values")

ggplot(data=myDataFrame, aes_string(y = "Latitude", x = "Longitude")) +
  geom_raster(aes(fill = Values))


geom_polygon(aes(x = long, y = lat, group = group),
             data = fctidy) +

#read the feature class
fc <- readOGR(dsn = gdbase,layer = "ChinookOL")

# # transform to same coordinates as google maps
fc <- spTransform(fc, "+proj=longlat +datum=WGS84")

# tidy feature class to plot with ggmap
fctidy <- tidy(fc)

coord_sf(crs = st_crs(3857)) +
  geom_sf(data = fc, aes(fill = OL),
          inherit.aes = FALSE, alpha = 0.5) +
  geom_sf(data = centroids, aes(),inherit.aes = FALSE) +
  #geom_label_repel(data=centroids, aes(x=lon, y=lat, label=Trends_Updated__OL)) +

centroids <- st_read(gdbase, "Chinook_centroids_2018_1")
centroids <- st_transform(centroids, 3857)
# create lat and lon fields from shape column
centroids %<>%
  mutate(., lon = str_extract(Shape, "-[0-9.]{1,}"),
         lat = str_extract(Shape, "[0-9.]{1,}"))

SpeciesLocated$lon <- as.numeric(SpeciesLocated$lon)
SpeciesLocated$lat <- as.numeric(SpeciesLocated$lat)

fcdf <- st_transform(fcdf, "+proj=longlat +datum=WGS84")

geom_polygon(aes(x = long, y = lat, group = group, color = group),
             data = fc) +

# reproject to match google maps
fc <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))

# rearrange feature class to plot with ggmap
# use broom:: tidy is this is deprecated
fc <- fortify(fc)

#read the feature class
fc <- readOGR(dsn = gdbase,layer="OL_Chinook_Unfinished_1")

# transform to same coordinates as google maps
#fcdf <- st_transform(fcdf, 3857) # bounding box only
#fcdf <- st_transform(fcdf, "+proj=longlat +datum=WGS84")

# make outlook group factor for fill
fctidy$OL <- as.factor(fctidy$OL)

# Define a function to fix the bbox to be in EPSG:3857
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster/50844502#50844502
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function to make the bounding box in the right CRS
basemap <- ggmap_bbox(basemap)

#coord_sf(crs = st_crs(3857)) + # tell ggplot2 map to be in 3857
geom_sf(data = fcdf, aes(fill = OL),
        inherit.aes = FALSE, alpha = 0.5) +

fcdf <- st_transform(fcdf, 3857) # bounding box only

# Define a function to fix the bbox to be in EPSG:3857
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster/50844502#50844502
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function to make the bounding box in the right CRS
basemap <- ggmap_bbox(basemap)

coord_sf(crs = st_crs(3857)) + # tell ggplot2 map to be in 3857

#
#   # simplify polygons without errors
#   shpChinook <- gSimplify(shpChinook, 100, topologyPreserve = TRUE)
#   shpChinook <- gBuffer(shpChinook, byid = TRUE, width = 0)
#
#   # change into usable dataframe
#   shpChinook <- spTransform(shpChinook, CRS("+proj=longlat +datum=WGS84"))
#
#   # make shapefile usable by ggplot
#   shpChinook <- tidy(shpChinook)

# #read the feature class
# fc <- readOGR(dsn = gdbase,layer="OL_Chinook_Unfinished_1")
#
# # tidy feature class to plot with ggmap
#  fctidy <- tidy(fc)


# location of folder with shapefiles
### change to working directory in package
#shapefileFolder <- "C:\\Users\\andersoned\\Documents\\SVN\\trunk\\RPackages\\salmonOutlook\\inst\\shapefiles"

# #read in  shapefile
# shpChinook <- readOGR(shapefileFolder, "OL_Chinook_Unfinished_1",
#                       verbose = FALSE, stringsAsFactors = FALSE)

# # read feature class as data frame
# fcdf <- st_read(shapefileFolder, "OL_Chinook_Unfinished_1")

geom_polygon(aes(x = long, y = lat, group = group),
             data = fctidy)

# read in feature class for conservation unit polygons
geodatabase <- "C:\\Users\\andersoned\\Documents\\Salmon Outlook Map\\SalmonOutlookLite2018\\Salmon_Outlook_Maps.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(geodatabase)
print(fc_list)

#read the feature class
fc <- readOGR(dsn = geodatabase,layer="OL_Chinook_Unfinished_1")

# Determine the FC extent, projection, and attribute information
summary(fc)

# transform to google maps geographic coordinates
fcWGS <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))

# View the feature class
plot(fcWGS)

# # use to add scalebar (I thought it was too busy so I removed it)
#   scalebar(location = "topright", y.min = 48.15, y.max = 56.2, st.bottom = TRUE,
#            x.min = -130, x.max = -119.5, dist = 100, dd2km = TRUE,
#            model = 'WGS84', st.dist = 0.03, st.size = 3)

# add north arrow
north2(ggp = outlookMap, x = 0.78, y = 0.88, scale = 0.08, symbol = 2)

# open pdf since ggsave does not work with ggsave
pdf()

pdf(file = mapname,width = 5.07, height = 5.07)
north2(ggp = outlookMap, x = 0.78, y = 0.88, scale = 0.08, symbol = 2)
dev.off()

annotate("text", x = -129, y = 48.2, label = createDate, size = 3) +

ggmap(basemap, extent = "device") +
  geom_image(data = SpeciesLocated, aes(x = long, y = lat, image = max)) +
  geom_image(data = SpeciesLocated, aes(x = long, y = lat, image = min)) +
  geom_image(data = SpeciesLocated, aes(x = long, y = lat, image = border)) +
  annotate("text", x = SpeciesLocated$long, y = SpeciesLocated$lat,
           label = SpeciesLocated$OL)
geom_text(data = SpeciesLocated,
          aes(x = long, y = lat, label = OL, size = 14),
          show.legend = FALSE)

geom_point(aes(x = long, y = lat),
           data = SpeciesLocated,
           alpha = 0.5) +

library(sf)
library(tidyverse)
library(viridis)
library(rvest)

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  scale_fill_viridis("Area") +
  ggtitle("Area of counties in North Carolina") +
  theme_bw()

library(ggplot2)
library(ggmap)
library(sf)
#> Linking to GEOS 3.6.2, GDAL 2.3.0, proj.4 5.1.0

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
nc_3857 <- st_transform(nc, 3857)

map <- get_map("north carolina", maptype = "satellite", zoom = 6, source = "google")

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(map)

ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = nc_3857, aes(fill = AREA), inherit.aes = FALSE)
