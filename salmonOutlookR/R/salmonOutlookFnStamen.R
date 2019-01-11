#======================================================================================================
# salmonOutlookFn.R
# By Erika Anderson
# Created August 2018

#======================================================================================================
#' Salmon Outlook Maps

#' @param species Specify which salmon species: Sockeye, Chinook, Coho, Chum, or Pink. No default.
#' @param province Choose the province, either BC or Yukon. Defaults to BC
#' @description Produces salmon outlook map for specified species, region and year combination
#' @return Returns png image with salmon outlook map
#' @keywords salmon outlook
#' @examples
#' salmonOutlook("Sockeye")
#' salmonOutlook("Chinook", "Yukon")

#' @importFrom lubridate year
#' @importFrom ggmap get_stamenmap
#' @importFrom ggplot2 geom_sf annotatev ggtitle
#' @importFrom ggimage geom_image
#' @importFrom magrittr %>% %<>%
#' @importFrom sf st_transform st_read
#' @importFrom stringr str_trim
#' @import dplyr

#' @export

salmonOutlook <- function(species,
                          province = "BC") {

  # # determine years
  # outlookYear <- year(today()) + 1
  #
  # centroids for each base map
  bc_bbox <- c(-135, 48, -115, 60)

  ykn_vec <- c(-133, 63)
  south_bc_vec <- c(-125.8, 52.5)

  # download map from google maps, keep current acording to their conditions
  # turn off labels from google to prevent clutter
  bc_map <- get_stamenmap(bc_bbox, zoom = 6,maptype = "terrain-background")
  # ykn_map <- get_googlemap(ykn_vec, zoom = 5,
  #                          style = 'feature:all|element:labels|visibility:off')
  # south_bc_map <- get_googlemap(south_bc_vec, zoom = 6,
  #                               style = 'feature:all|element:labels|visibility:off')
  #
  # #save base maps as Rdata files
  # save(bc_map, file = "basemaps/bc_map.RData")
  # save(ykn_map, file = "basemaps/ykn_map.RData")
  # save(south_bc_map, file = "basemaps/south_bc_map.RData")

  #load base maps from file
  load("basemaps/bc_map.RData")
  load("basemaps/ykn_map.RData")
  load("basemaps/south_bc_map.RData")

  # choose basemap accorting to user input
  if (toupper(province) == "BC") {basemap <- bc_map}
  if (toupper(province) == "YUKON") {basemap <- ykn_map}
  if (province == "southBC") {basemap <- south_bc_map}

  # create text to display on map
  title <- paste0(species, " Salmon Outlook ", outlookYear)
  createDate <- paste0("Created by DFO on ", Sys.Date())

  # load new trends data from csv file
  trends <- read.csv("inst/data/trends.csv")

  # convert Species and Stock to character
  trends$Species <- as.character(trends$Species)
  trends$Stock <- as.character(trends$Stock)

  #limit to species that user specified in appropriate years
  # replace 0 for no data
  trendsSpecies <- trends %>%
    mutate(., Species2 = toupper(str_trim(Species, side = "both"))) %>%
    filter(Species2 == toupper(species)) %>%
    filter(min != -9999) %>%
    mutate(., min = ifelse(min == -99, 0, min),
           max = ifelse(max == -99, 0, max),
           change = ifelse(change == -99, 0, change))

  # determine which centroid file to load
  if (toupper(species) == "PINK") {
    if ((outlookYear %% 2) == 0) {
      yearType <- "Even"
    } else {
      yearType <- "Odd" }
    centroidFile <- paste0("inst/data/", tolower(species), yearType, "Centroids.csv")
  } else {
    centroidFile <- paste0("inst/data/", species, "Centroids.csv")
    }

  #load centroid file
  centroids <- read.csv(centroidFile)

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

  # transform to same coordinates as google maps and almost works with both labels and polygons
  fc <- st_transform(fc, "+proj=longlat +datum=WGS84")
  ## transform to same coordinates as google maps, works with polygons with bounding box function exactly
  #fc <- st_transform(fc, 3857)

  # make outlook group factor for fill
  fc$OL <- as.factor(fc$OL)

  # change stock to character
  centroids$stock <- as.character(centroids$stock)

  #link centroids to outlook groups
  # chinook has two OL 42 so use composite key with stock
  if (toupper(species) == "CHINOOK") {
    SpeciesLocated <- trendsSpecies %>%
      left_join(., centroids, by = c("OL" = "ol", "Stock" = "stock"))

  } else {
    # use OL as key for other species
    SpeciesLocated <- trendsSpecies %>%
      left_join(., centroids, by = c("OL" = "ol"))}

  # add symbol filenames
  SpeciesLocated %<>%
    mutate(., symbol = paste0("inst/symbols/", min, max, change, ".png")) %>%
    filter(!is.na(lat))

  # # Define a function to fix the bbox to be in EPSG:3857
  # # https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster/50844502#50844502
  # ggmap_bbox <- function(map) {
  #   if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  #   # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  #   # and set the names to what sf::st_bbox expects:
  #   map_bbox <- setNames(unlist(attr(map, "bb")),
  #                        c("ymin", "xmin", "ymax", "xmax"))
  #
  #   # Coonvert the bbox to an sf polygon, transform it to 3857,
  #   # and convert back to a bbox (convoluted, but it works)
  #   bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  #
  #   # Overwrite the bbox of the ggmap object with the transformed coordinates
  #   attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  #   attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  #   attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  #   attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  #   map
  # }
  #
  # # Use the function to make the bounding box in the right CRS
  # basemap <- ggmap_bbox(basemap)

  # plot map
  outlookMap <- ggmap(basemap) +
    geom_sf(data = fc, aes(fill = OL), inherit.aes = FALSE, alpha = 0.5) +

    geom_image(data = SpeciesLocated, aes(x = long, y = lat, image = symbol)) +
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

  #save map
  mapname = paste0(species, outlookYear, province, ".png")
  ggsave(mapname, outlookMap, width = 5.07, height = 5.07)

}
