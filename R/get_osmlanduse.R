#' Get OSM data for land use / land cover analysis
#'
#' Build an Overpass query to retrieve OSM data for land use and
#' land cover analysis and mapping.
#'
#' @param area An sf object or the place name of an area
#' @param crop_to Character string indicating if the result should be the
#' intersection with the polygon ("area", default) or the bounding box ("bbox").
#' @details
#' The function retrieves OSM elements tagged with keys: natural, landuse, natural
#' amenity, aeroway and leisure.
#' @return A list with two sf objects:  The area and the land use data.
#' @examples
#' area <-  "Lezica y Torrezuri, Partido de Luján"
#' landuse <- get_osmlanduse(area, crop_to = "bbox")
#' @importFrom osmdata getbb
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_make_valid
#' @importFrom sf st_intersection
#' @importFrom sf st_set_crs
#' @importFrom sf st_cast
#' @importFrom sf st_crs
#' @importFrom sf st_as_sfc
#' @export
#'
get_osmlanduse <- function(area="Partido de Lujan", crop_to = "area"){

  crop_to_methods <- c("area","bbox")

  crop_to <- match.arg(crop_to,crop_to_methods)

  # Gets bounding box to query data to OSM


  if (class(area)[1]=="sf"){
      area <- sf::st_transform(area,4326)
      bbox <- sf::st_bbox(area)

    } else if (is.character(area)){

      area <- osmdata::getbb(area, format_out = "sf_polygon")

      bbox <- sf::st_bbox(area)

    } else
    {stop("area must be of class 'sf' or 'character'")}


# Build Overpass query

area_opq <- osmdata::opq(bbox,timeout = 50)


### Download polygons
natural <- osmdata::add_osm_feature(area_opq,key = "natural") |>
  osmdata_sf()

landuse <- osmdata::add_osm_feature(area_opq,key = "landuse") |>
  osmdata_sf()

amenity <- osmdata::add_osm_feature(area_opq,key = "amenity") |>
  osmdata_sf()

aeroway <- osmdata::add_osm_feature(area_opq,key = "aeroway",
                                      value = "aerodrome") |>
  osmdata_sf()

leisure <- osmdata::add_osm_feature(area_opq,key = "leisure") |>
  osmdata_sf()


## Download linear elements


### Commented until usage is implemented.

## waterway

# waterway <- osmdata::add_osm_feature(area_opq,key = "waterway") |>
#   osmdata_sf()
#
# # highway
#
# highway <- osmdata::add_osm_feature(area_opq,key = "highway") |>
#   osmdata_sf()
#
# # railway
#
# railway <- osmdata::add_osm_feature(area_opq,key = "railway") |>
#                 osmdata_sf()


# Unify the geometry type into multipolygons
# Select columns with values.

if(nrow(natural$osm_polygons) > 0 & !is.null(natural$osm_multipolygons)){

  natural <- rbind(sf::st_cast(subset(natural$osm_polygons,
                                  subset = !is.na(natural),
                                  select="natural"),"MULTIPOLYGON"),
                   subset(natural$osm_multipolygons,
                          subset = !is.na(natural),
                          select="natural"))

    names(natural)[1] <- "value"
    natural$key <- rep("natural",nrow(natural))

} else if(nrow(natural$osm_polygons) > 0){

   natural <- sf::st_cast(subset(natural$osm_polygons,
                             subset = !is.na(natural),
                             select="natural"),"MULTIPOLYGON")
   names(natural)[1] <- "value"
   natural$key <- rep("natural",nrow(natural))

} else if(!is.null(natural$osm_multipolygons)){
  natural <-  subset(natural$osm_multipolygons,
                     subset = !is.na(natural),
                     select="natural")
  names(natural)[1] <- "value"
  natural$key <- rep("natural",nrow(natural))
} else {
  natural <- NULL
}

if(nrow(landuse$osm_polygons) > 0 & !is.null(landuse$osm_multipolygons)){
  landuse <- rbind(sf::st_cast(subset(landuse$osm_polygons,
                                  subset = !is.na(landuse),
                                  select="landuse"),"MULTIPOLYGON"),
                   subset(landuse$osm_multipolygons,
                          subset = !is.na(landuse),
                          select="landuse"))

  names(landuse)[1] <- "value"
  landuse$key <- rep("landuse",nrow(landuse))

} else if(nrow(landuse$osm_polygons) > 0){

  landuse <- sf::st_cast(subset(landuse$osm_polygons,
                            subset = !is.na(landuse),
                            select="landuse"),"MULTIPOLYGON")
  names(landuse)[1] <- "value"
  landuse$key <- rep("landuse",nrow(landuse))

} else if(!is.null(landuse$osm_multipolygons)){

  landuse <- subset(landuse$osm_multipolygons,
                    subset = !is.na(landuse),
                    select="landuse")
  names(landuse)[1] <- "value"
  landuse$key <- rep("landuse",nrow(landuse))
} else {
  landuse <- NULL
}



if(nrow(amenity$osm_polygons) > 0 & !is.null(amenity$osm_multipolygons)){
  amenity <- rbind(sf::st_cast(subset(amenity$osm_polygons,
                                  subset = !is.na(amenity),
                                  select="amenity"),"MULTIPOLYGON"),
                   subset(amenity$osm_multipolygons,
                          subset = !is.na(amenity),
                          select="amenity"))

  names(amenity)[1] <- "value"
  amenity$key <- rep("amenity",nrow(amenity))

} else if(nrow(amenity$osm_polygons) > 0){

  amenity <- sf::st_cast(subset(amenity$osm_polygons,
                            subset = !is.na(amenity),
                            select="amenity"),"MULTIPOLYGON")
  names(amenity)[1] <- "value"
  amenity$key <- rep("amenity",nrow(amenity))

} else if(!is.null(amenity$osm_multipolygons)){

  amenity <- subset(amenity$osm_multipolygons,
                    subset = !is.na(amenity),
                    select="amenity")
  names(amenity)[1] <- "value"
  amenity$key <- rep("amenity",nrow(amenity))
} else {
  amenity <- NULL
}

if(nrow(aeroway$osm_polygons) > 0 & !is.null(aeroway$osm_multipolygons)){
  aeroway <- rbind(sf::st_cast(subset(aeroway$osm_polygons,
                                  subset = !is.na(aeroway),
                                  select="aeroway"),"MULTIPOLYGON"),
                   subset(aeroway$osm_multipolygons,
                          subset = !is.na(aeroway),
                          select="aeroway"))

  names(aeroway)[1] <- "value"
  aeroway$key <- rep("aeroway",nrow(aeroway))

} else if(nrow(aeroway$osm_polygons) > 0){

  aeroway <- sf::st_cast(subset(aeroway$osm_polygons,
                            subset = !is.na(aeroway),
                            select="aeroway"),"MULTIPOLYGON")
  names(aeroway)[1] <- "value"
  aeroway$key <- rep("aeroway",nrow(aeroway))

} else if(!is.null(aeroway$osm_multipolygons)){

  aeroway <- subset(aeroway$osm_multipolygons,
                    subset = !is.na(aeroway),
                    select="aeroway")
  names(aeroway)[1] <- "value"
  aeroway$key <- rep("aeroway",nrow(aeroway))
} else {
  aeroway <- NULL
}


if(nrow(leisure$osm_polygons) > 0 & !is.null(leisure$osm_multipolygons)){
  leisure <- rbind(sf::st_cast(subset(leisure$osm_polygons,
                                  subset = !is.na(leisure),
                                  select="leisure"),"MULTIPOLYGON"),
                   subset(leisure$osm_multipolygons,
                          subset = !is.na(leisure),
                          select="leisure"))

  names(leisure)[1] <- "value"
  leisure$key <- rep("leisure",nrow(leisure))

} else if(nrow(leisure$osm_polygons) > 0){

  leisure <- sf::st_cast(subset(leisure$osm_polygons,
                            subset = !is.na(leisure),
                            select="leisure"),"MULTIPOLYGON")
  names(leisure)[1] <- "value"
  leisure$key <- rep("leisure",nrow(leisure))

} else if(!is.null(leisure$osm_multipolygons)){

  leisure <- subset(leisure$osm_multipolygons,
                    subset = !is.na(leisure),
                    select="leisure")
  names(leisure)[1] <- "value"
  leisure$key <- rep("leisure",nrow(leisure))
} else {
  leisure <- NULL
}



### Commented until usage is implemented.
#
# if(nrow(waterway$osm_lines) > 0 | !is.null(waterway$osm_multilines)){
# waterway <- bind_rows(waterway$osm_lines,
#                       waterway$osm_multilines) |>
#   subset(subset = !is.na(waterway), select = c("osm_id","waterway"))
#  names(waterway)[2] <- "value"
#  waterway$key <- rep("waterway",nrow(waterway))
# }
#
# if(nrow(highway$osm_lines) > 0){
# highway <- highway$osm_lines |>
#  subset(subset = !is.na(highway),select = c("osm_id", "highway"))
#  names(highway)[2] <- "value"
#  highway$key <- rep("highway",nrow(highway))
# }
#
#
# if(nrow(highway$osm_lines)> 0){
# railway <- railway$osm_lines |>
#   subset(subset = !is.na(railway),select = c("osm_id","railway"))
#  names(railway)[2] <- "value"
#  railway$key <- rep("railway",nrow(railway))
# }

osmlanduse <- rbind(natural,landuse,amenity,aeroway,
                        leisure) # waterway,highway,railway)


# Set crs and converts bounding box to sf

bbox <- sf::st_set_crs(bbox,sf::st_crs(osmlanduse)) |>
  sf::st_as_sfc()

osmlanduse <- sf::st_make_valid(osmlanduse)

if (crop_to == "area"){
  osmlanduse <- sf::st_intersection(osmlanduse, area)
} else if (crop_to == "bbox"){
  osmlanduse <- sf::st_intersection(osmlanduse, bbox)

}

osmlanduse <-  sf::st_make_valid(osmlanduse)

osmlanduse <- list(area = area, osmlanduse=osmlanduse)

osmlanduse

}
