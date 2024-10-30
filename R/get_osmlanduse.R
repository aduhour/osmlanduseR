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
#' amenity, aeroway, leisure and protected_area.
#'
#' @return An sf object with land use data.
#' @examples
#' lujan <- get_osmlanduse()
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
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @export
#'
get_osmlanduse <- function(area="Partido de Lujan", crop_to = "area"){

# Gets bounding box to query data to OSM

  if (class(area)[1]=="sf"){
      area <- st_transform(area,4326)
      bbox <- st_bbox(area)

    } else if (is.character(area)){

      area <- getbb(area, format_out = "sf_polygon")

      bbox <- st_bbox(area)

    }


# Build Overpass query

area_opq <- opq(bbox,timeout = 50)


### Download polygons
natural <- add_osm_feature(area_opq,key = "natural") |>
  osmdata_sf()

landuse <- add_osm_feature(area_opq,key = "landuse") |>
  osmdata_sf()

amenity <- add_osm_feature(area_opq,key = "amenity") |>
  osmdata_sf()

aeroway <- add_osm_feature(area_opq,key = "aeroway",
                                      value = "aerodrome") |>
  osmdata_sf()

leisure <- add_osm_feature(area_opq,key = "leisure") |>
  osmdata_sf()

protected <- add_osm_feature(area_opq,key = "boundary",
                             value = "protected_area") |>
  osmdata_sf()


## Download linear elements


### Commented until usage has been implemented.

## waterway

# waterway <- add_osm_feature(area_opq,key = "waterway") |>
#   osmdata_sf()
#
# # highway
#
# highway <- add_osm_feature(area_opq,key = "highway") |>
#   osmdata_sf()
#
# # railway
#
# railway <- add_osm_feature(area_opq,key = "railway") |>
#                 osmdata_sf()


# Unify the geometry type into multipolygons
# Select columns osm_id and key values.

if(nrow(natural$osm_polygons) > 0 & !is.null(natural$osm_multipolygons)){
  natural <- bind_rows(st_cast(natural$osm_polygons,"MULTIPOLYGON"),
                     natural$osm_multipolygons) |>
    select(.data$osm_id, natural) |>
    rename(value=natural) |>
    mutate(key="natural")
} else if(nrow(natural$osm_polygons) > 0){
   natural <- st_cast(natural$osm_polygons,"MULTIPOLYGON") |>
     select(.data$osm_id, natural) |>
     rename(value=natural) |>
    mutate(key="natural")
} else if(!is.null(natural$osm_multipolygons)){
  natural <- natural$osm_multipolygons |>
  select(.data$osm_id, natural) |>
  rename(value=natural) |>
  mutate(key="natural")
} else {
  natural <- NULL
}

if(nrow(landuse$osm_polygons) > 0 & !is.null(landuse$osm_multipolygons)){
  landuse <- bind_rows(st_cast(landuse$osm_polygons,"MULTIPOLYGON"),
                     landuse$osm_multipolygons) |>
    select(.data$osm_id,landuse) |>
    rename(value=landuse) |>
    mutate(key="landuse")
} else if(nrow(landuse$osm_polygons) > 0){
  landuse <- st_cast(landuse$osm_polygons,"MULTIPOLYGON") |>
    select(.data$osm_id,landuse) |>
    rename(value=landuse) |>
    mutate(key="landuse")
} else if(!is.null(landuse$osm_multipolygons)){
  landuse <- landuse$osm_multipolygons |>
  select(.data$osm_id,landuse) |>
  rename(value=landuse) |>
  mutate(key="landuse")
} else {
  landuse <- NULL
}



if(nrow(amenity$osm_polygons) > 0 & !is.null(amenity$osm_multipolygons)){
  amenity <- bind_rows(st_cast(amenity$osm_polygons,"MULTIPOLYGON"),
                     amenity$osm_multipolygons) |>
    select(.data$osm_id,amenity) |>
    rename(value=amenity) |>
    mutate(key="amenity")
} else if(nrow(amenity$osm_polygons) > 0){
  amenity <- st_cast(amenity$osm_polygons,"MULTIPOLYGON") |>
    select(.data$osm_id,amenity) |>
    rename(value=amenity) |>
    mutate(key="amenity")
} else if(!is.null(amenity$osm_multipolygons)){
  amenity <- amenity$osm_multipolygons |>
  select(.data$osm_id,amenity) |>
  rename(value=amenity) |>
  mutate(key="amenity")
} else {
  amenity <- NULL
}

if(nrow(aeroway$osm_polygons) > 0 & !is.null(aeroway$osm_multipolygons)){
  aeroway <- bind_rows(st_cast(aeroway$osm_polygons,"MULTIPOLYGON"),
                     aeroway$osm_multipolygons) |>
    select(.data$osm_id,aeroway) |>
    rename(value=aeroway) |>
    mutate(key="aeroway")
} else if(nrow(aeroway$osm_polygons) > 0){
  aeroway <- st_cast(aeroway$osm_polygons,"MULTIPOLYGON") |>
    select(.data$osm_id,aeroway) |>
    rename(value=aeroway) |>
    mutate(key="aeroway")
} else if(!is.null(aeroway$osm_multipolygons)){
  aeroway <- aeroway$osm_multipolygons |>
  select(.data$osm_id,aeroway) |>
  rename(value=aeroway) |>
  mutate(key="aeroway")
} else {
  aeroway <- NULL
}

if(nrow(leisure$osm_polygons) > 0 & !is.null(leisure$osm_multipolygons)){
  leisure <- bind_rows(st_cast(leisure$osm_polygons,"MULTIPOLYGON"),
                     leisure$osm_multipolygons) |>
    select(.data$osm_id,leisure) |>
    rename(value=leisure) |>
    mutate(key="leisure")
} else if(nrow(leisure$osm_polygons) > 0){
  leisure <- st_cast(leisure$osm_polygons,"MULTIPOLYGON") |>
    select(.data$osm_id,leisure) |>
    rename(value=leisure) |>
    mutate(key="leisure")
} else if(!is.null(leisure$osm_multipolygons)){
  leisure <- leisure$osm_multipolygons |>
    select(.data$osm_id,leisure) |>
    rename(value=leisure) |>
    mutate(key="leisure")
} else {
  leisure <- NULL
}

if(nrow(protected$osm_polygons) > 0 & !is.null(protected$osm_multipolygons)){
protected <- bind_rows(st_cast(protected$osm_polygons,"MULTIPOLYGON"),
                       protected$osm_multipolygons) |>
  select(.data$osm_id,.data$boundary) |>
  rename(value=boundary) |>
  mutate(key="protected")
} else if(nrow(protected$osm_polygons) > 0){
  protected <- st_cast(protected$osm_polygons,"MULTIPOLYGON") |>
    select(.data$osm_id,.data$boundary) |>
    rename(value=boundary) |>
    mutate(key="protected")
} else if(!is.null(protected$osm_multipolygons)){
  protected <- protected$osm_multipolygons |>
    select(.data$osm_id,.data$boundary) |>
    rename(value=boundary) |>
    mutate(key="protected")
} else {
  protected <- NULL
}

### Commented until usage has been implemented.
#
# if(nrow(waterway$osm_lines) > 0 | !is.null(waterway$osm_multilines)){
# waterway <- bind_rows(waterway$osm_lines,
#                       waterway$osm_multilines) |>
#   select(.data$osm_id,waterway) |>
#   rename(value=waterway) |>
#   mutate(key="waterway")
# }
#
# if(nrow(highway$osm_lines) > 0){
# highway <- highway$osm_lines |>
#   select(.data$osm_id,highway) |>
#  rename(value=highway) |>
#    mutate(key="highway")
# }
#
#
# if(nrow(highway$osm_lines)> 0){
# railway <- railway$osm_lines |>
#   select(.data$osm_id,railway) |>
#   rename(value=railway) |>
#   mutate(key="railway")
# }

osmlanduse <- bind_rows(natural,landuse,amenity,aeroway,
                        leisure,protected) |> # waterway,highway,railway) |>
  st_make_valid()

# Set crs and converts bounding box to sf

bbox <- st_set_crs(bbox,st_crs(osmlanduse)) |>
  st_as_sfc()

if (crop_to == "area"){
  osmlanduse <- st_intersection(osmlanduse, area)
} else if (crop_to == "bbox"){
  osmlanduse <- st_intersection(osmlanduse, bbox)

}

osmlanduse

}
