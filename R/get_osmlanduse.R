#' Get OSM data for landuse/landcover analysis
#'
#' Build an Overpass query to retrieve OSM data for land use and
#' land cover analysis and mapping.
#'
#' @param area An sf object or the place name of an area
#' @param crop_to Character string indicating if the result should be the
#' intersection with the polygon ("area", default) or the bounding box ("bbox").
#' @details
#' The function retrieves OSM elements tagged with keys: natural, landuse, natural
#' amenity, aeroway, leisure, protected_area, waterway, highway and railway.
#'
#' @return An sf object with landuse data.
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


### Download linear elements

# waterway

waterway <- add_osm_feature(area_opq,key = "waterway") |>
  osmdata_sf()

# highway

highway <- add_osm_feature(area_opq,key = "highway") |>
  osmdata_sf()

# railway

railway <- add_osm_feature(area_opq,key = "railway") |>
                osmdata_sf()


# Unify the geometry type into multipolygons
# Select columns osm_id and key values.

natural <- bind_rows(st_cast(natural$osm_polygons,"MULTIPOLYGON"),
                     natural$osm_multipolygons) |>
  select(.data$osm_id, natural) |>
  rename(value=natural) |>
  mutate(key="natural")


landuse <- bind_rows(st_cast(landuse$osm_polygons,"MULTIPOLYGON"),
                     landuse$osm_multipolygons) |>
  select(.data$osm_id,landuse) |>
  rename(value=landuse) |>
  mutate(key="landuse")

amenity <- bind_rows(st_cast(amenity$osm_polygons,"MULTIPOLYGON"),
                     amenity$osm_multipolygons) |>
  select(.data$osm_id,amenity) |>
  rename(value=amenity) |>
  mutate(key="amenity")

aeroway <- bind_rows(st_cast(aeroway$osm_polygons,"MULTIPOLYGON"),
                     aeroway$osm_multipolygons) |>
  select(.data$osm_id,aeroway) |>
  rename(value=aeroway) |>
  mutate(key="aeroway")

leisure <- bind_rows(st_cast(leisure$osm_polygons,"MULTIPOLYGON"),
                     leisure$osm_multipolygons) |>
  select(.data$osm_id,leisure) |>
  rename(value=leisure) |>
  mutate(key="leisure")

protected <- bind_rows(st_cast(protected$osm_polygons,"MULTIPOLYGON"),
                       protected$osm_multipolygons) |>
  select(.data$osm_id,.data$boundary) |>
  rename(value=boundary) |>
  mutate(key="protected")


waterway <- bind_rows(waterway$osm_lines,
                      waterway$osm_multilines) |>
  select(.data$osm_id,waterway) |>
  rename(value=waterway) |>
  mutate(key="waterway")

highway <- highway$osm_lines |>
  select(.data$osm_id,highway) |>
 rename(value=highway) |>
   mutate(key="highway")

railway <- railway$osm_lines |>
  select(.data$osm_id,railway) |>
  rename(value=railway) |>
  mutate(key="railway")

osmlanduse <- bind_rows(natural,landuse,amenity,aeroway,
                        leisure,protected,
                      waterway,highway,railway) |>
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
