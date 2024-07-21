#' OSM data for landuse/landcover.
#'
#' Download data from OpenStreetMap for landuse/landcover analysis.
#' @param area An sf object or the name of an area.
#' @return An sf object with landuse data
#' @examples
#' lujan <- get_osmlanduse("Partido de Luján")
#' @export
#'
get_osmlanduse <- function(area="Partido de Luján"){
# @param elements A character vector to select the elements to download.
#  elements=c("landuse","natural","amenity","aeroway","leisure","protected_area","waterway",
#             "highway","railway")
  require(sf)
  require(osmdata)
  require(tidyverse)

# area <- "Partido de Mercedes"

# Obtiene el recuadro delimitador para solicitar los datos de OSM

  if (class(area)[1]=="sf"){
    recuadro <- st_bbox(area)
    } else if (is.character(area)){
      recuadro <- getbb(area) %>%
        t() %>%
        as.data.frame() %>%
        st_as_sf(coords = c("x","y")) %>%
        st_bbox()
    }


# Construir la consulta a Overpass

area_opq <- opq(recuadro,timeout = 50)


### Polígonos
natural <- add_osm_feature(area_opq,key = "natural") %>%
  osmdata_sf()

landuse <- add_osm_feature(area_opq,key = "landuse") %>%
                                    osmdata_sf()

amenity <- add_osm_feature(area_opq,key = "amenity") %>%  #,
    osmdata_sf()

aeroway <- add_osm_feature(area_opq,key = "aeroway",
                                      value = "aerodrome") %>%
  osmdata_sf()

leisure <- add_osm_feature(area_opq,key = "leisure") %>%  #,
                                      #value = c("park","pitch")) %>%
  osmdata_sf()

protected <- add_osm_feature(area_opq,key = "boundary",
                             value = "protected_area") %>%
  osmdata_sf()


### Lineales

# Ríos y arroyos

waterway <- add_osm_feature(area_opq,key = "waterway") %>%
  osmdata_sf()

# highway  Calles y rutas # pendiente de resolver

highway <- add_osm_feature(area_opq,key = "highway") %>%
  osmdata_sf()

# railway

railway <- add_osm_feature(area_opq,key = "railway") %>%
                osmdata_sf()




# Convertimos los polígonos a multipolígonos, los unimos y seleccionamos el
# el id y todos los valores de la etiqueta.

natural <- bind_rows(st_cast(natural$osm_polygons,"MULTIPOLYGON"),
                     natural$osm_multipolygons) %>%
                     select(osm_id, natural) %>%
                     rename(value=natural) %>%
                     mutate(key="natural")


landuse <- bind_rows(st_cast(landuse$osm_polygons,"MULTIPOLYGON"),
                     landuse$osm_multipolygons) %>%
  select(osm_id,landuse) %>%
  rename(value=landuse) %>%
  mutate(key="landuse")

amenity <- bind_rows(st_cast(amenity$osm_polygons,"MULTIPOLYGON"),
                     amenity$osm_multipolygons) %>%
  select(osm_id,amenity) %>%
  rename(value=amenity) %>%
  mutate(key="amenity")

aeroway <- bind_rows(st_cast(aeroway$osm_polygons,"MULTIPOLYGON"),
                     aeroway$osm_multipolygons) %>%
  select(osm_id,aeroway) %>%
  rename(value=aeroway) %>%
  mutate(key="aeroway")

leisure <- bind_rows(st_cast(leisure$osm_polygons,"MULTIPOLYGON"),
                     leisure$osm_multipolygons) %>%
  select(osm_id,leisure) %>%
  rename(value=leisure) %>%
  mutate(key="leisure")

protected <- bind_rows(st_cast(protected$osm_polygons,"MULTIPOLYGON"),
                       protected$osm_multipolygons) %>%
  select(osm_id,boundary) %>%
  rename(value=boundary) %>%
  mutate(key="protected")




waterway <- bind_rows(waterway$osm_lines,
                      waterway$osm_multilines) %>%
  select( osm_id,waterway) %>%
  rename(value=waterway) %>%
  mutate(key="waterway")

highway <- highway$osm_lines %>%
  select(osm_id,highway) %>%
 rename(value=highway) %>%
   mutate(key="highway")

railway <- railway$osm_lines %>%
  select(osm_id,railway) %>%
  rename(value=railway) %>%
  mutate(key="railway")

osmlanduse <- bind_rows(natural,landuse,amenity,aeroway,
                        leisure,protected,
                      waterway,highway,railway) %>%
  st_make_valid()

recuadro <- st_set_crs(recuadro,st_crs(osmlanduse)) %>%
  st_as_sfc()


osmlanduse <- st_intersection(osmlanduse, recuadro)

osmlanduse

}
