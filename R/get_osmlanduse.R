#' Landuse data
#'
#' Download data from OpenStreetMap útil para el mapa de usos del suelo.
#' @param area El área o región a descargar
#' @return Un objeto sf (tabla)
#' @examples
#' use <- get_osmlanduse("Partido de Luján")
#' @export
#'
get_osmlanduse <- function(area){

  require(sf,osmdata,dplyr)

  #subcuencaC <- st_read("../../Mapas de uso  Cuenca Rio Lujan/Cuenca Lujan Completa/subcuencas/INA/SubCuencaC.geojson")
  #area <- subcuencaC
  #area <- "Partido de Luján"

  # Obtiene el recuadro envolvente para solicitar los datos de OSM

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


# Consultar elementos disponibles en el área a estudiar


# --------- por el momento consultamos los que queremos.



#   Definir categorías de uso o cobertura a agrupar en categorías más grandes
#   Siguiendo Volante 2009 (que se basa en FAO) y Urban atlas (ver si es necesario
#   en las categorías que tienen menos definición) para las categorías
#   y Fonte2016 para convertir las categorías de OSM

#   1.    Áreas cultivadas                            Clave     Valor
#   1.1   Cultivo de leñosas no forestales. Frutales  landuse   orchard
#   1.2   Cultivos forestales                         landuse   forest
#   1.3   Cultivo de herbáceas
#   1.3.1 Cultivos anuales                            landuse   farmland, greenhouse_horticulture
#   1.3.2 Pasturas                                    landuse   meadow

#   2.    Áreas naturales o seminaturales de vegetación terrestre
#   2.1   Cobertura de arbóreas                       natural   wood
#   2.2   Arbustos                                    natural   scrub
#   2.3   Pastizales                                  natural   grassland

#   3.    Vegetación natural en áreas
#         regularmente inundadas                      natural   wetland +
#   3.1   Cobertura de arbóreas                       natural   wood
#   3.2   Arbustos                                    natural   scrub
#   2.3   Pastizales                                  natural   grassland

#   4.    Superficies artificiales y áreas asociadas (Volante 2007 y ver si va Urban atlas)

#   4.1   Superficies construidas.
#   4.1.1 Áreas urbanas                               landuse   residential,garages

#   4.1.2 Construcciones industriales, de servicios  landuse   industrial, commercial,
#                                                               military, cemetery, retail, railway,
#                                                               harbour.
#                                                     leisure   marina
#                                                     amenity   prison, school

#   4.1.3 Construcciones y vivienda rural             landuse   farmyard

#   4.1.4 Ferrocarriles y vías de circulación.        pendiente de resolver

#   4.1.5 Extracción de minerales, basureros y        landuse   construction, landfill,
#         áreas en construcción.                                quarry, depot, brownfield
#
#   4.2   Superficies no construidas
#   4.2.1 Áreas artificiales con vegetación           landuse   greenfield,
#         (no agrícola)                                         grass
#   4.2.2 Parques urbanos                             leisure   park
#   4.2.3 Campos deportivos.                          landuse   recreation_ground
#                                                     leisure   sport, golf_course


#   5.    Cuerpos de agua
#   5.1   Artificiales
#   5.1.1 En movimiento                               waterway  canal, ditch, drain
#   5.1.2 Estacionarios                               natural   water
#                                                     water     wastewater, pond, reservoir
#   5.2   Naturales
#   5.1.1 En movimiento:Ríos y arroyos.               waterway stream, river,
#   5.1.2 Estacionarios.                              natural   water
#                                                     water     lake
#
#   6.2   Humedales, pantanos o esteros.              natural   wetland

#   7.    Tierras usadas para conservación            boundary  protected_area
#         y recreación.

#   ----------------------------------------


# --------------------------------------------------



# -----------------------------------
# Agregar elementos a consultar
# -----------------------------------

# Consultamos en grupos por clave (landuse, natural, amenity, leisure,
# boundary, highway y railway)
# incluyendo los valores de interés

#  Vegetación o tipo de superficie.
#  Etiqueta natural=*
#  Toma los valores que existen en https://wiki.openstreetmap.org/wiki/Key:natural
#  para elementos de relacionados con la vegetación o el tipo de superficie.


area_natural <- add_osm_feature(area_opq,key = "natural",
                                      value = c("wood","grassland",
                                                "wetland", "scrub", "heath",
                                                "bare_rock", "sand")) %>%
  osmdata_sf()


#  Etiqueta natural=water
#  Etiqueta natural=*
#  Toma los valores que existen en https://wiki.openstreetmap.org/wiki/Key:natural
#  para elementos de relacionados con la vegetación o el tipo de superficie.


# area_water <- add_osm_feature(subC_opq,key = "water",
#                                   value = c("pond","river","wastewater",
#                                              "reservoir", "lake")) %>%
# osmdata_sf()

# Probar esta alternativa y después separar los tipos
area_water <- add_osm_feature(area_opq,key = "natural",
                                    value = "water") %>%
  osmdata_sf()


# waterway

area_waterway <- add_osm_feature(area_opq,key = "waterway",
                                       value = c("canal","ditch","drain",
                                                 "river", "stream")) %>%
  osmdata_sf()

# -------------------------------------------
# Etiqueta landuse=*
# -------------------------------------------

area_landuse <- add_osm_feature(area_opq,key = "landuse",
                                      value = c("commercial","construction",
                                                "education", "industrial","residential",
                                                "retail", "allotments", "farmland",
                                                "farmyard","forest", "meadow",
                                                "orchard",
                                                "reservoir",
                                                "cemetery", "depot","grass",
                                                "greenfield","greenhouse_horticulture",
                                                "landfill", "military", "plant_nursery",
                                                "quarry","railway","recreation_ground",
                                                "religious")) %>%
  osmdata_sf()

# amenity

area_amenity <- add_osm_feature(area_opq,key = "amenity",
                                      value = c("prison","school","university",
                                                "social_facility")) %>% #Es por el Capitán Sarmiento. Revisar si va otra etiqueta
  osmdata_sf()

# Aeródromos y aeropuertos.

area_aeroway <- add_osm_feature(area_opq,key = "aeroway",
                                      value = "aerodrome") %>%
  osmdata_sf()


# leisure

area_leisure <- add_osm_feature(area_opq,key = "leisure",
                                      value = c("park","pitch")) %>%
  osmdata_sf()


# highway  Calles y rutas # pendiente de resolver

# area_highway <- add_osm_feature(area_opq,key = "highway",
#                                      value = c("residential","unclassified",
#                                                "primary","secondary","tertiary",
#                                                "trunk","motorway","construction")) %>%
#  osmdata_sf()

# railway

area_railway <- add_osm_feature(area_opq,key = "railway",
                                      value = c("rail","disused",
                                                "preserved")) %>%
                osmdata_sf()



# Conservación

area_protected <- add_osm_feature(area_opq,key = "boundary",
                                        value = "protected_area") %>%
  osmdata_sf()


# Convertimos los polígonos a multipolígonos, los unimos y seleccionamos el nombre,
# el id y todos los valores de la etiqueta.

# se seleccionan las columnas de interés. Son estas?

natural <- bind_rows(st_cast(area_natural$osm_polygons,"MULTIPOLYGON"),
                     area_natural$osm_multipolygons) %>%
                     select(name,osm_id, natural)


water <- bind_rows(st_cast(area_water$osm_polygons,"MULTIPOLYGON"),
                   area_water$osm_multipolygons) %>%
  select(name,osm_id,water) %>%
  rename(value=water) %>%
  mutate(key="water")


landuse <- bind_rows(st_cast(area_landuse$osm_polygons,"MULTIPOLYGON"),
                     area_landuse$osm_multipolygons) %>%
  select(name,osm_id,landuse) %>%
  rename(value=landuse) %>%
  mutate(key="landuse")

amenity <- bind_rows(st_cast(area_amenity$osm_polygons,"MULTIPOLYGON"),
                     area_amenity$osm_multipolygons) %>%
  select(name,osm_id,amenity) %>%
  rename(value=amenity) %>%
  mutate(key="amenity")

aeroway <- bind_rows(st_cast(area_aeroway$osm_polygons,"MULTIPOLYGON"),
                     area_aeroway$osm_multipolygons) %>%
  select(name,osm_id,aeroway) %>%
  rename(value=aeroway) %>%
  mutate(key="aeroway")

leisure <- bind_rows(st_cast(area_leisure$osm_polygons,"MULTIPOLYGON"),
                     area_leisure$osm_multipolygons) %>%
  select(name,osm_id,leisure) %>%
  rename(value=leisure) %>%
  mutate(key="leisure")

protected <- bind_rows(st_cast(area_protected$osm_polygons,"MULTIPOLYGON"),
                       area_protected$osm_multipolygons) %>%
  select(name,osm_id,boundary) %>%
  rename(value=boundary) %>%
  mutate(key="protected")

railway <- area_railway$osm_lines %>%
  select(name,osm_id,railway) %>%
  rename(value=railway) %>%
  mutate(key="railway")

waterway <- bind_rows(area_waterway$osm_lines,
                      area_waterway$osm_multilines) %>%
  select(name, osm_id,waterway) %>%
  rename(value=waterway) %>%
  mutate(key="waterway")

# highway <- area_highway$osm_lines %>%
#  select(name, osm_id,highway) %>%
# rename(value=highway) %>%
#   mutate(key="highway")

osmlanduse <- bind_rows(natural,water,landuse,amenity,aeroway,
                        leisure,protected,
                      waterway,railway) %>%
  st_make_valid()

recuadro <- st_set_crs(recuadro,st_crs(osmlanduse)) %>%
  st_as_sfc()


osmlanduse <- st_intersection(osmlanduse, recuadro)

osmlanduse

}
