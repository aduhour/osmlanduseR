#' Measure Landuse
#'
#' Measure area polygons of osm landuse removing overlapping
#' @param osmlanduse Output of get_osmlanduse.
#' @param crs Set the CRS to measure area.
#' @param units The units for the area measures.
#' @return An sf object with area measures
#' @examples
#' landuse <- get_osmlanduse()
#' measures <- measure_osmlanduse(landuse)
#' @export
measure_osmlanduse <- function(osmlanduse, crs=5347, units="ha"){

  require(sf)
  require(tidyverse)
  require(units)


  #osmlanduse <- st_crs(osmlanduse,4326)

  # 5347 es Posgar2007
  # Completar por que se usa

  osmlanduse <-  st_transform(osmlanduse,crs)

  # medimos el área en este punto para utilizar después
  # al remover superposición

  osmlanduse.area <-  st_area(osmlanduse)
  # ------------------------------------------------------------------
  # Realizamos la diferecia para eliminar superposiciones
  # ------------------------------------------------------------------
  # Sugiere eliminar superposición priorizando áreas más pequeñas.

  # Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf,
  # A. Open land cover from OpenStreetMap and remote sensing.
  # International Journal of Applied Earth Observationd and Geoinformation, 2017

  #  Issues of overlapping features were resolved by prioritizing smaller
  # polygons over larger ones (Fig. 3). This is essential for the calculation of
  # correct area statistics or potential comparisons to other data, and gives
  # priority to the detail in the map.

  # qué pares de polígonos se superponen?

   overlaps <- st_overlaps(osmlanduse)

  # vector de las filas que se superponen con otras, que tienen una sola
  # superposición

   overlap <- lengths(overlaps) == 1

  # si hay polígonos que se superponen, continuamos con el procedimiento

    if(sum(overlap) > 0){

      osmlanduse.not.overlap <- osmlanduse[!overlap,] # polígonos que no se superponen

      osmlanduse.overlap <- osmlanduse[overlap,] # polígonos que se superponen

      overlaps.unique <- as.data.frame(st_overlaps(osmlanduse,retain_unique=TRUE)) # recrea el índice de polígonos que se superponn

      for (i in 1:nrow(overlaps.unique)){

        x <- as.integer(overlaps.unique[i,][1])
        y <- as.integer(overlaps.unique[i,][2])

        if (osmlanduse.area[x] < osmlanduse.area[y]){

            overlaps.unique[i,][1] <- y
            overlaps.unique[i,][2] <- x

        }
      }

      osmlanduse.overlap <- st_difference(osmlanduse.overlap,st_union(osmlanduse[overlaps.unique[,2],]))

      osmlanduse <- bind_rows(osmlanduse.overlap,osmlanduse.not.overlap) # ver si reemplazo por una función base
}
   area <- st_area(osmlanduse) # Removida la superposición, calcula el área

   units(area) <- units

   osmlanduse <- cbind(osmlanduse,area)

   osmlanduse
}

  # Prioridades en caso de superposición

  #  Prioridad            Clase           Descripción

  #       1               4.1.2 y 4.1.4   Construcciones industriales, de servicios, y red vial

  #       2               5 y 3           Cuerpos de agua y humedales

  #       3               4.2             Superficies artificiales no construidas

  #       4               4.1.5           Extracción de minerales, basureros y
  #                                       áreas en construcción.

  #       5               4.1.1 y 4.1.3   Áreas urbanas, construcciones y vivienda rural.


  #       6               1.2             Cultivos forestales

  #       7               2               Áreas naturales o seminaturales de vegetación terrestre

  #       7               1.1 y 1.3       Áreas cultivadas, menos cultivos forestales




  ###
  #   Para hacer operaciones espaciales entre cada tipo de uso se puede
  #   dividir cada mapa en múltiples mapas por tipo de 'landuse' or 'natural'

  #   Hago esto con la función split y guardando en una lista en la que cada
  #   objeto se puede acceder usando el operador $

#
#   landuse_list  <- landuse %>% split(.$Tipo)
#   natural_list  <- natural %>% split(.$Tipo)
#   leisure_list  <- leisure %>% split(.$Tipo)
#   amenity_list  <- amenity %>% split(.$Tipo)
#   waterway_list <- waterway %>% split(.$Tipo)
#   water_list    <- water %>% split(.$Tipo)
#
#
#   # --------------------
#   # Creamos las capas de objetos según la clasificación
#   # --------------------
#
#   # Cultivadas
#
#
#   cultivadas <- with(landuse_list, list(anuales=farmland,
#                                         pasturas=meadow,
#                                         forestales=forest,
#                                         frutales=orchard)) %>%
#     lapply(transform, Leyenda_1 = "Áreas cultivadas")
#
#   cultivadas$anuales <- mutate(cultivadas$anuales, Leyenda_2="Cultivos anuales")
#   cultivadas$frutales <- mutate(cultivadas$frutales, Leyenda_2="Frutales")
#   cultivadas$forestales <- mutate(cultivadas$forestales, Leyenda_2="Forestales")
#   cultivadas$pasturas <- mutate(cultivadas$pasturas, Leyenda_2="Pasturas")
#
#
#   # Naturales
#
#   naturales <- with(natural_list,list(wood=wood,grassland=grassland,scrub=scrub)) %>%
#     lapply(transform,Leyenda_1 = "Áreas naturales")
#
#   naturales$wood <- mutate(naturales$wood, Leyenda_2 = "Árboles")
#
#   naturales$grassland <- mutate(naturales$grassland, Leyenda_2 = "Herbáceas")
#
#   naturales$scrub <- mutate(naturales$scrub, Leyenda_2 = "Arbustos")
#
#
#   # Humedales
#
#   humedales <- natural_list$wetland %>%
#     mutate(Leyenda_1 = "Humedales") %>%
#     mutate(Leyenda_2 = "Humedales")
#
#   #artificiales
#
#   industrialyservicios = list(industrial = landuse_list$industrial,
#                               #comercial = landuse_list$commercial,
#                               #cementerio = landuse_list$cemetery,
#                               #retail = landuse_list$retail,
#                               ferrocarril = landuse_list$railway,
#                               invernaderos = landuse_list$greenhouse_horticulture,
#                               escuelas = amenity_list$school,
#                               #universidad = amenity_list$university,
#                               #penitenciario = amenity_list$prison,
#                               #resolver lo de social_facility
#                               deposito = landuse_list$depot) %>%
#     lapply(transform, Leyenda_1 = "Superficies artificiales") %>%
#     lapply(transform, Leyenda_2 = "Construcciones industriales y de servicios")
#
#   urbano = landuse_list$residential %>%
#     mutate(Leyenda_1 = "Superficies artificiales") %>%
#     mutate(Leyenda_2 = "Áreas urbanas")
#
#   rural = landuse_list$farmyard %>%
#     mutate(Leyenda_1 = "Superficies artificiales") %>%
#     mutate(Leyenda_2 = "Infraestructura y vivienda rural")
#
#   extractivasyconstruccion = list(construccion = landuse_list$construction, #) %>%
#                                   cantera = landuse_list$quarry,
#                                   basural = landuse_list$landfill)%>%
#     lapply(transform, Leyenda_1 = "Superficies artificiales") %>%
#     lapply(transform, Leyenda_2 = "Áreas de extracción de minerales, disposición de residuos y en construcción")
#
#
#   noconstruidas = list(vegetacion=bind_rows(landuse_list$greenfield,
#                                             landuse_list$grass),
#                        parques=leisure_list$park,
#                        camposdeportivos = leisure_list$pitch)%>%
#     lapply(transform, Leyenda_1 = "Superficies artificiales") %>%
#     lapply(transform, Leyenda_2 = "Superficies no construidas, con vegetación no agrícola.")
#
#
#   # Cuerpos de agua
#   cuerpos_de_agua_artificiales = list(zanja = waterway_list$ditch,
#                                       #waterway_list$canal,
#                                       canal_desagüe = waterway_list$drain,
#                                       # water_list$wastewater,
#                                       lago_artificial = water_list$pond) %>%
#     lapply(transform, Leyenda_1 = "Cuerpos de agua") %>%
#     lapply(transform, Leyenda_2 = "Cuerpos de agua artificiales")
#
#   cuerpos_de_agua_naturales = list(arroyo = waterway_list$stream,
#                                    rio = waterway_list$river,
#                                    cuerpo_del_rio = water_list$river) %>%
#     lapply(transform, Leyenda_1 = "Cuerpos de agua") %>%
#     lapply(transform, Leyenda_2 =" Cuerpos de agua naturales")
#
#
#
#
#   # Cultivadas
#   cultivadas$anuales <- st_difference(cultivadas$anuales,st_union(cultivadas$pasturas))
#   cultivadas$anuales <- st_difference(cultivadas$anuales,st_union(cultivadas$frutales))
#   cultivadas$anuales <- st_difference(cultivadas$anuales,st_union(cultivadas$forestales))
#
#   cultivadas$anuales <- st_difference(cultivadas$anuales,st_union(urbano))
#
#   # Artificiales
#
#   industrialyservicios$industrial <- st_difference(industrialyservicios$industrial, st_union(industrialyservicios$comercial))
#
#   urbano <- st_difference(urbano, st_union(bind_rows(industrialyservicios)))
#
#   urbano <- st_difference(urbano, st_union(bind_rows(extractivasyconstruccion)))
#
#
#   # ----------------------------------------------------
#   # Unificamos los usos del suelo en una sola tabla.
#   # ----------------------------------------------------
#
#   usos_del_suelo <- bind_rows(bind_rows(cultivadas),
#                               bind_rows(naturales),
#                               bind_rows(industrialyservicios),
#                               urbano, rural,
#                               # bind_rows(extractivasyconstruccion),
#                               bind_rows(noconstruidas),
#                               bind_rows(cuerpos_de_agua_artificiales),
#                               bind_rows(cuerpos_de_agua_naturales),
#                               humedales) %>%
#     mutate(area_ha = set_units(st_area(.),"ha"))
#
#   st_write(usos_del_suelo,paste("mapas/usos_del_suelo -",today(),".geojson"))
#
#
#   # Calcular área
#
#   subcuencaC_5347 <- mutate(subcuencaC_5347, area = st_area(subcuencaC_5347))
#

#
#   # Convertir el área a hectáreas
#
#   subcuencaC_5347 <- subcuencaC_5347 %>%
#     mutate(area_ha = set_units(area,"ha")) %>%
#     select(-area) # elimina la columna area
#
#
#
#
#   # tm_shape(subcuencaC_5347)+tm_fill(alpha = 0.5)+tm_shape(waterway$geometry)+tm_borders()
#   # tm_shape(landuse)+tm_fill()+tm_shape(natural)+tm_fill(col = "red")
#
#   #  --------------------------------
#
#   # Remover polígonos de menos de 2 ha
#   # Es necesario?
#
#   # mapa_landuse_urbano <- filter(mapa_landuse_urbano,as.numeric(area_ha) >=2)
