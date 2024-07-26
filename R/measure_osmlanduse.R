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

  #units <- as_units(units)

  is.empty <- function(x){
    if(identical(x,integer(0))){
      result <- TRUE
    } else result <- FALSE
    result
  }

  # --------------------------------
  # provisorio hasta pulir la función
  # library(sf)
  # library(tidyverse)
  # library(units)
  #
 #  osmlanduse <-  st_read("../data/lujan.geojson")
  # crs <- 5347
  # units="ha"

  # --------------------------------

  #osmlanduse <- st_crs(osmlanduse,4326)

  # 5347 es Posgar2007
  # Completar por que se usa

  osmlanduse <-  st_transform(osmlanduse,crs)

  osmlanduse <-  mutate(osmlanduse, area=st_area(osmlanduse))

  units(osmlanduse$area) <- units

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

  # vector de las filas que se superponen con otras

   not.overlap <- sapply(overlaps,FUN = is.empty)

  # si hay polígonos que se superponen, continuamos con el procedimiento

    if(sum(not.overlap) < nrow(osmlanduse)){

      osmlanduse.not.overlap <- osmlanduse[not.overlap,] # polígonos que no se superponen

      osmlanduse.overlap <- osmlanduse[!not.overlap,] # polígonos que se superponen

      overlaps <- st_overlaps(osmlanduse.overlap) # recrea el índice de polígonos que se superponn

      for (i in 1:nrow(osmlanduse.overlap)){

        if(length(overlaps[[i]]==1)){

          if (osmlanduse.overlap$area[i,] > osmlanduse.overlap$area[overlaps[[i]],]){

            osmlanduse.overlap[i,] <- st_difference(osmlanduse.overlap[i,], st_union(osmlanduse.overlap[overlaps[[i]],]))

          } else {

             osmlanduse.overlap[i,] <-  st_difference(osmlanduse.overlap[overlaps[[i]],], st_union(osmlanduse.overlap[i,]))

          }
        }
      }
      osmlanduse <- bind_rows(osmlanduse.overlap,osmlanduse.not.overlap)
    }
osmlanduse
}

#
# i <- 14
#
# areaosmlandus <-  st_area(osmlanduse[i,]) > st_area(osmlanduse[osmlanduse_overlap[[i]],])
# length(areaosmlandus)
# }
# for (i in 1:5){ #nrow(osmlanduse_overlap)){
#
#     if (!identical(osmlanduse_overlap[[i]],integer(0))){
#
#         if (st_area(osmlanduse[i,]) > st_area(osmlanduse[osmlanduse_overlap[[i]],])){
#           osmlanduse[i,] <- st_difference(osmlanduse[i,], osmlanduse[osmlanduse_overlap[[i]],])
#         } else {
#           osmlanduse[osmlanduse_overlap[[i]],] <-  st_difference(osmlanduse[osmlanduse_overlap[[i]],],
#                                                                 osmlanduse[i,])
#         }
#
#       }
#     }
#   }




  # natural <- st_transform(natural, 5347)
  # water <- st_transform(water, 5347)
  # landuse <- st_transform(landuse,5347)
  # amenity <- st_transform(amenity,5347)
  # leisure <- st_transform(leisure, 5347)
  # railway <- st_transform(railway,5347)
  # waterway <- st_transform(waterway,5347)
  # highway <-  st_transform(highway,5347)


  ## Recortar no sería necesario porque lo debería hacer get_osmlanduse

  # Recortar el mapa a la subcuenca C


  #
  # subcuencaC_5347 <- st_transform(subcuencaC,5347)
  #
  # natural <- st_intersection(natural,subcuencaC_5347)
  # water <- st_intersection(water,subcuencaC_5347)
  # landuse <- st_intersection(landuse,subcuencaC_5347)
  # amenity <- st_intersection(amenity,subcuencaC_5347)
  # leisure <- st_intersection(leisure,subcuencaC_5347)
  # railway <- st_intersection(railway,subcuencaC_5347)
  # waterway <- st_intersection(waterway,subcuencaC_5347)
  # highway <- st_intersection(highway,subcuencaC_5347)


## Remover superposición






  ### REMOVER SUPERPOSICIÓN



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
#
#
#   # Remover polígonos de menos de 2 ha
#   # Es necesario?
#
#   # mapa_landuse_urbano <- filter(mapa_landuse_urbano,as.numeric(area_ha) >=2)
