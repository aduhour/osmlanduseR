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
  #require(tidyverse)
  require(units)


  #osmlanduse <- st_crs(osmlanduse,4326)

  # 5347 es Posgar2007
  # Completar por que se usa

  osmlanduse <-  st_transform(osmlanduse,crs)

  # medimos el área en este punto para utilizar después
  # al remover superposición


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

  overlaps <- as.data.frame(st_overlaps(osmlanduse,retain_unique=TRUE))

  # vector de las filas que se superponen con otras

   overlap <- unique(c(overlaps[,1],overlaps[,2]))

  # si hay polígonos que se superponen, continuamos con el procedimiento

    if(sum(overlap) > 0){

      osmlanduse.not.overlap <- osmlanduse[-overlap,] # polígonos que no se superponen

      osmlanduse.overlap <- osmlanduse[overlap,] # polígonos que se superponen

      osmlanduse.area <-  st_area(osmlanduse[c(overlaps[,1],overlaps[,2]),])

      for (i in 1:nrow(overlaps)){

        x <- i
        y <- i + nrow(overlaps)

        if (osmlanduse.area[x] < osmlanduse.area[y]){

            overlaps[i,][1] <- y
            overlaps[i,][2] <- x

        }
      }

      osmlanduse.overlap.removed <- rbind(st_difference(osmlanduse.overlap,
                                                        st_union(osmlanduse[overlaps[,2],])),
                                          osmlanduse[overlaps[,2],])

      osmlanduse <- rbind(osmlanduse.overlap.removed,osmlanduse.not.overlap)
}
   area <- st_area(osmlanduse) # Removida la superposición, calcula el área

   units(area) <- units

   osmlanduse <- cbind(osmlanduse,area)

   osmlanduse
}
