#' Map Landuse
#'
#' Map landuse classified
#'
#' @param classified Output of classify_osmlanduse.
#' @param title Map title
#' @return A tmap map
#' @examples
#' landuse <- get_osmlanduse()
#' measures <- measure_osmlanduse(landuse)
#' data(clc)
#' classified <- classify_osmlanduse(measures,clc)
#' map <- map_osmlanduse(classified)
#' map
#' @importFrom tmap tm_shape
#' @importFrom tmap tm_polygons
#' @importFrom tmap tm_title
#' @importFrom tmap tm_scalebar
#' @importFrom tmap tm_compass
#' @importFrom tmap tm_credits
#' @export
map_osmlanduse <- function(classified, title = "osmlanduseR map"){

  #require(tmap)

  # preparar para un argumento como archivo
    # usos_del_suelo <- st_read("mapas/usos_del_suelo - 2023-10-04 .geojson")

  map <-  tm_shape(classified) + tm_polygons(fill = "class_name") +
    #tm_style("cobalt") +
    tm_title(title) +
    tm_scalebar(width = 10) +  tm_compass(position = c("left","top"))+
    tm_credits("OpenStreetMap contributors")

  map
}




