#' Map Land use
#'
#' Map land use classified
#'
#' @param classified Output of classify_osmlanduse.
#' @param title Map title
#' @return A tmap map
#' @examples
#' area <-  "Lezica y Torrezuri, Partido de Luján"
#' landuse <- get_osmlanduse(area, crop_to = "bbox")
#' data(clc)
#' classified <- classify_osmlanduse(landuse,osm_tag = clc$osm_tag,
#' class_name = clc$class_name)
#' map <- map_osmlanduse(classified)
#' map
#' @importFrom tmap tm_shape
#' @importFrom tmap tm_polygons
#' @importFrom tmap tm_scale_categorical
#' @importFrom tmap tm_scalebar
#' @importFrom tmap tm_compass
#' @importFrom tmap tm_credits
#' @importFrom tmap tm_layout
#' @importFrom tmap tm_title
#' @export
map_osmlanduse <- function(classified, title = "osmlanduseR map"){
  map <-  tm_shape(classified) +
    tm_polygons(fill = "class_name",
                fill.scale = tm_scale_categorical(values = "parks.iguazu_falls")) +
    tm_compass(position = c("left","top")) +
    tm_scalebar() +
    tm_credits(paste("\u00a9 OpenStreetMap contributors")) +
    tm_title(title) +
    tm_layout(legend.frame = FALSE)
  map
}




