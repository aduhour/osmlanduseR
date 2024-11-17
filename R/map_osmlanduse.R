#' Map Land use
#'
#' Map land use classified
#'
#' @param classified Output of classify_osmlanduse.
#' @param title Map title
#' @return A tmap map
#' @examples
#' landuse <- get_osmlanduse()
#' data(clc)
#' classified <- classify_osmlanduse(measures,clc$class_name, clc$osm_tag)
#' map <- map_osmlanduse(classified)
#' map
#' @importFrom tmap tm_shape
#' @importFrom tmap tm_polygons
#' @importFrom tmap tm_scale_bar
#' @importFrom tmap tm_compass
#' @importFrom tmap tm_credits
#' @export
map_osmlanduse <- function(classified, title = "osmlanduseR map"){
  map <-  tm_shape(classified) + tm_polygons(fill = "class_name") +
    tm_title(title) +
    tm_compass(position = c("left","top")) +
    tm_scale_bar(width = 10) +
    tm_credits(paste("\u00a9 OpenStreetMap contributors")) +
    tm_layout(legend.frame = FALSE, title = title) # legend.position = c("left","top")
  map
}




