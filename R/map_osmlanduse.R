#' Map Land use
#'
#' Map land use classified
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
  map <-  tm_shape(classified) + tm_polygons(fill = "class_name") +
    tm_title(title) +
    tm_compass(position = c("left","top")) +
    tm_scalebar(width = 10) +
    tm_credits(paste("\u00a9 OpenStreetMap contributors")) +
    tm_layout(legend.frame = FALSE)# legend.position = c("left","top")
  map
}




