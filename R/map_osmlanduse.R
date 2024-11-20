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
#' @export
map_osmlanduse <- function(classified, title = "osmlanduseR map"){
  map <-  tmap::tm_shape(classified) +
    tmap::tm_polygons(fill = "class_name",
                fill.scale = tmap::tm_scale_categorical(values = "cols4all.area7")) +
    tmap::tm_compass(position = c("left","top")) +
    tmap::tm_scalebar() +
    tmap::tm_credits(paste("\u00a9 OpenStreetMap contributors")) +
    tmap::tm_title(title) +
    tmap::tm_layout(legend.frame = FALSE)
  map
}




