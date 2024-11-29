#' Map Land use
#'
#' Map land use classified
#'
#' @param classified Output of classify_osmlanduse.
#' @param title Map title
#' @param ... Optional arguments passed to \code{tm_layout} function.
#' @return A tmap map
#' @examples
#' area <-  "Lezica y Torrezuri, Partido de Luján"
#' lezica <- get_osmlanduse(area, crop_to = "bbox")
#' data(clc)
#' classified <- classify_osmlanduse(lezica$osmlanduse,osm_tag = clc$osm_tag,
#' class_name = clc$class_name)
#' map <- map_osmlanduse(classified)
#' map
#' @importFrom stats aggregate
#' @export
map_osmlanduse <- function(classified, title = "osmlanduseR map",...){
  lndagg <- aggregate(subset(classified,select = "area"),
                      by=list("Land use class" = classified$class_name),
                      FUN = sum)
  map <-  tmap::tm_shape(lndagg) +
    tmap::tm_polygons(fill = "Land use class",
                      fill.scale = tmap::tm_scale_categorical(values = "cols4all.area7")) +
    tmap::tm_add_legend(type = "polygons", title = "Area (ha)",
                        labels = round(lndagg$area,1),
                        fill="white",col="white")+
    tmap::tm_compass(position = c("left","top")) +
    tmap::tm_scalebar() +
    tmap::tm_credits(paste("\u00a9 OpenStreetMap contributors")) +
    tmap::tm_title(title) +
    tmap::tm_layout(legend.stack = "horizontal",
                    legend.outside = TRUE,
                    legend.outside.position = "bottom",
                    ...)
  map
}




