#' Classify Landuse
#'
#' Classify OpenStreetMap tags into predefined land use classes
#'
#' @param osmlanduse Output of get_osmlanduse or measure_osmlanduse.
#' @param osm_tag A vector of OpenStreetMap tag values
#' @param class_name A vector of the same length of \code{osm_tag} assigning a class name or number to each
#' OpenStreetMap tag and the landuse classes.
#' @return An sf object adding a column with the classified landuse classes.
#' @examples
#' landuse <- get_osmlanduse()
#' measures <- measure_osmlanduse(landuse)
#' data(clc)
#' classified <- classify_osmlanduse(measures,clc)
#' @export
classify_osmlanduse <- function(osmlanduse,osm_tag,class_name){

osmlanduse$class_name <- class_name[match(osmlanduse$value, osm_tag)]

osmlanduse
}




