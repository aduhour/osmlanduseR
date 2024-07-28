#' Classify Landuse
#'
#' Classify OpenStreetMap tags into predefined landuse classes
#'
#' @param osmlanduse Output of get_osmlanduse or measure_osmlanduse.
#' @param classes A table with the correspondence between tags and landuse classes. See examples
#' @return An sf object adding columns representing landuse classes
#' @examples
#' landuse <- get_osmlanduse()
#' measures <- measure_osmlanduse(landuse)
#' data(clc)
#' classified <- classify_osmlanduse(measures,clc)
#' @export
classify_osmlanduse <- function(osmlanduse,classes){

# if(alguna prueba para cargar la tabla de clases)

osmlanduse$Class_Name <- classes$Class_Name[match(osmlanduse$value, classes$OSM_tag)]

osmlanduse
}




