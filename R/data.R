#' Corine Land Cover (CLC) classes and OSM tags
#'
#' Legend harmonization between OSM tags and Corine Land Cover (CLC) classes, level two legend.
#'
#' The data was adapted from Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf, A.
#' Open land cover from OpenStreetMap and remote sensing International.
#' Journal of Applied Earth Observationd and Geoinformation, 2017
#'
#' to be used in classification.
#'
#' @format ## `clc`
#' A data frame with 61 rows and 4 columns:
#' \describe{
#'   \item{class}{Number.}
#'   \item{class_number}{CLC Class number.}
#'   \item{class_name}{CLC Class Name.}
#'   \item{osm_tag}{Openstreet map tag value associated with each class.}
#' }
#' @source <http://dx.doi.org/10.1016/j.jag.2017.07.014>
"clc"
