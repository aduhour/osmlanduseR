#' Corine Land Cover (CLC) classes and OSM tags
#'
#' Legend harmonization between OSM tags and Corine Land Cover (CLC) classes.
#' Level two legend.
#'
#' The data was adapted from Schultz et al. (2017) and Fonte et al. (2016)
#' to be used in classification.
#'
#' @format
#' A data frame with 60 rows and 5 columns:
#' \describe{
#'   \item{class}{Number.}
#'   \item{class_number}{CLC Class number.}
#'   \item{class_priority}{Priority to remove overlapping.}
#'   \item{class_name}{CLC Class Name.}
#'   \item{osm_tag}{OpenStreetMap tag value associated with each class.}
#' }
#' @source
#' Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf, A. 2017.
#' Open land cover from OpenStreetMap and remote sensing International.
#' Journal of Applied Earth Observation and Geoinformation.
#' http://dx.doi.org/10.1016/j.jag.2017.07.014
#'
#' Fonte, C.; Minghini, M.; Antoniou, V.; See, L.; Patriarca, J.;
#' Brovelli, M. & Milcinski, G. 2016.
#' An Automated methodology for converting OSM data into a land use/cover map.
#' 6th International Conference on Cartography & GIS.
#'
"clc"

#' INTA FAO Land Cover Classes
#'
#' Legend harmonization between OSM tags and the National Institute of
#' Agricultural Technology (INTA) - FAO Land Use and Land
#' Cover classification system. Level one legend.
#'
#' The data was adapted from Volante et al. (2009)
#' to be used in classification.
#'
#' @format
#' A data frame with 65rows and 6 columns:
#' \describe{
#'   \item{class}{Number.}
#'   \item{class_number}{LCCS Class number.}
#'   \item{class_priority}{Priority to remove overlapping.}
#'   \item{class_name_short}{Abreviated LCCS Class Name.}
#'   \item{class_name}{LCCS Class Name.}
#'   \item{osm_tag}{OpenStreetMap tag value associated with each class.}
#' }
#' @source
#' Volante, J. N. 2009. Monitoreo de la Cobertura y el Uso del Suelo a partir de
#' sensores remotos. Instituto Nacional de Tecnología Agropecuaria,
#'
"intafao"
