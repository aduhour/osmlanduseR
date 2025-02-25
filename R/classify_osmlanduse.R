#' Classify Land Use
#'
#' Classify OpenStreetMap tags into predefined land use classes,
#' removes overlapping polygons and measure area.
#'
#' @details
#' The function classifies every land use polygon to the selected classes,
#' removes overlapping prioritizing smaller polygons as suggested by
#' Schultz et al., (2017) (method \code{smaller}),
#' or establishing land use priorities as suggested by Fonte et al., (2016)
#' (method \code{hierarchichal}),
#' then adds a column with the area measure.
#'
#' The default coordinate reference system is EPSG:5347 (POSGAR 2007),
#' adopted by the National Geographic Institute for Argentina.
#'
#' @references Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf, A. 2017.
#' Open land cover from OpenStreetMap and remote sensing.
#' International Journal of Applied Earth Observation and Geoinformation.
#' \url{http://dx.doi.org/10.1016/j.jag.2017.07.014}
#'
#' Fonte, C.; Minghini, M.; Antoniou, V.; See, L.; Patriarca, J.;
#  Brovelli, M. & Milcinski, G. 2016.
#  An Automated methodology for converting OSM data into a land use/cover map.
#  6th International Conference on Cartography & GIS.
#' @param osmlanduse An sf object, part of the output of \code{get_osmlanduse}.
#' @param osm_tag A vector of OpenStreetMap tag values
#' @param class_name A vector of the same length of \code{osm_tag} assigning a
#' class name or number to each OpenStreetMap tag.
#' @param crs Set the Coordinate Reference System to transform the data to measure area.
#' @param units The units for the area measures.
#' @param priority A vector of the same length of \code{osm_tag}, assigning
#' integers from 1 (greater priority) to the number of land use classes.
#' @param method The method to resolve orverlapping polygons (see Details).
#' @return An sf object with the classified land use classes and area measures added.
#' @examples
#' area <-  "Lezica y Torrezuri, Partido de Luján"
#' lezica <- get_osmlanduse(area, crop_to = "bbox")
#' data(clc)
#' classified <- classify_osmlanduse(lezica$osmlanduse,osm_tag = clc$osm_tag,
#' class_name = clc$class_name)
#' @importFrom sf st_transform
#' @importFrom sf st_overlaps
#' @importFrom sf st_area
#' @importFrom sf st_difference
#' @importFrom sf st_geometry_type
#' @export
classify_osmlanduse <- function(osmlanduse,  osm_tag, class_name,
                                crs=5347, units="ha", priority = NULL,
                                method = "smaller"){

   osmlanduse <-  sf::st_transform(osmlanduse,crs)

  methods <- c("smaller", "hierarchical")

  method <- match.arg(method, methods)

  if (method == "hierarchical" & is.null(priority)){
    stop("A vector of priorities must be provided as argument.")
  }

  ## Classify land use

  osmlanduse$class_name <- class_name[match(osmlanduse$value, osm_tag)]

  # ---------------------
  # Remove overlapping
  # ---------------------

  # smaller method:

  # Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf, A. 2017
  # Open land cover from OpenStreetMap and remote sensing.
  # International Journal of Applied Earth Observationd and Geoinformation,
  # Elsevier BV, 63, 206-213. 10.1016/j.jag.2017.07.014

  # pp 208: Issues of overlapping features were resolved by prioritizing smaller
  # polygons over larger ones (Fig. 3). This is essential for the calculation of
  # correct area statistics or potential comparisons to other data, and gives
  # priority to the detail in the map.

  # hierarchichal method:

  # Fonte, C.; Minghini, M.; Antoniou, V.; See, L.; Patriarca, J.;
  # Brovelli, M. & Milcinski, G. 2016.
  # An Automated methodology for converting OSM data into a land use/cover map.
  # 6th International Conference on Cartography & GIS.

  # pp 5: In the present work these inconsistencies were removed by considering a
  # hierarchy of feature importance, which is shown in Table 2
  # for level 2 classes; in the case of overlap, priority is given to the classes
  # occupying the highest level in the list

  # Pairs of overlapping polygons:

  overlaps <- as.data.frame(sf::st_overlaps(osmlanduse,retain_unique=TRUE))

  # Vector of rows representing overlapping polygons:

   overlap <- unique(c(overlaps[,1],overlaps[,2]))

  # There are overlapping polygons?

    if(sum(overlap) > 0){

      # Filter non overlapping polygons

      osmlanduse_not_overlap <- osmlanduse[-overlap,]

      # Filter overlapping polygons

      osmlanduse_overlap <- osmlanduse[overlap,]

      # Measure area in overlapping polygons and adds the column with the data.

      osmlanduse_overlap$area <-  sf::st_area(osmlanduse_overlap)

      # Select method to remove overlapping.

      switch (method, smaller = {

      # Reorder the overlapping polygons by area

        osmlanduse_overlap <-  osmlanduse_overlap[order(osmlanduse_overlap$area),]

       }, hierarchical = {

          osmlanduse_overlap$priority <- priority[match(osmlanduse_overlap$value, osm_tag)]

          #Order first by priority and then by area

          osmlanduse_overlap <-  osmlanduse_overlap[order(osmlanduse_overlap$priority,osmlanduse_overlap$area),]

      })

      # When st_difference is called with a single argument,
      # overlapping areas are erased from geometries that are indexed
      # at greater numbers in the argument to x

      # As the overlapping polygons were ordered following the selected method,
      # this line removes overlapping:

      osmlanduse_overlap <- sf::st_make_valid(osmlanduse_overlap)

      osmlanduse_overlap <- sf::st_difference(osmlanduse_overlap)

      # Remove column with temporary measures of area for overlapping removal.

      osmlanduse_overlap_removed <- osmlanduse_overlap[,names(osmlanduse_not_overlap)]

      # Unify dataset binding overlapping and non overlapping rows

      osmlanduse <- rbind(osmlanduse_overlap_removed,osmlanduse_not_overlap)

      # It is necessary to separate non-polygon objects in order to measure areas,
      # make maps, and provide a list of these geometries to improve them in OSM.

      is_polygon <- sf::st_geometry_type(osmlanduse)=="POLYGON" | sf::st_geometry_type(osmlanduse)=="MULTIPOLYGON"

      non_polygon <- osmlanduse[!is_polygon,]

      osmlanduse <- osmlanduse[is_polygon,]

      message(paste("There were", nrow(osmlanduse_overlap_removed),
                    "overlapping polygons" ))
        } else {

         message("There is no overlapping polygons")

      }

   # -----------------------------------------------
   # Once removed overlapping, it measures the area.
   # -----------------------------------------------

   area <- sf::st_area(osmlanduse)

   units(area) <- units

   # Returns result

   osmlanduse <- cbind(osmlanduse,area)

   if (!all(is_polygon)){
     message(paste("There were", sum(!is_polygon),
                   "non polygon objects" ))
   } else non_polygon <- NA

   osmlanduse <- list(classified = osmlanduse, non_polygon = non_polygon)

   osmlanduse
}
