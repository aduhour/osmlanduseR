#' Classify land use
#'
#' Classify OpenStreetMap tags into predefined land use classes,
#' removes overlapping polygons and measure area.
#'
#' @details
#' The function classifies every land use polygon to the selected clases,
#' removes overlapping prioritizing smaller polygons as suggested by
#' Schultz et al., (2017),
#' or establishing land use priorities as suggested by Fonte et al., (2016),
#' then adds a column with area measure.
#'
#' The default coordinate reference system is POSGAR 2007,
#' adopted by the National Geographic Institute for Argentina.
#'
#' @references Schultz, M.; Vossa, J.; Auera, M.; Carterb, S. & Zipf, A. 2017.
#' Open land cover from OpenStreetMap and remote sensing.
#' International Journal of Applied Earth Observation and Geoinformation.
#' #' \url{http://dx.doi.org/10.1016/j.jag.2017.07.014}
#' Fonte, C.; Minghini, M.; Antoniou, V.; See, L.; Patriarca, J.;
#  Brovelli, M. & Milcinski, G. 2016.
#  An Automated methodology for converting OSM data into a land use/cover map.
#  6th International Conference on Cartography & GIS.
#' @param osmlanduse An sf object, the output of get_osmlanduse.
#' @param crs Set the Coordinate reference system to transform the data to measure area.
#' @param units The units for the area measures.
#' @param osm_tag A vector of OpenStreetMap tag values
#' @param class_name A vector of the same length of \code{osm_tag} assigning a class name or number to each
#' @param priority A vector of integers from 1 to the number of land use classes.
#' @param method The method to resolve orverlapping polygons.
#' OpenStreetMap tag and the land use classes.
#' @return An sf object with the classified
#' land use classes and area measures added.
#' @examples
#' landuse <- get_osmlanduse()
#' data(clc)
#' classified <- classify_osmlanduse(landuse,osm_tag = clc$osm_tag,
#' class_name = clc$class_name)
#' @importFrom sf st_transform
#' @importFrom sf st_overlaps
#' @importFrom sf st_area
#' @importFrom sf st_union
#' @importFrom sf st_difference
#' @export
classify_osmlanduse <- function(osmlanduse, crs=5347, units="ha", osm_tag,
                               class_name,
                               priority = NULL, method = "smaller"){

  osmlanduse <-  st_transform(osmlanduse,crs)

  methods <- c("smaller", "hierarchical")

  method <- match.arg(method, methods)

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

  overlaps <- as.data.frame(st_overlaps(osmlanduse,retain_unique=TRUE))

  # Vector of rows representing overlapping polygons:

   overlap <- unique(c(overlaps[,1],overlaps[,2]))

  # There are overlapping polygons?

    if(sum(overlap) > 0){

      # Filter not overlapping polygons

      osmlanduse.not.overlap <- osmlanduse[-overlap,]

      # Filter overlapping polygons

      osmlanduse.overlap <- osmlanduse[overlap,]

      # Measure area in overlapping polygons and adds the column with the data.

      osmlanduse.overlap$area <-  st_area(osmlanduse.overlap)

      # Select method to remove overlapping.

      switch (method, smaller = {

      # Reorder the overlapping polygons by area

        osmlanduse.overlap <-  osmlanduse.overlap[order(osmlanduse.overlap$area),]

      }, hierarchical = {
          if (is.null(priority)){
            stop("A vector of priorities must be provided as argument.")
          }
        stop("The method has not been implemented")
      })

      # When st_difference is called with a single argument,
      # overlapping areas are erased from geometries that are indexed
      # at greater numbers in the argument to x

      # As the overlapping polygons are ordered by area,
      # this line removes overlapping prioritizing the smaller ones.

      osmlanduse.overlap <- st_difference(osmlanduse.overlap)

      osmlanduse.overlap.removed <- osmlanduse.overlap[,!(names(osmlanduse.overlap) %in% c("area"))]

      osmlanduse <- rbind(osmlanduse.overlap.removed,osmlanduse.not.overlap)
    } else {
      message("There is no overlapping polygons")
    }

   # -----------------------------------------------
   # Once removed overlapping, it measures the area.
   # -----------------------------------------------

   area <- st_area(osmlanduse)

   units(area) <- units

   osmlanduse <- cbind(osmlanduse,area)

   osmlanduse
}
