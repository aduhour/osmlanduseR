% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_osmlanduse.R
\name{get_osmlanduse}
\alias{get_osmlanduse}
\title{Get OSM data for land use / land cover analysis}
\usage{
get_osmlanduse(area = "Partido de Lujan", crop_to = "area")
}
\arguments{
\item{area}{An sf object or the place name of an area}

\item{crop_to}{Character string indicating if the result should be the
intersection with the polygon ("area", default) or the bounding box ("bbox").}
}
\value{
A list with two sf objects:  The area and the land use data.
}
\description{
Build an Overpass query to retrieve OSM data for land use and
land cover analysis and mapping.
}
\details{
The function retrieves OSM elements tagged with keys: natural, landuse, natural
amenity, aeroway and leisure.
}
\examples{
area <-  "Lezica y Torrezuri, Partido de Luján"
landuse <- get_osmlanduse(area, crop_to = "bbox")
}
