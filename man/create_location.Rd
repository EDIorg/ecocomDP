% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_location.R
\name{create_location}
\alias{create_location}
\title{Create the location table}
\usage{
create_location(
  L0_flat,
  location_id,
  location_name,
  latitude = NULL,
  longitude = NULL,
  elevation = NULL
)
}
\arguments{
\item{L0_flat}{(tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).}

\item{location_id}{(character) Column in \code{L0_flat} containing the identifier assigned to each unique location at the observation level.}

\item{location_name}{(character) One or more columns in \code{L0_flat} of sampling locations ordered from high to low in terms of nesting, where the lowest is the level of observation (e.g. \code{location_name = c("plot", "subplot")}).}

\item{latitude}{(character) An optional column in \code{L0_flat} containing the latitude in decimal degrees of \code{location_id}. Latitudes south of the equator are negative.}

\item{longitude}{(character) An optional column in \code{L0_flat} containing the longitude in decimal degrees of \code{location_id}. Longitudes west of the prime meridian are negative.}

\item{elevation}{(character) An optional column in \code{L0_flat} containing the elevation in meters relative to sea level of \code{location_id}. Above sea level is positive. Below sea level is negative.}
}
\value{
(tbl_df, tbl, data.frame) The location table.
}
\description{
Create the location table
}
\details{
This function collects specified columns from \code{L0_flat}, creates data frames for each \code{location_name}, assigns \code{latitude}, \code{longitude}, and \code{elevation} to the lowest nesting level (i.e. the observation level) returning \code{NA} for higher levels (these will have to be filled manually afterwards), and determines the relationships between location_id and parent_location_id from \code{L0_flat} and \code{location_name}.

To prevent the listing of duplicate location_name values, and to enable the return of \code{location_name} columns by \code{flatten_data()}, location_name values are suffixed with the column they came from according to: \code{paste0(<column name>, "__", <column value>)}. Example: A column named "plot" with values "1", "2", "3", in \code{L0_flat} would be listed in the resulting location table under the location_name column as "1", "2", "3" and therefore no way to discern these values correspond with "plot". Applying the above listed solution returns "plot__1", "plot__2", "plot__3" in the location table and returns the column "plot" with values c("1", "2", "3") by \code{flatten_data()}.

"flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.

Additionally, latitude, longitude, and elevation of sites nested above the observation level will have to be manually added after the location table is returned.
}
\examples{
flat <- ants_L0_flat

location <- create_location(
  L0_flat = flat, 
  location_id = "location_id", 
  location_name = c("block", "plot"), 
  latitude = "latitude", 
  longitude = "longitude", 
  elevation = "elevation")

location

}
