#' Create the location table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param location_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique location at the observation level.
#' @param location_name (character) One or more columns in \code{L0_wide} of sampling locations ordered, in terms of nesting, from high to low (e.g. \code{location_name = c("region", "state", "county", "plot", "subplot")}).
#' @param latitude (character) An optional column in \code{L0_wide} containing the latitude in decimal degrees of \code{location_id}. Latitudes south of the equator are negative.
#' @param longitude (character) An optional column in \code{L0_wide} containing the longitude in decimal degrees of \code{location_id}. Longitudes west of the prime meridian are negative.
#' @param elevation (character) An optional column in \code{L0_wide} containing the elevation in meters relative to sea level of \code{location_id}. Above sea level is positive. Below sea level is negative.
#' 
#' @details This function collects specified columns from \code{L0_wide}, creates data frames for each \code{location_name}, assigns \code{latitude}, \code{longitude}, and \code{elevation} to the lowest nesting level (i.e. the observation level) returning \code{NA} for higher levels (these will have to be filled manually afterwards), and determines the relationships between location_id and parent_location_id from \code{L0_wide} and \code{location_name}.
#'     
#' @note Values in \code{location_name} columns of \code{L0_wide} should be modifed to provide both context and value before running this function. Not doing so may result in ambiguous location_name values in the resulting location table. Example: A column named "plot" with values "1", "2", "3", in \code{L0_wide} will be listed in the resulting location_name column as values "1", "2", "3" and there will be no way to discern these values correspond with "plot". A general fix is to modify values in the \code{location_name} columns of \code{L0_wide} with \code{paste0(<column name>, "_", <columun value>)}, which will return both the column context and value in the location_name column of the location table as  "plot_1", "plot_2", "plot_3".
#' 
#' Additionally, latitude, longitude, and elevation of sites nested above the observation level will have to be manually added after the location data.frame is returned.
#'
#' @return (data.frame) The location table.
#'     
#' @export
#' 
#' @examples 
#' wide <- ants_L0_wide
#' 
#' location <- create_location(
#'   L0_wide = wide, 
#'   location_id = "location_id", 
#'   location_name = c("block", "plot"), 
#'   latitude = "latitude", 
#'   longitude = "longitude", 
#'   elevation = "elevation")
#' 
#' location
#' 
create_location <- function(L0_wide, 
                            location_id,
                            location_name,
                            latitude = NULL,
                            longitude = NULL, 
                            elevation = NULL) {

  message("Creating location")
  validate_arguments(fun.name = "create_location", fun.args = as.list(environment()))
  
  cols_to_gather <- c(location_id, latitude, longitude, elevation, location_name)
  loc_wide <- L0_wide %>%
    dplyr::select(all_of(c(location_id, latitude, longitude, elevation, location_name))) %>%
    dplyr::distinct()
  
  # Add any missing columns to loc_wide because downstream depend on them
  if (is.null(latitude)) {
    loc_wide$latitude <- NA
  }
  if (is.null(longitude)) {
    loc_wide$longitude <- NA
  }
  if (is.null(elevation)) {
    loc_wide$elevation <- NA
  }
  
  # create list of data frames for each level of location_name
  res <- lapply(
    seq_along(location_name),
    function(i) {
      if (i != length(location_name)) {         # not lowest level
        res <- data.frame(
          location_id = NA_character_,
          location_name = as.character(unique(loc_wide[[location_name[i]]])),
          latitude = NA,
          longitude = NA,
          elevation = NA,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
        res$location_id <- paste0(letters[i], seq(nrow(res))) # add unique location_id (safe assumption?)
      } else if (i == length(location_name)) {   # is lowest level
        res <- data.frame(
          location_id = as.character(loc_wide$location_id),
          location_name = as.character(loc_wide[[location_name[i]]]),
          latitude = loc_wide$latitude,
          longitude = loc_wide$longitude,
          elevation = loc_wide$elevation,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
      }
      return(res)
    })
  
  # Add look up parent_location_id for each level of location_name
  invisible(
    lapply(
      seq_along(location_name),
      function(i) {
        if (i != 1) {
          map <- loc_wide %>% # map location_name at this level to location_name one level up)
            dplyr::select(c(location_name[i-1], location_name[i])) 
          for (r in seq(nrow(res[[i]]))) {
            locname <- res[[i]]$location_name[r]
            parent_location_name <- map[[location_name[i-1]]][
              map[[location_name[i]]] == locname]
            parent_location_id <- res[[i-1]]$location_id[
              res[[i-1]]$location_name == parent_location_name]
            res[[i]]$parent_location_id[r] <<- parent_location_id
          }
        }
      }))
  
  # combine data frames
  res <- dplyr::bind_rows(res)
  
  return(res)
}