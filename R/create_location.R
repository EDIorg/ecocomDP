#' Create the location table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param location_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique location at the observation level. Identifiers corresponding to higher location levels will be assigned by this function.
#' @param latitude (character) An optional column in \code{L0_wide} containing the latitude in decimal degrees of \code{location_id}. Latitudes south of the equator are negative.
#' @param longitude (character) An optional column in \code{L0_wide} containing the longitude in decimal degrees of \code{location_id}. Longitudes west of the prime meridian are negative.
#' @param elevation (character) An optional column in \code{L0_wide} containing the elevation in meters relative to sea level of \code{location_id}. Above sea level is positive. Below sea level is negative.
#' @param nesting (character) Columns in \code{L0_wide} of sampling locations, the group of which forms a nested set, ordered from high to low (e.g. \code{nesting = c("region", "state", "county", "plot", "subplot")}).
#' @details This function collects specified columns from \code{L0_wide}, creates data frames for each \code{nesting}, assigns \code{latitude}, \code{longitude}, and \code{elevation} to the lowest nesting level (i.e. the observation level) returning \code{NA} for higher levels (these will have to be filled manually afterwards), and determines the relationships between location_id and parent_location_id from \code{L0_wide} and \code{nesting}. Default names of optional columns are ignored if they can't be found in \code{L0_wide} (i.e. no need to set as NULL).
#'     
#' @note Values in \code{nesting} columns of \code{L0_wide} should be modifed to provide both context and value before running this function. Not doing so may result in ambiguous location_name values in the resulting location table. Example: A column named "plot" with values "1", "2", "3", in \code{L0_wide} will be listed in the resulting location_name column as values "1", "2", "3" and there will be no way to discern these values correspond with "plot". A general fix is to modify values in the \code{nesting} columns of \code{L0_wide} with \code{paste0(column_name, "_", columun_value)}, which will return both the column context and value in the location_name column of the location table as  "plot_1", "plot_2", "plot_3".
#'
#' @return (data.frame) The location table of ecocomDP.
#'     
#' @export
#' 
#' @examples 
#' 
create_location <- function(L0_wide, 
                            location_id = "location_id",
                            latitude = "latitude",
                            longitude = "longitude", 
                            elevation = "elevation",
                            nesting = NULL) {
  # TODO: validate_arguments()
  # - if nesting = NULL then ....
  # - cols exist in L0_wide for non-required cols
  # - NULL optional cols if not in L0_wide
  # - rename cols in L0_wide if not 1-to-1 match
  # - return
  # - required cols vs optional cols
  # TODO: Metadtaa note: lat, lon, and elev of site levels nested higher in the tree, above the observation level must be manually added after the location data frame is created
  message("Creating location")
  cols_to_gather <- c(location_id, latitude, longitude, elevation, nesting)
  loc_wide <- L0_wide %>%
    dplyr::select(all_of(c(location_id, latitude, longitude, elevation, nesting))) %>%
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
  # create list of data frames for each level of nesting
  res <- lapply(
    seq_along(nesting),
    function(i) {
      if (i != length(nesting)) {         # not lowest level
        res <- data.frame(
          location_id = NA_character_,
          location_name = as.character(unique(loc_wide[[nesting[i]]])),
          latitude = NA,
          longitude = NA,
          elevation = NA,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
        res$location_id <- paste0(letters[i], seq(nrow(res))) # add unique location_id (safe assumption?)
      } else if (i == length(nesting)) {   # is lowest level
        res <- data.frame(
          location_id = as.character(loc_wide$location_id),
          location_name = as.character(loc_wide[[nesting[i]]]),
          latitude = loc_wide$latitude,
          longitude = loc_wide$longitude,
          elevation = loc_wide$elevation,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
      }
      return(res)
    })
  # Add look up parent_location_id for each level of nesting
  invisible(
    lapply(
      seq_along(nesting),
      function(i) {
        if (i != 1) {
          map <- loc_wide %>% # map location_name at this level to location_name one level up)
            dplyr::select(c(nesting[i-1], nesting[i])) 
          for (r in seq(nrow(res[[i]]))) {
            location_name <- res[[i]]$location_name[r]
            parent_location_name <- map[[nesting[i-1]]][
              map[[nesting[i]]] == location_name]
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