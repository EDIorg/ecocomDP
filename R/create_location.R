#' Create the location table
#'
#' @param L0_wide (data.frame) The fully joined L0 dataset, in wide format.
#' @param location_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique location at the observation level. Identifiers corresponding to higher location levels will be assigned by this function.
#' @param latitude (character; optional) Column in \code{L0_wide} containing the latitude in decimal degrees of \code{location_id}. Latitudes south of the equator are negative.
#' @param longitude (character; optional) Column in \code{L0_wide} containing the longitude in decimal degrees of \code{location_id}. Longitudes west of the prime meridian are negative.
#' @param elevation (character; optional) Column in \code{L0_wide} containing the elevation in meters relative to sea level of \code{location_id}. Above sea level is positive. Below sea level is negative.
#' @param location_nesting (character) Columns in \code{L0_wide} containing location names and listed from high to low. Example: \code{location_nesting = c("block", "plot", "subplot")}.
#' 
#'
#' @details This function collects specified columns from \code{L0_wide}, creates data frames for each \code{location_nesting}, assigns \code{latitude}, \code{longitude}, and \code{elevation} to the lowest nesting level (i.e. the observation level) returning \code{NA} for higher levels (these will have to be filled manually afterwards), and determines the relationships between location_id and parent_location_id from \code{L0_wide} and \code{location_nesting}.
#'     
#' @note Values in \code{location_nesting} columns of \code{L0_wide} should be modifed to provide both context and value before running this function. Not doing so may result in ambiguous location_name values in the resulting location table. Example: A column named "plot" with values "1", "2", "3", in \code{L0_wide} will be listed in the resulting location_name column as values "1", "2", "3" and there will be no way to discern these values correspond with "plot". A general fix is to modify values in the \code{location_nesting} columns of \code{L0_wide} with \code{paste0(column_name, "_", columun_value)}, which will return both the column context and value in the location_name column of the location table as  "plot_1", "plot_2", "plot_3".
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
                            location_nesting = NULL) {
  # TODO: validate_arguments()
  # - cols exist in L0_wide for non-required cols
  # - NULL optional cols if not in L0_wide
  # - rename cols in L0_wide if not 1-to-1 match
  # - return
  # - required cols vs optional cols
  # TODO: Metadtaa note: lat, lon, and elev of site levels nested higher in the tree, above the observation level must be manually added after the location data frame is created
  message("Creating location")
  cols_to_gather <- c(location_id, latitude, longitude, elevation, location_nesting)
  loc_wide <- L0_wide %>%
    dplyr::select(all_of(c(location_id, latitude, longitude, elevation, location_nesting))) %>%
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
  # create list of data frames for each level of location_nesting
  res <- lapply(
    seq_along(location_nesting),
    function(i) {
      if (i != length(location_nesting)) {         # not lowest level
        res <- data.frame(
          location_id = NA_character_,
          location_name = as.character(unique(loc_wide[[location_nesting[i]]])),
          latitude = NA,
          longitude = NA,
          elevation = NA,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
        res$location_id <- paste0(letters[i], seq(nrow(res))) # add unique location_id (safe assumption?)
      } else if (i == length(location_nesting)) {   # is lowest level
        res <- data.frame(
          location_id = as.character(loc_wide$location_id),
          location_name = as.character(loc_wide[[location_nesting[i]]]),
          latitude = loc_wide$latitude,
          longitude = loc_wide$longitude,
          elevation = loc_wide$elevation,
          parent_location_id = NA_character_,
          stringsAsFactors = FALSE)
      }
      return(res)
    })
  # Add look up parent_location_id for each level of location_nesting
  invisible(
    lapply(
      seq_along(location_nesting),
      function(i) {
        if (i != 1) {
          map <- loc_wide %>% # map location_name at this level to location_name one level up)
            dplyr::select(c(location_nesting[i-1], location_nesting[i])) 
          for (r in seq(nrow(res[[i]]))) {
            location_name <- res[[i]]$location_name[r]
            parent_location_name <- map[[location_nesting[i-1]]][
              map[[location_nesting[i]]] == location_name]
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