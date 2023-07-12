#' Create the location table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param location_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique location at the observation level.
#' @param location_name (character) One or more columns in \code{L0_flat} of sampling locations ordered from high to low in terms of nesting, where the lowest is the level of observation (e.g. \code{location_name = c("plot", "subplot")}).
#' @param latitude (character) An optional column in \code{L0_flat} containing the latitude in decimal degrees of \code{location_id}. Latitudes south of the equator are negative.
#' @param longitude (character) An optional column in \code{L0_flat} containing the longitude in decimal degrees of \code{location_id}. Longitudes west of the prime meridian are negative.
#' @param elevation (character) An optional column in \code{L0_flat} containing the elevation in meters relative to sea level of \code{location_id}. Above sea level is positive. Below sea level is negative.
#' 
#' @details This function collects specified columns from \code{L0_flat}, creates data frames for each \code{location_name}, assigns \code{latitude}, \code{longitude}, and \code{elevation} to the lowest nesting level (i.e. the observation level) returning \code{NA} for higher levels (these will have to be filled manually afterwards), and determines the relationships between location_id and parent_location_id from \code{L0_flat} and \code{location_name}.
#' 
#' To prevent the listing of duplicate location_name values, and to enable the return of \code{location_name} columns by \code{flatten_data()}, location_name values are suffixed with the column they came from according to: \code{paste0(<column name>, "__", <column value>)}. Example: A column named "plot" with values "1", "2", "3", in \code{L0_flat} would be listed in the resulting location table under the location_name column as "1", "2", "3" and therefore no way to discern these values correspond with "plot". Applying the above listed solution returns "plot__1", "plot__2", "plot__3" in the location table and returns the column "plot" with values c("1", "2", "3") by \code{flatten_data()}.
#' 
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#' 
#' Additionally, latitude, longitude, and elevation of sites nested above the observation level will have to be manually added after the location table is returned.
#'
#' @return (tbl_df, tbl, data.frame) The location table.
#'     
#' @export
#' 
#' @examples 
#' flat <- ants_L0_flat
#' 
#' location <- create_location(
#'   L0_flat = flat, 
#'   location_id = "location_id", 
#'   location_name = c("block", "plot"), 
#'   latitude = "latitude", 
#'   longitude = "longitude", 
#'   elevation = "elevation")
#' 
#' location
#' 
create_location <- function(L0_flat, 
                            location_id,
                            location_name,
                            latitude = NULL,
                            longitude = NULL, 
                            elevation = NULL) {

  validate_arguments(fun.name = "create_location", fun.args = as.list(environment()))
  
  # Disambiguate location names and enable return of location_name columns by flatten_data(). This assumes all characters before the first unique marker represent the column name and those after are the values.
  L0_flat <- suffix_colname_to_vals(tbl = L0_flat, cols = location_name)
  
  cols_to_gather <- c(location_id, latitude, longitude, elevation, location_name)
  loc_wide <- L0_flat %>%
    dplyr::select(all_of(c(location_id, latitude, longitude, elevation, location_name))) %>%
    dplyr::distinct()
  
  # Add missing cols to loc_wide - Some downstream processes require them
  if (is.null(latitude)) {
    loc_wide$latitude <- NA
  }
  if (is.null(longitude)) {
    loc_wide$longitude <- NA
  }
  if (is.null(elevation)) {
    loc_wide$elevation <- NA
  }
  
  # Assign ids to higher levels - Ensures referential integrity of self 
  # referencing ids
  for (i in seq_along(location_name)) {
    current <- location_name[i]                    # Current level
    current_and_higher <- location_name[1:i]       # Current level and higher levels
    lowest <- location_name[length(location_name)] # Lowest level
    if (current != lowest) {
      ids <- loc_wide %>%
        dplyr::group_by(dplyr::across(any_of(current_and_higher))) %>% 
        dplyr::group_indices() %>% 
        paste0(letters[i], .)
      loc_wide[[paste0(current, "_id")]] <- ids
    }
  }
  
  # Parse each level into it's own data frame - Easier to deal with one at a 
  # time than all at once
  res <- lapply(
    seq_along(location_name),
    function(i) {
      current <- location_name[i]                    # Current level
      lowest <- location_name[length(location_name)] # Lowest level (the level of observation)
      # Select data frame for level
      if (current != lowest) {
        cols2select <- c(paste0(current, "_id"), current, "latitude", "longitude",  "elevation")
      } else {
        cols2select <- c("location_id", current, "latitude", "longitude", "elevation")
      }
      res <- loc_wide %>% dplyr::select(dplyr::any_of(cols2select))
      # Add parent ids
      if (i != 1) {
        parent <- location_name[i-1]
        res[["parent_location_id"]] <- loc_wide[[paste0(parent, "_id")]]
      } else {
        res$parent_location_id <- NA_character_
      }
      # Apply standard col names
      res <- res %>% dplyr::rename(location_name = any_of(current))
      if (paste0(current, "_id") %in% colnames(res)) {
        res <- res %>% dplyr::rename(location_id = any_of(paste0(current, "_id")))
      }
      # Sort for human readability
      res <- sort_by_alphanumeric(x = res, col = "location_id")
      # Only the level of observation gets latitude, longitude, and elevation values
      if (current != lowest) {
        res$latitude <- NA
        res$longitude <- NA
        res$elevation <- NA
      }
      # Only need unique rows
      res <- unique.data.frame(res)
      return(res)
    })
  # Coerce data frame classes into a consistent type so they can be combined
  res <- lapply(res, coerce_table_classes, name = "location", cls = class(L0_flat))
  # Combine data frames into the location table
  res <- dplyr::bind_rows(res)
  
  return(res)
}








# Sort table by alphanumeric values
#
# @param x (tbl_df, tbl, data.frame) A table
# @param col (character) Column containing alphanumeric values to sort on
# 
# @details Parses \code{col} values into alpha and numeric components and sorts first on alpha and second on numeric.
#
# @return (tbl_df, tbl, data.frame) \code{x} sorted by values in \code{col}.
# 
sort_by_alphanumeric <- function(x, col) {
  # Parse ids into alpha and numbr cols
  x$alpha <- stringr::str_remove_all(x[[col]], "[^[:alpha:]]")
  x$numbr <- as.numeric(stringr::str_remove_all(x[[col]], "[^[:digit:]]"))
  # Sort on alpha then on numbr
  x <- dplyr::group_by(x, alpha, numbr)
  x <- dplyr::arrange(x, .by_group = TRUE)
  # Remove alpha and numbr cols
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -alpha, -numbr)
  return(x)
}








# Make values unique by adding the column name as a suffix
#
# @param tbl (tbl_df, tbl, data.frame) A table
# @param cols (character) A list of columns in \code{tbl} to apply this opperation to
#
# @return \code{tbl} with \code{cols} modified according to: paste0(<column name>, "__", <column value>). The unique marker injected by this function can be used to parse out the column name the values originated from.
#
suffix_colname_to_vals <- function(tbl, cols) {
  for (col in cols) {
    tbl[[col]] <- paste0(col, "__", tbl[[col]])
  }
  return(tbl)
}








# A version of create_location() still in use by NEON
#
# @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
# @param location_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique location at te observation level.
# @param location_name (character) One or more columns in \code{L0_flat} of sampling locations ordered from high to low i terms of nesting, where the lowest is the level of observation (e.g. \code{location_name = c("plot", "subplot")}).
# @param latitude (character) An optional column in \code{L0_flat} containing the latitude in decimal degrees of \ode{location_id}. Latitudes south of the equator are negative.
# @param longitude (character) An optional column in \code{L0_flat} containing the longitude in decimal degrees of \ode{location_id}. Longitudes west of the prime meridian are negative.
# @param elevation (character) An optional column in \code{L0_flat} containing the elevation in meters relative to sea lvel of \code{location_id}. Above sea level is positive. Below sea level is negative.
#
# @return (tbl_df, tbl, data.frame) The location table.
# 
create_location_deprecated <- function(L0_flat, 
                                       location_id,
                                       location_name,
                                       latitude = NULL,
                                       longitude = NULL, 
                                       elevation = NULL) {
  
  validate_arguments(fun.name = "create_location", fun.args = as.list(environment()))
  
  cols_to_gather <- c(location_id, latitude, longitude, elevation, location_name)
  loc_wide <- L0_flat %>%
    dplyr::select(all_of(c(location_id, latitude, longitude, elevation, location_name))) %>%
    dplyr::distinct()
  
  # Add missing cols to loc_wide - Some downstream processes require them
  if (is.null(latitude)) {
    loc_wide$latitude <- NA
  }
  if (is.null(longitude)) {
    loc_wide$longitude <- NA
  }
  if (is.null(elevation)) {
    loc_wide$elevation <- NA
  }
  
  # Assign ids to higher levels - Ensures referential integrity of self 
  # referencing ids
  for (i in seq_along(location_name)) {
    current <- location_name[i]                    # Current level
    current_and_higher <- location_name[1:i]       # Current level and higher levels
    lowest <- location_name[length(location_name)] # Lowest level
    if (current != lowest) {
      ids <- loc_wide %>%
        dplyr::group_by(dplyr::across(any_of(current_and_higher))) %>% 
        dplyr::group_indices() %>% 
        paste0(letters[i], .)
      loc_wide[[paste0(current, "_id")]] <- ids
    }
  }
  
  # Parse each level into it's own data frame - Easier to deal with one at a 
  # time than all at once
  res <- lapply(
    seq_along(location_name),
    function(i) {
      current <- location_name[i]                    # Current level
      lowest <- location_name[length(location_name)] # Lowest level (the level of observation)
      # Select data frame for level
      if (current != lowest) {
        cols2select <- c(paste0(current, "_id"), current, "latitude", "longitude",  "elevation")
      } else {
        cols2select <- c("location_id", current, "latitude", "longitude", "elevation")
      }
      res <- loc_wide %>% dplyr::select(dplyr::any_of(cols2select))
      # Add parent ids
      if (i != 1) {
        parent <- location_name[i-1]
        res[["parent_location_id"]] <- loc_wide[[paste0(parent, "_id")]]
      } else {
        res$parent_location_id <- NA_character_
      }
      # Apply standard col names
      res <- res %>% dplyr::rename(location_name = any_of(current))
      if (paste0(current, "_id") %in% colnames(res)) {
        res <- res %>% dplyr::rename(location_id = any_of(paste0(current, "_id")))
      }
      # Only need unique rows
      res <- unique.data.frame(res)
      # Sort for human readability
      res <- sort_by_alphanumeric(x = res, col = "location_id")
      return(res)
    })
  # Coerce data frame classes into a consistent type so they can be combined
  res <- lapply(res, coerce_table_classes, name = "location", cls = class(L0_flat))
  # Combine data frames into the location table
  res <- dplyr::bind_rows(res)
  
  return(res)
}