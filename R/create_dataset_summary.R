#' Create the dataset_summary table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param package_id (character) Column in \code{L0_flat} containing the identifier of the derived L1 dataset.
#' @param original_package_id (character) An optional column in \code{L0_flat} containing the identifier of the source L0 dataset.
#' @param length_of_survey_years (character) Column in \code{L0_flat} containing the number of years the study has been ongoing. Use \code{calc_length_of_survey_years()} to calculate this value.
#' @param number_of_years_sampled (character) Column in \code{L0_flat} containing the number of years within the period of study that samples were taken. Use \code{calc_number_of_years_sampled()} to calculate this value.
#' @param std_dev_interval_betw_years (character) Column in \code{L0_flat} containing the standard deviation of the interval between sampling events. Use \code{calc_std_dev_interval_betw_years()} to calculate this value.
#' @param max_num_taxa (character) Column in \code{L0_flat} containing the number of unique taxa in the source L0 dataset.
#' @param geo_extent_bounding_box_m2 (character) An optional column in \code{L0_flat} containing the area (in meters) of the study location, if applicable (some L0 were collected at a single point). Use \code{calc_geo_extent_bounding_box_m2()} to calculate this value.
#' 
#' @details This function collects specified columns from \code{L0_flat} and returns distinct rows.
#' 
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#' 
#' @return (tbl_df, tbl, data.frame) The dataset_summary table.
#' 
#' @export
#' 
#' @examples 
#' flat <- ants_L0_flat
#' 
#' dataset_summary <- create_dataset_summary(
#'   L0_flat = flat, 
#'   package_id = "package_id", 
#'   original_package_id = "original_package_id", 
#'   length_of_survey_years = "length_of_survey_years",
#'   number_of_years_sampled = "number_of_years_sampled", 
#'   std_dev_interval_betw_years = "std_dev_interval_betw_years", 
#'   max_num_taxa = "max_num_taxa", 
#'   geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2")
#' 
#' dataset_summary
#' 
create_dataset_summary <- function(L0_flat, 
                                   package_id,
                                   original_package_id = NULL,
                                   length_of_survey_years,
                                   number_of_years_sampled,
                                   std_dev_interval_betw_years,
                                   max_num_taxa,
                                   geo_extent_bounding_box_m2 = NULL) {
  
  validate_arguments(fun.name = "create_dataset_summary", fun.args = as.list(environment()))
  
  
  # get cols
  cols_to_gather <- c(package_id, original_package_id, length_of_survey_years,
                      number_of_years_sampled, std_dev_interval_betw_years,
                      max_num_taxa, geo_extent_bounding_box_m2)
  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct()
  # add missing cols
  if (is.null(original_package_id)) {
    res$original_package_id <- NA_character_
  }
  if (is.null(geo_extent_bounding_box_m2)) {
    res$geo_extent_bounding_box_m2 <- NA_character_
  }
  # reorder
  res <- res %>%
    dplyr::select(package_id, original_package_id, length_of_survey_years,
                  number_of_years_sampled, std_dev_interval_betw_years,
                  max_num_taxa, geo_extent_bounding_box_m2)
  # coerce classes
  res <- coerce_table_classes(res, "dataset_summary", class(res))
  return(res)
}








#' Calculate geo_extent_bounding_box_m2 for the dataset_summary table
#'
#' @param west (numeric) West longitude in decimal degrees and negative if west of the prime meridian.
#' @param east (numeric) East longitude in decimal degrees and negative if west of the prime meridian.
#' @param north (numeric) North latitude in decimal degrees and negative if south of the equator.
#' @param south (numeric) South latitude in decimal degrees and negative if south of the equator.
#'
#' @return (numeric) Area of study site in meters squared.
#'     
#' @export
#'
calc_geo_extent_bounding_box_m2 <- function(
  west, east, north, south) {
  df <- data.frame(
    longitude = c(west, east, east, west),
    latitude = c(north, north, south, south))
  res <- round(geosphere::areaPolygon(df))
  return(res)
}








#' Calculate length_of_survey_years for the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) Number of years the study has been ongoing.
#' 
#' @export
#' 
calc_length_of_survey_years <- function(dates) {
  res <- round(
    lubridate::time_length(
      lubridate::interval(min(dates), max(dates)),
      unit = 'year'))
  return(res)
}








#' Calculate number_of_years_sampled for the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) Number of survey years in which a sample was taken.
#' 
#' @export
#' 
calc_number_of_years_sampled <- function(dates) {
  years_sampled <- stats::na.omit(unique(lubridate::year(dates)))
  res <- length(years_sampled) - 1
  return(res)
}








#' Calculate std_dev_interval_betw_years for the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) The standard deviation between sampling events (in years).
#' 
#' @export
#' 
calc_std_dev_interval_betw_years <- function(dates) {
  res <- round(stats::sd(diff(unique(lubridate::date(dates)))/365), 2)
  return(res)
}
