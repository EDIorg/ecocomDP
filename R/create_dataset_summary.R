#' Create the dataset_summary table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param package_id (character) Column in \code{L0_wide} containing the identifier this dataset will have once it's published in a data repository.
#' @param original_package_id (character) The optional column in \code{L0_wide} containing the identifier of the L0 dataset in the repository it's published (some L0 are not published).
#' @param length_of_survey_years (character) Column in \code{L0_wide} containing the number of years the study has been ongoing. Use \code{calc_length_of_survey_years()} to calculate this value.
#' @param number_of_years_sampled (character) Column in \code{L0_wide} containing the number of years within the period of the study that samples were taken. Use \code{calc_number_of_years_sampled()} to calculate this value.
#' @param std_dev_interval_betw_years (character) Column in \code{L0_wide} containing the standard deviation of the interval between sampling events. Use \code{calc_std_dev_interval_betw_years()} to calculate this value.
#' @param max_num_taxa (character) Column in \code{L0_wide} containing the number of unique taxa in this dataset.
#' @param geo_extent_bounding_box_m2 (character) An optional column in \code{L0_wide} containing the area (in meters) of the study location, if applicable (some L0 were collected at a single point). Use \code{calc_geo_extent_bounding_box_m2()} to calculate this value.
#'
#' @details This function collects specified columns from \code{L0_wide} and returns distinct rows. Default names of optional columns are ignored if they can't be found in \code{L0_wide} (i.e. no need to set as NULL).
#'
#' @return (data.frame) The dataset_summary table of ecocomDP.
#'     
#' @export
#' 
#' @examples 
#' 
#'
create_dataset_summary <- function(L0_wide, 
                                   package_id = "package_id",
                                   original_package_id = "original_package_id",
                                   length_of_survey_years = "length_of_survey_years",
                                   number_of_years_sampled = "number_of_years_sampled",
                                   std_dev_interval_betw_years = "std_dev_interval_betw_years",
                                   max_num_taxa = "max_num_taxa",
                                   geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2") {
  message("Creating dataset_summary")
  # TODO: validate_arguments()
  # - cols exist in L0_wide for non-required cols
  # - NULL optional cols if not in L0_wide
  # - rename cols in L0_wide if not 1-to-1 match
  # - required cols vs optional cols
  # - return
  # get cols
  cols_to_gather <- c(package_id, original_package_id, length_of_survey_years,
                      number_of_years_sampled, std_dev_interval_betw_years,
                      max_num_taxa, geo_extent_bounding_box_m2)
  res <- L0_wide %>%
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
  return(res)
}








#' Calculate geo_extent_bounding_box_m2 of the dataset_summary table
#'
#' @param west (numeric) West longitude in decimal degrees and negative if west of the prime meridian.
#' @param east (numeric) East longitude in decimal degrees and negative if west of the prime meridian.
#' @param north (numeric) North latitude in decimal degrees and negative if south of the equator.
#' @param south (numeric) South latitude in decimal degrees and negative if south of the equator.
#'
#' @return
#'     (numeric) Area in square meters.
#'     
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' }
#'
calc_geo_extent_bounding_box_m2 <- function(
  west, east, north, south) {
  df <- data.frame(
    longitude = c(west, east, east, west),
    latitude = c(north, north, south, south))
  res <- round(geosphere::areaPolygon(df))
  return(res)
}








#' Calculate length_of_survey_years of the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) Number of years the study has been ongoing
#' @export
#'
#' @examples
#' 
calc_length_of_survey_years <- function(dates) {
  res <- round(
    lubridate::time_length(
      lubridate::interval(min(dates), max(dates)),
      unit = 'year'))
  return(res)
}








#' Calculate number_of_years_sampled of the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) Number of survey years
#' @export
#'
#' @examples
#' 
calc_number_of_years_sampled <- function(dates) {
  years_sampled <- na.omit(unique(lubridate::year(dates)))
  res <- length(years_sampled)
  return(res)
}








#' Calculate std_dev_interval_betw_years of the dataset_summary table
#'
#' @param dates (Date) Dates from the L0 source dataset encompassing the entire study duration.
#'
#' @return (numeric) Number of survey years
#' @export
#'
#' @examples
#' 
calc_std_dev_interval_betw_years <- function(dates) {
  res <- round(sd(diff(unique(lubridate::date(dates)))/365), 2)
  return(res)
}
