#' Create the observation table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param observation_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique observation.
#' @param event_id (character) An optional column in \code{L0_wide} containing the identifier assigned to each unique sampling event.
#' @param package_id (character) Column in \code{L0_wide} containing the identifier this dataset will have once it's published in a data repository.
#' @param location_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique location at the observation level.
#' @param datetime (character) Column in \code{L0_wide} containing the date, and if applicable time, of the observation following the ISO-8601 standard format (i.e. YYYY-MM-DD hh:mm:ss).
#' @param taxon_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique organism at the observation level.
#' @param variable_name (character) Columns in \code{L0_wide} containing the variables measured.
#' @param unit (character) An optional column in \code{L0_wide} containing the units of each \code{variable_name} following the column naming convention: unit_<variable_name> (e.g. "unit_abundance").
#' 
#' @details This function collects specified columns from \code{L0_wide}, converts into long (attribute-value) form with \code{variable_name} names and values to the resulting table's "variable_name" and "value" columns, respectively. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column.
#'
#' @return (data.frame) The observation table.
#' 
#' @export
#'
#' @examples
#' 
create_observation <- function(L0_wide, 
                               observation_id,
                               event_id = NULL, 
                               package_id,
                               location_id, 
                               datetime,
                               taxon_id,
                               variable_name,
                               unit = NULL) {
  message("Creating observation")
  
  validate_arguments(fun.name = "create_observation", fun.args = as.list(environment()))
  
  
  cols_to_gather <- c(observation_id, event_id, package_id, location_id, datetime, taxon_id, variable_name, unit)
  res <- L0_wide %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(across(variable_name, as.numeric)) %>% 
    tidyr::pivot_longer(variable_name, names_to = "variable_name", values_to = "value") %>%
    dplyr::arrange(variable_name, observation_id)
  # add units
  if (!is.null(unit)) {
    unit_map <- L0_wide %>% dplyr::select(unit) %>% na.omit()
    unit_map <- data.frame(variable_name = colnames(unit_map),
                           unit = as.character(unique.data.frame(unit_map)),
                           stringsAsFactors = FALSE)
    unit_map$variable_name <- stringr::str_remove_all(unit_map$variable_name, 
                                                      "unit_")
    res <- dplyr::left_join(res, unit_map, by = "variable_name")
  }
  # add missing cols
  if (is.null(event_id)) {
    res$event_id <- NA_character_
  }
  if (is.null(unit)) {
    res$unit <- NA_character_
  }
  # reorder
  res <- res %>%
    dplyr::select(observation_id, event_id, package_id, location_id, datetime, taxon_id, variable_name, value, unit)
  # recalculate observation_id
  if (any(duplicated(res$observation_id))) {
    res$observation_id <- seq(nrow(res))
  }
  return(res)
}