#' Create the observation table
#'
#' @param L0_flat (data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param observation_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique observation.
#' @param event_id (character) An optional column in \code{L0_flat} containing the identifier assigned to each unique sampling event.
#' @param package_id (character) Column in \code{L0_flat} containing the identifier this dataset will have once it's published in a data repository.
#' @param location_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique location at the observation level.
#' @param datetime (character) Column in \code{L0_flat} containing the date, and if applicable time, of the observation following the ISO-8601 standard format (i.e. YYYY-MM-DD hh:mm:ss).
#' @param taxon_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique organism at the observation level.
#' @param variable_name (character) Column in \code{L0_flat} containing the names of variables measured.
#' @param value (character) Column in \code{L0_flat} containing the values of \code{variable_name}.
#' @param unit (character) An optional column in \code{L0_flat} containing the units of \code{variable_name}.
#' 
#' @details "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" ecocomDP tables can be consistely spread due to the frequent occurence of L0 source datasets with > 1 core observation variable.
#' 
#' This function collects specified columns from \code{L0_flat}, converts into long (attribute-value) form with \code{variable_name} names and values to the resulting table's "variable_name" and "value" columns, respectively. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column.
#'
#' @return (data.frame) The observation table.
#' 
#' @export
#'
#' @examples
#' flat <- ants_L0_flat
#' 
#' observation <- create_observation(
#'   L0_flat = flat, 
#'   observation_id = "observation_id", 
#'   event_id = "event_id", 
#'   package_id = "package_id",
#'   location_id = "location_id", 
#'   datetime = "datetime", 
#'   taxon_id = "taxon_id", 
#'   variable_name = "variable_name",
#'   value = "value",
#'   unit = "unit")
#' 
#' observation
#' 
create_observation <- function(L0_flat, 
                               observation_id,
                               event_id = NULL, 
                               package_id,
                               location_id, 
                               datetime,
                               taxon_id,
                               variable_name,
                               value,
                               unit = NULL) {
  message("Creating observation")
  validate_arguments(fun.name = "create_observation", fun.args = as.list(environment()))
  # Get cols
  cols_to_gather <- c(observation_id, event_id, package_id, location_id, datetime, taxon_id, variable_name, value, unit)
  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::arrange(variable_name, observation_id)
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
  return(res)
}