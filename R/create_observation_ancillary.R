#' Create the observation_ancillary table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param observation_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique observation.
#' @param variable_name (character) Columns in \code{L0_wide} containing the ancillary observation data.
#' @param unit (character) An optional column in \code{L0_wide} containing the units of each \code{variable_name} following the column naming convention: unit_<variable_name> (e.g. "unit_temperature").
#' 
#' @details This function collects specified columns from \code{L0_wide}, converts into long (attribute-value) form with \code{variable_name} names and values to the resulting table's "variable_name" and "value" columns, respectively. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column.
#'
#' @return (data.frame) The observation_ancillary table.
#' 
#' @export
#'
#' @examples
#' wide <- ants_L0_wide
#' 
#' observation_ancillary <- create_observation_ancillary(
#'   L0_wide = wide,
#'   observation_id = "observation_id", 
#'   variable_name = c("trap.type", "trap.num", "moose.cage"))
#' 
#' observation_ancillary
#' 
create_observation_ancillary <- function(L0_wide, 
                                         observation_id, 
                                         variable_name, 
                                         unit = NULL) {
  message("Creating observation_ancillary")
  
  validate_arguments(fun.name = "create_observation_ancillary", fun.args = as.list(environment()))
  
  # gather cols
  cols_to_gather <- c(observation_id, variable_name)
  res <- L0_wide %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(across(variable_name, as.character)) %>% # ancillary table variable_name needs character coercion
    tidyr::pivot_longer(variable_name, names_to = "variable_name", values_to = "value") %>%
    dplyr::arrange(observation_id)
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
  if (is.null(unit)) {
    res$unit <- NA_character_
  }
  # keep only distinct values
  res <- dplyr::distinct(res)
  # add primary key
  res$observation_ancillary_id <- seq(nrow(res))
  # reorder
  res <- res %>%
    dplyr::select(observation_ancillary_id, observation_id, variable_name, value, unit)
  return(res)
}