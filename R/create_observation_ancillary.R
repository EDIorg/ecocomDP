#' Create the observation_ancillary table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param observation_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique observation.
#' @param variable_name (character) Columns in \code{L0_flat} containing the ancillary observation data.
#' @param unit (character) An optional column in \code{L0_flat} containing the units of each \code{variable_name} following the column naming convention: unit_<variable_name> (e.g. "unit_temperature").
#' 
#' @details This function collects specified columns from \code{L0_flat}, converts into long (attribute-value) form by gathering \code{variable_name}. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column.
#' 
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return (tbl_df, tbl, data.frame) The observation_ancillary table.
#' 
#' @export
#'
#' @examples
#' flat <- ants_L0_flat
#' 
#' observation_ancillary <- create_observation_ancillary(
#'   L0_flat = flat,
#'   observation_id = "observation_id", 
#'   variable_name = c("trap.type", "trap.num", "moose.cage"))
#' 
#' observation_ancillary
#' 
create_observation_ancillary <- function(L0_flat, 
                                         observation_id, 
                                         variable_name, 
                                         unit = NULL) {
  
  validate_arguments(fun.name = "create_observation_ancillary", fun.args = as.list(environment()))
  
  # gather cols
  cols_to_gather <- c(observation_id, variable_name)
  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(across(any_of(variable_name), as.character)) %>% # ancillary table variable_name needs character coercion
    tidyr::pivot_longer(any_of(variable_name), names_to = "variable_name", values_to = "value") %>%
    dplyr::arrange(observation_id)
  # add units
  res <- add_units(L0_flat, res, unit)
  # keep only distinct values
  res <- dplyr::distinct(res)
  # add primary key
  res$observation_ancillary_id <- seq(nrow(res))
  # reorder
  res <- res %>%
    dplyr::select(observation_ancillary_id, observation_id, variable_name, value, unit)
  # coerce classes
  res <- coerce_table_classes(res, "observation_ancillary", class(L0_flat))
  return(res)
}