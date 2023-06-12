#' Create the taxon_ancillary table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param taxon_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique organism at the observation level.
#' @param datetime (character) An optional in \code{L0_flat} containing the date, and if applicable time, of ancillary location data following the ISO-8601 standard format (e.g. YYYY-MM-DD hh:mm:ss).
#' @param variable_name (character) Columns in \code{L0_flat} containing the ancillary taxon data.
#' @param unit (character) An optional column in \code{L0_flat} containing the units of each \code{variable_name} following the column naming convention: unit_<variable_name> (e.g. "unit_average_length").
#' @param author (character) An optional column in \code{L0_flat} containing the person associated with identification of taxa in the taxon table.
#' 
#' @details This function collects specified columns from \code{L0_flat}, converts into long (attribute-value) form by gathering \code{variable_name}. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column.
#' 
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return (tbl_df, tbl, data.frame) The taxon_ancillary table.
#' 
#' @export
#'
#' @examples
#' flat <- ants_L0_flat
#' 
#' taxon_ancillary <- create_taxon_ancillary(
#'   L0_flat = flat,
#'   taxon_id = "taxon_id",
#'   variable_name = c(
#'     "subfamily", "hl", "rel", "rll", "colony.size", 
#'     "feeding.preference", "nest.substrate", "primary.habitat", 
#'     "secondary.habitat", "seed.disperser", "slavemaker.sp", 
#'     "behavior", "biogeographic.affinity", "source"),
#'   unit = c("unit_hl", "unit_rel", "unit_rll"))
#' 
#' taxon_ancillary
#' 
create_taxon_ancillary <- function(L0_flat, 
                                   taxon_id, 
                                   datetime = NULL, 
                                   variable_name, 
                                   unit = NULL, 
                                   author = NULL) {
  
  validate_arguments(fun.name = "create_taxon_ancillary", fun.args = as.list(environment()))
  
  # gather cols
  cols_to_gather <- c(taxon_id, datetime, variable_name, author)
  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(across(any_of(variable_name), as.character)) %>% # ancillary table variable_name needs character coercion
    tidyr::pivot_longer(any_of(variable_name), names_to = "variable_name", values_to = "value") %>%
    dplyr::arrange(taxon_id)
  # add units
  res <- add_units(L0_flat, res, unit)
  # add missing cols
  if (is.null(datetime)) {
    res$datetime <- NA_character_
  }
  if (is.null(author)) {
    res$author <- NA_character_
  }
  # keep only distinct values
  res <- dplyr::distinct(res)
  # add primary key
  res$taxon_ancillary_id <- seq(nrow(res))
  # reorder
  res <- res %>%
    dplyr::select(taxon_ancillary_id, taxon_id, datetime, variable_name, value, unit, author)
  # coerce classes
  res <- coerce_table_classes(res, "taxon_ancillary", class(L0_flat))
  return(res)
}