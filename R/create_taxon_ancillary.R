#' Create the taxon_ancillary table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param taxon_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique organism at the observation level.
#' @param datetime (character) An optional in \code{L0_wide} containing the date, and if applicable time, of ancillary location data following the ISO-8601 standard format (i.e. YYYY-MM-DD hh:mm:ss).
#' @param variable_name (character) Columns in \code{L0_wide} containing the ancillary taxon data.
#' @param unit (character) The optional column in \code{L0_wide} containing the units of each \code{variable_name} following the column naming convention <unit>_<variable_name> (e.g. "unit_average_length").
#' @param author (character) An optional column in \code{L0_wide} containing the person associated with identification of taxa in the taxon table.
#' 
#' @details This function collects specified columns from \code{L0_wide}, converts into long (attribute-value) form with \code{variable_name} names and values to the resulting table's "variable_name" and "value" columns, respectively. Regular expression matching joins \code{unit} to any associated \code{variable_name} and is listed in the resulting table's "unit" column. Default names of optional columns are ignored if they can't be found in \code{L0_wide} (i.e. no need to set as NULL).
#'
#' @return (data.frame) The taxon_ancillary table of ecocomDP.
#' @export
#'
#' @examples
#' 
create_taxon_ancillary <- function(L0_wide, 
                                   taxon_id = "taxon_id", 
                                   datetime = "datetime", 
                                   variable_name, 
                                   unit = NULL, 
                                   author = "author") {
  message("Creating taxon_ancillary")
  # TODO: validate_arguments()
  # - cols exist in L0_wide for non-required cols
  # - NULL optional cols if not in L0_wide
  # - rename cols in L0_wide if not 1-to-1 match
  # - check unit_variable_name convention
  # - required cols vs optional cols
  # - return
  
  # gather cols
  cols_to_gather <- c(taxon_id, datetime, variable_name, author)
  res <- L0_wide %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::mutate(across(variable_name, as.character)) %>% # ancillary table variable_name needs character coercion
    tidyr::pivot_longer(variable_name, names_to = "variable_name", values_to = "value") %>%
    dplyr::arrange(taxon_id)
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
  if (is.null(datetime)) {
    res$datetime <- NA_character_
  }
  if (is.null(unit)) {
    res$unit <- NA_character_
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
  return(res)
}