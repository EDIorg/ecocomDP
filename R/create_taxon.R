#' Create the taxon table
#'
#' @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
#' @param taxon_id (character) Column in \code{L0_flat} containing the identifier assigned to each unique organism at the observation level.
#' @param taxon_rank (character) An optional column in \code{L0_flat} containing the taxonomic rank of the organism in \code{taxon_name}.
#' @param taxon_name (character) Column in \code{L0_flat} containing the taxonomic name of the organism.
#' @param authority_system (character) An optional column in \code{L0_flat} containing the name of the authority system \code{authority_taxon_id} is from (e.g. "ITIS").
#' @param authority_taxon_id (character) An optional column in \code{L0_flat} containing the identifier corresponding to \code{taxon_name} in the \code{authority_system}.
#' 
#' @details This function collects specified columns from \code{L0_flat} and returns distinct rows.
#' 
#' Taxa listed in the taxon table, and resolved to one of the supported authority systems (i.e. \href{https://www.itis.gov/}{ITIS}, \href{https://www.marinespecies.org/}{WORMS}, or \href{https://gbif.org}{GBIF}), will have their full taxonomic hierarchy expanded, including any common names for each level.
#' 
#' "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#'
#' @return (tbl_df, tbl, data.frame) The taxon table.
#' 
#' @export
#'
#' @examples
#' flat <- ants_L0_flat
#' 
#' taxon <- create_taxon(
#'   L0_flat = flat, 
#'   taxon_id = "taxon_id", 
#'   taxon_rank = "taxon_rank", 
#'   taxon_name = "taxon_name", 
#'   authority_system = "authority_system", 
#'   authority_taxon_id = "authority_taxon_id")
#' 
#' taxon
#' 
create_taxon <- function(L0_flat, 
                         taxon_id,
                         taxon_rank = NULL, 
                         taxon_name,
                         authority_system = NULL, 
                         authority_taxon_id = NULL) {
  
  validate_arguments(fun.name = "create_taxon", fun.args = as.list(environment()))
  
  # gather cols
  cols_to_gather <- c(taxon_id, taxon_rank, taxon_name, authority_system, authority_taxon_id)
  res <- L0_flat %>%
    dplyr::select(all_of(cols_to_gather)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(taxon_id)
  # add missing cols
  if (is.null(taxon_rank)) {
    res$taxon_rank <- NA_character_
  }
  if (is.null(authority_system)) {
    res$authority_system <- NA_character_
  }
  if (is.null(authority_taxon_id)) {
    res$authority_taxon_id <- NA_character_
  }
  # reorder
  res <- res %>%
    dplyr::select(taxon_id, taxon_rank, taxon_name, authority_system, authority_taxon_id)
  # coerce classes
  res <- coerce_table_classes(res, "taxon", class(L0_flat))
  return(res)
}