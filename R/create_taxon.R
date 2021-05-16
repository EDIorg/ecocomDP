#' Create the taxon table
#'
#' @param L0_wide (data.frame) The fully joined source L0 dataset, in wide format.
#' @param taxon_id (character) Column in \code{L0_wide} containing the identifier assigned to each unique organism at the observation level.
#' @param taxon_rank (character) The optional column in \code{L0_wide} containing the taxonomic rank of the organism in \code{taxon_name}.
#' @param taxon_name (character) Column in \code{L0_wide} containing the taxonomic name of the organism.
#' @param authority_system (character) An optional column in \code{L0_wide} containing the name of the authority system \code{authority_taxon_id} is from (e.g. "ITIS").
#' @param authority_taxon_id (character) An optional column in \code{L0_wide} containing the identifier corresponding to \code{taxon_name} in the \code{authority_system}.
#' 
#' @details This function collects specified columns from \code{L0_wide} and returns distinct rows. Default names of optional columns are ignored if they can't be found in \code{L0_wide} (i.e. no need to set as NULL).
#' 
#' If taxon authority_system contains values of .... then the corresponding taxa will have their whole hierarchy expanded within the EML metadata returned by create_eml().
#'
#' @return (data.frame) The taxon table of ecocomDP.
#' @export
#'
#' @examples
#' 
create_taxon <- function(L0_wide, 
                         taxon_id = "taxon_id",
                         taxon_rank = "taxon_rank", 
                         taxon_name = "taxon_name",
                         authority_system = "authority_system", 
                         authority_taxon_id = "authority_taxon_id") {
  message("Creating taxon")
  # TODO: validate_arguments()
  # - cols exist in L0_wide for non-required cols
  # - NULL optional cols if not in L0_wide
  # - rename cols in L0_wide if not 1-to-1 match
  # - return
  # - required cols vs optional cols
  
  # gather cols
  cols_to_gather <- c(taxon_id, taxon_rank, taxon_name, authority_system, authority_taxon_id)
  res <- L0_wide %>%
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
  return(res)
}