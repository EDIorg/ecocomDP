#' Make the taxon table
#'
#' @param x
#'     (data frame) A data frame containing taxa names (in one or more 
#'     columns).
#' @param cols
#'     (character) One or more columns in \code{x} that when combined form a 
#'     unique taxa that may be resolvable to a taxonomic authority system. 
#'     (e.g. \code{cols = c("genus", "species")}).
#' @param taxa
#'     (character) If not using \code{x} and \code{cols}, then a list of 
#'     unique taxa can be input using this argument.
#' @param name.type
#'     (character) Taxonomic name type. Can be: 'scientific', 'common', or 
#'     'both'.
#' @param data.sources
#'     (integer) An ordered numeric vector of ID's corresponding to taxonomic 
#'     authorities to resolve taxa to (in the order of decreasing preference). 
#'     Run \code{taxonomyCleanr::view_taxa_authorities()} to see supported 
#'     authorities.
#'
#' @return 
#' A list containing:
#' \item{taxon}{A data frame of the taxon table}
#' \item{x}{The input \code{x} with an added taxon_id column linking unique
#' \code{cols} to the list of taxa in the taxon table.}
#'     
#' @export
#'
make_taxon <- function(
  x = NULL, cols = NULL, taxa = NULL, name.type = "scientific", 
  data.sources = NULL) {
  
  message('Creating taxon')
  
  # Validate arguments --------------------------------------------------------
  
  if (!is.null(x) & is.null(cols)) {
    stop('Input argument "cols" is missing! Specify column names that create ',
         'unique taxa.', call. = FALSE)
  }
  
  if (!is.null(taxa)){
    if (class(taxa) != 'character'){
      stop('Input argument "taxa" is not of character class', call. = FALSE)
    }
  }

  if (!is.null(name.type)){
    name.type <- tolower(name.type)
    if ((!stringr::str_detect(name.type, 'scientific')) & 
        (!stringr::str_detect(name.type, 'common')) &
        (!stringr::str_detect(name.type, 'both'))){
      stop('Input argument "name.type" must be "scientific", "common", or ',
           '"both"', call. = FALSE)
    }
  } else {
    stop('Input argument "name.type" is missing', call. = FALSE)
  }
  
  if (is.null(data.sources)){
    stop('Input argument "data.sources" is missing', call. = FALSE)
  }
  
  # Create "taxa" if using "x" and "cols"
  if (!is.null(x) & !is.null(cols)) {
    x$keys <- apply(x[, cols] ,1 ,paste ,collapse = " ")
    taxa <- unique(x$keys)
  }
  
  # Resolve to authorities ----------------------------------------------------
  
  if (name.type == 'scientific') {
    
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = data.sources,
      x = taxa)
    
    taxon <- data.frame(
      taxon_id = paste0("tx_", seq(nrow(taxa_resolved))),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F)

  } else if (name.type == 'common'){
    
    taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = data.sources,
      x = taxa)
    
    taxon <- data.frame(
      taxon_id = paste0("tx_", seq(nrow(taxa_resolved))),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F)
    
  } else if (name.type == 'both'){
    
    authorities <- taxonomyCleanr::view_taxa_authorities()
    
    # Scientific
    use_i <- data.sources %in% authorities$id[
      authorities$resolve_sci_taxa == 'supported']
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = data.sources[use_i],
      x = taxa)
    
    # Common
    index <- is.na(taxa_resolved$taxa_clean)
    use_i <- data.sources %in% authorities$id[
      authorities$resolve_comm_taxa == 'supported']
    taxa_comm_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = data.sources[use_i],
      x = taxa_resolved$taxa[index])
    
    # Combine
    taxa_resolved[index, ] <- taxa_comm_resolved
    
    # Add to taxon table
    taxon <- data.frame(
      taxon_id = paste0("tx_", seq(nrow(taxa_resolved))),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F)
    
  }

  # Return --------------------------------------------------------------------
  
  if (!is.null(x) & !is.null(cols)) {
    x$keys <- taxon$taxon_id[match(x$keys, taxon$taxon_name)]
    x <- dplyr::rename(x, taxon_id = keys)
    return(list(taxon = taxon, x = x))
  } else {
    return(list(taxon = taxon, x = NULL))
  }

}
