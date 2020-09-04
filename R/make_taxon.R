#' Make taxon table
#'
#' @description  
#'     Make a data frame of the taxon table.
#'
#' @param taxa
#'     (character) Taxa names to be resolved
#' @param taxon.id
#'     (character) Primary key for the taxon table (i.e. taxon_id) 
#'     corresponding to the order of taxa names provided to the taxa argument.
#'     If left blank, then taxon_id values are auto generated.
#' @param name.type
#'     (character) Taxonomic name type. Can be: 'scientific', 'common', or 
#'     'both'.
#' @param data.sources
#'     (integer) An ordered numeric vector of ID's corresponding to data 
#'     sources (i.e. taxonomic authorities) you'd like to query, in the order 
#'     of decreasing preference. Run `taxonomyCleanr::view_taxa_authorities` 
#'     to see supported data sources. Columns `resolve_sci_taxa`, and 
#'     `resolve_comm_taxa` correspond to scientific and common searches.
#'
#' @return 
#'     (data frame) A fully populated taxon table. NA's are listed when an 
#'     authority match could not be made.
#'     
#' @export
#'

make_taxon <- function(taxa, taxon.id = NULL, name.type, data.sources){
  
  message('Creating the ecocomDP taxon table')
  
  # Validate arguments --------------------------------------------------------
  
  if (!missing(taxa)){
    if (class(taxa) != 'character'){
      stop('Input argument "taxa" is not of character class')
    }
  } else {
    stop('Input argument "taxa" is missing.')
  }
  
  if (!is.null(taxon.id)){
    if (class(taxon.id) != 'character'){
      stop('Input argument "taxon.id" is not of character class')
    }
    if (length(taxon.id) != length(taxa)){
      stop('Length of argument "taxon.id" does not match length Of argument "taxa"')
    }
  } 

  if (!missing(name.type)){
    name.type <- tolower(name.type)
    if ((!stringr::str_detect(name.type, 'scientific')) & 
        (!stringr::str_detect(name.type, 'common')) &
        (!stringr::str_detect(name.type, 'both'))){
      stop('Input argument "name.type" must be "scientific", "common", or "both"')
    }
  } else {
    stop('Input argument "name.type" is missing')
  }
  
  if (missing(data.sources)){
    stop('Input argument "data.sources" is missing')
  }
  
  # Resolve to authorities ----------------------------------------------------
  
  if (name.type == 'scientific'){
    
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = data.sources,
      x = taxa
    )
    
    # Add to taxon table
    
    taxon <- data.frame(
      taxon_id = rep(NA_character_, nrow(taxa_resolved)),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F
    )

  } else if (name.type == 'common'){
    
    taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = data.sources,
      x = taxa
    )
    
    # Add to taxon table
    
    taxon <- data.frame(
      taxon_id = rep(NA_character_, nrow(taxa_resolved)),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F
    )
    
  } else if (name.type == 'both'){
    
    authorities <- taxonomyCleanr::view_taxa_authorities()
    
    # Scientific
    
    use_i <- data.sources %in% authorities$id[
      authorities$resolve_sci_taxa == 'supported'
    ]
    
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = data.sources[use_i],
      x = taxa
    )
    
    # Common
    
    index <- is.na(taxa_resolved$taxa_clean)
    
    use_i <- data.sources %in% authorities$id[
      authorities$resolve_comm_taxa == 'supported'
      ]
    
    taxa_comm_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = data.sources[use_i],
      x = taxa_resolved$taxa[index]
    )
    
    # Combine
    
    taxa_resolved[index, ] <- taxa_comm_resolved
    
    # Add to taxon table
    
    taxon <- data.frame(
      taxon_id = rep(NA_character_, nrow(taxa_resolved)),
      taxon_rank = taxa_resolved$rank,
      taxon_name = taxa_resolved$taxa,
      authority_system = taxa_resolved$authority,
      authority_taxon_id = taxa_resolved$authority_id,
      stringsAsFactors = F
    )
    
  }
  
  # Add taxon_id --------------------------------------------------------------
  
  if (is.null(taxon.id)){
    
    taxon$taxon_id <- paste0(
      'tx_',
      seq(nrow(taxon))
    )
    
  } else {
    
    taxon$taxon_id <- taxon.id
    
  }
  
  # Return --------------------------------------------------------------------
  
  taxon

}
