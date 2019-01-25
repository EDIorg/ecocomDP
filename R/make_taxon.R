#' Make taxon table
#'
#' @description  
#'     Make a data frame of the taxon table.
#'
#' @usage 
#'     make_taxon(taxa, taxon.id = NULL, name.type, data.sources)
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
    if ((!stringr::str_detect(name.type, 'scientific')) | 
        (!stringr::str_detect(name.type, 'common')) |
        (!stringr::str_detect(name.type, 'both'))){
      stop('Input argument "name.type" must be "scientific", "common", or "both"')
    }
  } else {
    stop('Input argument "name.type" is missing')
  }
  
  if (missing(data.sources)){
    stop('Input argument "data.sources" is missing')
  }
  
  # ----------------------
  
  
  
}