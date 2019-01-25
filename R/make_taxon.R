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
  
  
  
}