#' Clean taxon
#'
#' @description  
#'     Clean taxonomy data of level-0 dataset and export to file.
#'
#' @usage clean_taxon(path, parent.package.id, table.name, taxon.col)
#' 
#'     Run this function to clean up the taxonomy data of an input dataset
#'     prior to running this information through the Integrated Taxonomic 
#'     Information System (ITIS) to obtain rank values and a Taxonomic Serial
#'     Number.
#'
#' @param path 
#'     A path to the dataset working directory where the clean taxon table will
#'     be exported.
#'     
#' @param parent.package.id
#'     Identifier of parent data package (e.g. knb-lter-hfr.118.28).
#'     
#' @param table.name
#'     Full name of table from which the taxonomy data is stored.
#'     
#' @param taxon.col
#'     Column names containing the taxonomy data.
#'
#' @return 
#'     A tab delimited UTF-8 file in the ecocomDP working directory titled 
#'     \emph{taxon_clean.txt} containing clean taxon names that can then be
#'     used to query ITIS.
#'     
#' @details 
#'     This function overwrites
#'     \emph{taxon_clean.txt} files you have created in the ecocomDP 
#'     working directory.
#'
#' @export
#'


clean_taxon <- function(path, parent.package.id, table.name, col.name) {
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  if (missing(parent.package.id)){
    stop("Specify path to dataset working directory.")
  }
  
  # Parameters ----------------------------------------------------------------
  
  
  # Begin function ------------------------------------------------------------

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new variable tables? This will overwrite your previous work! (y or n):  ")
  
  if (answer == "y"){
    
  }
}

