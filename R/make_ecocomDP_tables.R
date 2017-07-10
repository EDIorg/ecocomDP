#' Make ecocomDP tables
#'
#' @description  
#'     Translate user supplied data tables to the Ecological Community Data
#'     Pattern (ecocomDP) then write to file.
#'
#' @usage 
#'     make_ecocomDP_tables(path)
#'
#' @param path 
#'     A path to the dataset working directory containing the level-0 (raw)
#'     data tables and the level-0 to level-1 mapping (map.txt).
#'
#' @return 
#'     \emph{taxon.csv} A comma delimited UTF-8 file containing taxonomic data.
#'     A taxon is a unique taxonomic entity recorded at some taxon rank. Each
#'     taxon should be associated with an authority, which can be a citation or
#'     or URL. An authority taxon id (such as an ITIS TSN) should be included
#'     if available. [Click here for schema details](https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/taxon.html).
#'     \itemize{
#'         \item \strong{taxon_id} Identifier assigned to each unique entry,
#'         most likely code-generated.
#'         \item \strong{taxon_level}
#'     
#'     
#'     }
#'     
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be imported.
#'     
#' @export     
#'     


make_ecocomDP_tables <- function(path){
  
  
  
  
}
