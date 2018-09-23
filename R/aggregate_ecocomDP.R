#' aggregate_ecocomDP
#'
#' @description  
#'     Aggregate one or more ecocomDP.
#'
#' @usage aggregate_ecocomDP(package.ids)
#'     
#' @param package.ids
#'     (character) One or more data package IDs listed in the data frame created by 
#'     `list_all_ecocomDP`. EDI data package IDs are of the form 
#'     'edi.package_number.revision_number' (e.g. edi.100.2). NEON data package
#'     IDs are of the form 'data_product.identifier.revision_number' (e.g.
#'     'DP1.20120.001'). NOTE: Previous data package revisions of ecocomDP 
#'     packages are valid entries.
#'     
#' @return 
#'     A single ecocomDP containing all the content of the individual ecocomDP
#'     listed in the argument "package.ids".
#'         
#' @export
#'

aggregate_ecocomDP <- function(){
  
  
  
}