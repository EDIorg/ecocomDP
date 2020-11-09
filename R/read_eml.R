#' Read EML metadata from a data repository
#' 
#' @param package.id (character) Data package identifier
#' @param repository (character) Data repository in which \code{package.id} resides and associated with \code{environment}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which \code{package.id} exists. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows reading of EML from different environments. Default is "production".
#' 
#' @description A wrapper function to repository specific read methods (the user.domain arg drives the logic).
#'
#' @return (xml_document, xml_node) EML metadata
#' @export
#'
#' @examples
#' 
read_eml <- function(package.id,
                     repository = "EDI",
                     environment = "production") {
  
  return(eml)
  
}