#' Read EML metadata from a data repository
#' 
#' @description A wrapper function to repository specific read methods (the repository arg drives the logic).
#' 
#' @param package.id (character) Data package identifier
#'
#' @return (xml_document, xml_node) EML metadata
#' @export
#'
#' @examples
#' 
read_eml <- function(package.id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Get EML -------------------------------------------------------------------
  
  if (repository == "EDI") {
    eml <- api_read_metadata(package.id, environment)
  }
  
  return(eml)
  
}