#' Does a data package have a Darwin Core Archive Event Core child?
#'
#' @param package.id (character) Data package identifier
#'
#' @return (logical) TRUE/FALSE
#' 
#' @export
#'
has_child_dwcae <- function(package.id) {
  
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
  
  # Repository specific methods -----------------------------------------------
  
  if (repository == "EDI") {
    
    children <- xml2::xml_text(
      xml2::xml_find_all(
        suppressMessages(
          EDIutils::api_list_data_descendants(package.id, environment)),
        ".//packageId"))
    
    if (length(children) != 0) {
      r <- sapply(
        children,
        function(child) {
          eml <- suppressMessages(
            EDIutils::api_read_metadata(child, environment))
          res <- "Darwin Core Archive (DwC-A) Event Core" %in% 
            xml2::xml_text(xml2::xml_find_all(eml, ".//keyword"))
          return(res)
        })
      return(any(r))
    } else {
      return(FALSE)
    }
    
  }
  
}