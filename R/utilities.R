#' Increment data package version number
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Package identifier with version number incremented by 1.
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' increment_package_version("edi.100.1")
#' 
increment_package_version <- function(package.id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  # Repository specific methods -----------------------------------------------
  
  if (repository == "EDI") {
    
    parts <- unlist(stringr::str_split(package.id, "\\."))
    parts[3] <- as.character(as.numeric(parts[3]) + 1)
    parts <- paste(parts, collapse = ".")
    return(parts)
    
  }
  
}








#' Get previous data package version
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Previous data package version
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' \dontrun{
#' #' get_previous_data_package_version("edi.100.2")
#' }
#' 
get_previous_version <- function(package.id) {
  
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
    
    parts <- unlist(stringr::str_split(package.id, "\\."))
    
    vers <- suppressMessages(
      EDIutils::api_list_data_package_revisions(
        scope = parts[1],
        identifier = parts[2],
        environment = environment))
    
    if (length(vers) == 1) {
      return("0")
    }
    
    parts[3] <- vers[length(vers) - 1]
    parts <- paste(parts, collapse = ".")
    return(parts)
    
  }
  
}