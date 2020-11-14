#' Upload data package to a repository
#'
#' @description A wrapper function to repository specific upload methods.
#'
#' @param path (character) Directory in which L1 tables, scripts, and metadata exist.
#' @param package.id (character) Identifier of data package to be uploaded
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @return (character) Evaluation/upload summary
#' @export
#'
#' @examples
#' 
upload_to_repository <- function(path,
                                 package.id,
                                 user.id,
                                 user.pass) {
  
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
  
  # Upload --------------------------------------------------------------------
  
  if (repository == "EDI") {
    
    eml <- EDIutils::api_update_data_package(
      path = path, 
      package.id = package.id, 
      environment = environment, 
      user.id = user.id, 
      user.pass = user.pass,
      affiliation = repository)
    
  }
  
}