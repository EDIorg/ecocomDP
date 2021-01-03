#' Run L0 to L1 conversion script
#' 
#' @description Repository specific methods for handling script arguments is enabled by this function.
#'
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param package.id.L0 (character) Identifier of newest L0 data package.
#' @param package.id.L1 (character) Identifier of new L1 being created by this script
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#'
#' @return
#' @export
#'
run_conversion_script <- function(path, 
                                  package.id.L0, 
                                  package.id.L1, 
                                  url) {
  
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
  
  # Run script ----------------------------------------------------------------
  
  message(paste0("Creating new L1 (", package.id.L1, ") from newest L0 (", 
                 package.id.L0, ")."))
  
  if (repository == "EDI") {
    
    r <- create_ecocomDP(
      path = path,
      package.id.L0 = package.id.L0, 
      package.id.L1 = package.id.L1, 
      environment = environment,
      url = url)
    
  }
    
}