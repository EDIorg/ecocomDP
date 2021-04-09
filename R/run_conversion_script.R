#' Run L0 to L1 conversion script
#' 
#' @description Repository specific methods for handling script arguments is enabled by this function.
#'
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param id.L0.newest (character) Identifier of newest L0 data package.
#' @param id.L1.next (character) Identifier of new L1 being created by this script
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#'
#' @return
#' @export
#'
run_conversion_script <- function(path, 
                                  id.L0.newest, 
                                  id.L1.next, 
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
  
  message(paste0("Creating new L1 (", id.L1.next, ") from newest L0 (", 
                 id.L0.newest, ")."))
  
  if (repository == "EDI") {
    
    r <- create_ecocomDP(
      path = path,
      package.id.L0 = id.L0.newest, 
      package.id.L1 = id.L1.next, 
      environment = environment,
      url = url)
    
  }
    
}