#' Event notification handler
#'
#' @description Runs on a chron. Checks for unprocessed items in SQlite using webservices defined in ADD WEBSERVICE DEFINITIONS HERE and pops the longest waiting item into the appropriate workflow. Currently supported are \code{update_L1()} or \code{update_L2_dwca()}.
#' 
#' @param config (character) Full path to configuration file. The config file defines global variables for parameterizing workflows in this function. The config.R file sets these variables:
#' \itemize{
#' \item{"config.repository": Repository system. Must be one of the supported repositories.}
#' \item{"config.environment": Repository environment. Some repositories have development and production environments.}
#' \item{"config.path": Path to directory containing workflow files}
#' \item{"config.www": Public URL for repository access to data and metadata files}
#' \item{"config.user.id": User ID for upload to repository}
#' \item{"config.user.pass": User password for upload to repository}
#' }
#'
#' @export
#'
routine_handler <- function(config) {
  
  # Parameterize --------------------------------------------------------------
  
  source(file = config)
  
  # Pull from queue -----------------------------------------------------------
  
  next_in_line <- pull_from_queue(config.repository)
  
  # Continue if there is anything to process
  
  if (is.null(next_in_line)) {
    return(NULL)
  }
  
  # Identify workflows to call ------------------------------------------------
  
  # Knowing which workflow to call depends on whether the data package is a 
  # parent of an L1 or a parent of one or more L2.
  
  parent_of_L1 <- has_child("ecocomDP", next_in_line$package.id)
  
  if (!parent_of_L1) {
    parent_of_dwcae <- has_child(
      "Darwin Core Archive (DwC-A) Event Core", next_in_line$package.id)
  }
  
  # Lock ----------------------------------------------------------------------
  
  # TODO: Create lock file in dir of ecocom-listener
  
  # Call workflow -------------------------------------------------------------
  
  # TODO: Refactor for iteration when more than one L1-to-L2 workflow exists.
  
  if (parent_of_L1) {
    
    # update_L1(
    #   package.id.L0 = package.id, # newest L0 id arriving via event notification
    #   path = config.path, 
    #   url = config.www,
    #   user.id = config.user.id,
    #   user.pass = config.user.pass)
    
    # TODO: Complete workflow and implement on server
    
    # Manual testing:
    update_L1(
      package.id.L0 = "edi.95.5",
      path = config.path, 
      url = config.www, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (parent_of_dwcae) {
    
    update_L2_dwca(
      package.id.L1 = package.id,
      package.id.L2 = names(parent_of_dwcae),
      core.name = "event",
      path = config.path,
      url = config.www,
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else {
    
    "Input data is not one of the supported types, and will not be processed."
    
  }
  
  # Compile log ---------------------------------------------------------------
  
  # Message maintainers -------------------------------------------------------
  
  message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
  # Remove from queue ---------------------------------------------------------
  
  # qind
  # r <- httr::DELETE(
  #   paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
  
  # Unlock --------------------------------------------------------------------
  
}








#' Pull next item from the ecocom-listener's queue
#'
#' @param repository (character) Repository hosting the ecocom-listener. Facilitates repository specific methods. Currently supported is: EDI.
#'
#' @return
#' \item{index}{(integer) Index of item in queue. Is later used for removing the item from the queue.}
#' \item{package.id}{(character) Data package identifier}
#' \item{config.environment}{(character) Location of the \code{package.id} within a repository system. This variable is written to the Global Environment for use in \code{routine_handler()}.}
#'
pull_from_queue <- function(repository) {
  
  # EDI -----------------------------------------------------------------------
  
  if (repository == "EDI") {
    
    # EDI has listeners in "production" and "staging" environments
    prd <- httr::GET(
      "https://regan.edirepository.org/ecocom-listener/package.lternet.edu")
    stg <- httr::GET(
      "https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu")
    
    # Production has priority over staging
    if (httr::status_code(prd) == 200) {
      assign("config.environment", "production", envir = .GlobalEnv)
      cntnt <- httr::content(prd, as = "text")
    } else if (httr::status_code(stg) == 200) {
      assign("config.environment", "staging", envir = .GlobalEnv)
      cntnt <- httr::content(stg, as = "text")
    }
    
    # Return parsed content
    if (exists("cntnt")) {
      res <- list(
        index = as.integer(stringr::str_split(cntnt, ",", simplify = T)[1]),
        package.id = stringr::str_split(cntnt, ",", simplify = T)[2])
      return(res)
    }

  }
  
  # Other repository ----------------------------------------------------------
  
}