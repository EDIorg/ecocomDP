#' Event notification handler
#'
#' @description Runs on a chron. Checks for unprocessed items in SQlite using webservices defined in ADD WEBSERVICE DEFINITIONS HERE and pops the longest waiting item into the appropriate routine. Currently supported are \code{update_L1()} or \code{update_L2_dwca()}.
#' 
#' @param config (character) Full path to configuration file. The config file defines global variables for parameterizing routines in this function. The config.R file sets these variables:
#' \itemize{
#' \item{"config.repository": Repository system. Must be one of the supported repositories.}
#' \item{"config.environment": Repository environment. Some repositories have development and production environments.}
#' \item{"config.path": Path to directory containing routine files}
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
  
  # Check lock ----------------------------------------------------------------
  
  # Trying to run a routine when one is already underway creates issues
  
  if (file.exists(paste0(config.path, "/lock.txt"))) {
    return(NULL)
  }
  
  # Pull from queue -----------------------------------------------------------
  
  # Get next in line
  
  nil <- pull_from_queue(config.repository)
  
  if (is.null(nil)) {
    return(NULL)
  }
  
  # Identify routine(s) to call ----------------------------------------------
  
  # Knowing which routine to call depends on whether the "next data package
  # in line" is a parent of an L1 or a parent of one or more L2.
  
  # Because it's possible (but rare) for the next in line to already have a 
  # child created by one of the routines listed below, first check for such
  # descendants before continuing. This check safely assumes a previous 
  # version has valid parent attributes, which are tightly controlled by 
  # validate_ecocomDP().
  
  nil$parent_of_L1 <- has_child("ecocomDP", nil$package.id)
  if (nil$parent_of_L1) {
    # TODO: Remove from queue
    # r <- httr::DELETE(
    #   paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
    return(NULL)
  }
  
  if (!nil$parent_of_L1) {
    nil$parent_of_dwcae <- has_child(
      "Darwin Core Archive (DwC-A) Event Core", nil$package.id) 
    if (nil$parent_of_dwcae) {
      # TODO: Remove from queue
      # r <- httr::DELETE(
      #   paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
      return(NULL)
    }
  }
  
  # Was the previous version of the next in line a parent of a child created 
  # by one of the routines listed below? If not, the next in line may have 
  # wandered into the wrong queue and should be removed.
  
  nil$parent_of_L1 <- has_child(
    "ecocomDP", get_previous_version(nil$package.id))
  
  if (!nil$parent_of_L1) {
    nil$parent_of_dwcae <- has_child(
      "Darwin Core Archive (DwC-A) Event Core", 
      get_previous_version(nil$package.id)) 
  }
  
  if (!any(c(nil$parent_of_L1, nil$parent_of_dwcae))) {
    message(paste0("No supported routine for ", nil$package.id), ". Removing ",
            "from queue.")
    # TODO: Remove from queue
    # r <- httr::DELETE(
    #   paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
    return(NULL)
  }
  
  # Lock ----------------------------------------------------------------------
  
  # Lock file prevents race conditions by indicating a routine in progress
  
  file.create(paste0(config.path, "/lock.txt"))
  
  if (!file.exists(paste0(config.path, "/lock.txt"))) {
    message("Could not create lock.txt. Exiting routine_handler().")
    return(NULL)
  }
  
  # Call routine -------------------------------------------------------------
  
  # TODO: Refactor for iteration when more than one L1-to-L2 routine exists
  
  if (nil$parent_of_L1) {
    
    update_L1(
      package.id.L0 = nil$package.id,
      package.id.L1 = names(nil$parent_of_L1),
      path = config.path, 
      url = config.www, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (nil$parent_of_dwcae) {
    
    update_L2_dwca(
      package.id.L1 = nil$package.id,
      package.id.L2 = increment_package_version(names(nil$parent_of_dwcae)),
      core.name = "event",
      path = config.path,
      url = config.www,
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  }
  
  # Compile log ---------------------------------------------------------------
  
  # log_master.txt - A permanent log of all ecocomDP processes
  # dependencies_update_L1.txt - A list of all dependencies used in update_L1().
  # Consider helper to create these two files
  
  # Message maintainers -------------------------------------------------------
  
  # TODO: Send email to maintainers with a log file specific to the data object conversion (not the entire log_master.txt). This should be combined with the email notification currently sent by event_notification_handler() when an event notification is received.
  # message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
  # Remove from queue ---------------------------------------------------------
  
  # qind
  # r <- httr::DELETE(
  #   paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
  
  # Unlock --------------------------------------------------------------------
  
  # Remove lock file so the next routine can proceed
  
  file.remove(paste0(config.path, "/lock.txt"))
  
  if (file.exists(paste0(config.path, "/lock.txt"))) {
    message("Could not remove lock.txt. Devine intervention is required.")
    return(NULL)
  }
  
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
  
}