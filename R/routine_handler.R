#' Event notification workflow handler
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
#' \item{"config.email.address": Gmail address. Only Gmail is currently supported}
#' \item{"config.email.pass": Password for email address}
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
  
  # Get next in queue
  
  niq <- get_from_queue()
  if (is.null(niq)) {
    return(NULL)
  }
  
  # Logging -------------------------------------------------------------------
  
  # Return warnings where they occur for troubleshooting
  dflt_warn <- getOption("warn")
  on.exit(options(warn = dflt_warn), add = TRUE)
  options(warn = 1)
  
  # Create file
  flog <- paste0("log_", format(Sys.time(), "%Y%m%d%H%M%S"), ".txt")
  log <- file(paste0(config.path, "/", flog), open = "wt")
  sink(log, type = "message")
  on.exit(message("----- Exiting routine_handler() at ", Sys.time()), add = TRUE)
  on.exit(message("----- Copying log"))
  on.exit(
    file.copy(
      from = paste0(config.path, "/", flog),
      to = paste0(dirname(config.path), "/logs/", flog)))
  on.exit(sink(type = "message"), add = TRUE)
  on.exit(close(log), add = TRUE)
  
  # Email
  on.exit(
    send_email(
      from = config.email.address, 
      to = config.email.address, 
      attachment = paste0(config.path, "/", flog), 
      smtp.relay = "smtp.gmail.com",
      relay.user = config.email.address, 
      relay.user.pass = config.email.pass,
      subject = flog, 
      msg = "Log file from ecocomDP routine_handler() is attached"),
    add = TRUE)
  
  # Header
  message("----- Starting routine_handler() at ", Sys.time())
  message("----- Next in queue is ", niq$id)
  
  # Identify routine(s) to call ----------------------------------------------
  
  # Knowing which routine to call depends on whether the "next data package
  # in queue" is a parent of an L1 or a parent of one or more L2.
  
  # Because it's possible (but rare) for the next in queue to already have a 
  # child created by one of the routines listed below, first check for such
  # descendants before continuing. This check safely assumes a previous 
  # version has valid parent attributes, which are tightly controlled by 
  # validate_ecocomDP().
  
  niq$parent_of_L1 <- has_child("ecocomDP", niq$id)
  if (niq$parent_of_L1) {
    message("----- ", niq$id, " already has the L1 child ", 
            names(niq$parent_of_L1))
    r <- delete_from_queue(niq$index, niq$id)
    return(NULL)
  }
  
  if (!niq$parent_of_L1) {
    niq$parent_of_dwcae <- has_child(
      "Darwin Core Archive (DwC-A) Event Core", niq$id)
    if (niq$parent_of_dwcae) {
      message("----- ", niq$id, " already has the L2 DwC-A child ", 
              names(niq$parent_of_dwcae))
      r <- delete_from_queue(niq$index, niq$id)
      return(NULL)
    }
  }
  
  # Was the previous version of the next in queue a parent of a child created 
  # by one of the routines listed below? If not, the next in queue may have 
  # wandered into the wrong queue and should be removed.
  
  niq$parent_of_L1 <- has_child(
    "ecocomDP", get_previous_version(niq$id))
  
  if (!niq$parent_of_L1) {
    niq$parent_of_dwcae <- has_child(
      "Darwin Core Archive (DwC-A) Event Core", 
      get_previous_version(niq$id)) 
  }
  
  if (!any(c(niq$parent_of_L1, niq$parent_of_dwcae))) {
    message("----- No supported routine for ", niq$id)
    r <- delete_from_queue(niq$index, niq$id)
    return(NULL)
  }
  
  # Lock ----------------------------------------------------------------------
  
  # Lock file prevents race conditions by indicating a routine in progress
  
  r <- file.create(paste0(config.path, "/lock.txt"))
  on.exit(file.remove(paste0(config.path, "/lock.txt")), add = TRUE)
  
  if (!file.exists(paste0(config.path, "/lock.txt"))) {
    message("----- Could not create lock.txt")
    return(NULL)
  }
  
  # Call routine -------------------------------------------------------------
  
  # TODO: Refactor for iteration when more than one L1-to-L2 routine exists
  
  if (niq$parent_of_L1) {
    
    message("----- ", niq$id, " is an L0")
    
    update_L1(
      id.L0.newest = niq$id,
      id.L1.newest = names(niq$parent_of_L1),
      path = config.path, 
      url = config.www, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (niq$parent_of_dwcae) {
    
    message("----- ", niq$id, " is an L1")
    
    update_L2_dwca(
      id.L1.newest = niq$id,
      id.L2.next = increment_package_version(names(niq$parent_of_dwcae)),
      core.name = "event",
      path = config.path,
      url = config.www,
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  }
  
  # Clear workspace -----------------------------------------------------------
  
  message("----- Cleaning ", config.path)
  
  r <- file.remove(list.files(config.path, full.names = T))
  
  # Remove from queue ---------------------------------------------------------
  
  # r <- delete_from_queue(niq$index, niq$id)
  
  return(NULL)
  
}








#' Get next item in the ecocom-listener's queue
#'
#' @return
#' \item{index}{(integer) Index of item in queue. Is later used for removing the item from the queue.}
#' \item{id}{(character) Data package identifier}
#' \item{config.environment}{(character) Location of the \code{id} within a repository system. This variable is written to the Global Environment for use in \code{routine_handler()}.}
#'
get_from_queue <- function() {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  # Repository specific methods -----------------------------------------------
  
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
        id = stringr::str_split(cntnt, ",", simplify = T)[2])
      return(res)
    }

  }
  
}








#' Delete item from the ecocom-listener's queue
#'
#' @param index (integer) Index of item to remove
#' @param id (character) Data package identifier, corresponding with \code{index}, to remove
#'
#' @return (logical) Was the item successfully removed from the queue?
#'
delete_from_queue <- function(index, id) {
  
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
    
    # Only the index number is needed to delete an item from the "production" 
    # and "staging" queues (it's the same queue).
    r <- httr::DELETE(
      paste0("https://regan.edirepository.org/ecocom-listener/", index))
    
    if (httr::status_code(r) == 200) {
      message(id, " has been deleted from the queue")
      return(TRUE)
    } else {
      message(id, " could not be deleted from the queue. Devine intervention ",
              "is required.")
      return(FALSE)
    }
    
  }
  
}