#' Event notification workflow handler
#'
#' @description Runs on a chron. Checks for unprocessed items in SQlite using webservices defined in ADD WEBSERVICE DEFINITIONS HERE and pops the longest waiting item into the appropriate routine. Currently supported are \code{update_L1()} or \code{update_L2_dwca()}.
#' 
#' @param config (character) Full path to config.R which contains global variables for parameterizing this function. Create config.R with \code{create_config()}
#'
#' @export
#'
routine_handler <- function(config) {
  
  # Parameterize --------------------------------------------------------------
  
  source(file = config)
  
  # Check lock ----------------------------------------------------------------
  # Running a routine when one is underway creates issues, so don't even try
  
  if (file.exists(paste0(config.path, "/lock.txt"))) {
    return(NULL)
  }
  
  # Get next in queue ---------------------------------------------------------
  
  niq <- get_from_queue()
  if (is.null(niq)) {
    return(NULL)
  }
  
  # Logging -------------------------------------------------------------------
  # Effective logging changes system args and handles exceptions
  
  # Return warnings where they occur for troubleshooting
  dflt_warn <- getOption("warn")
  on.exit(options(warn = dflt_warn), add = TRUE)
  options(warn = 1)
  
  # Create file
  flog <- paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  log <- file(paste0(config.path, "/", flog), open = "wt")
  sink(log, type = "message")
  on.exit(message("----- Copying ", flog, " to ./logs"), add = TRUE)
  on.exit(
    file.copy(
      from = paste0(config.path, "/", flog),
      to = paste0(dirname(config.path), "/logs/", flog)),
    add = TRUE)
  on.exit(message("----- Exiting routine_handler() at ", Sys.time()), add = TRUE)
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
      msg = "Log file from ecocomDP routine_handler\\(\\) is attached"),
    add = TRUE)
  
  # Header
  message("----- Starting routine_handler() at ", Sys.time())
  message("----- Next in queue is ", niq$id)
  
  # Identify routine(s) to call ----------------------------------------------
  # Knowing which routine to call depends on whether the "next data package
  # in queue" is a parent of an L1 or a parent of one or more L2.
  
  # Because it's possible (but rare) for the next in queue to already have a 
  # child created by one of the routines listed below, first check for such
  # descendants before continuing.
  
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
  
  # FIXME: It's possible that > 1 version ago has an L1 child due to gaps in 
  # processing. This should continue searching previous versions until 
  # exhausted. SOLUTION: This can be fixed by looking for unprocessed "earlier" 
  # versions of niq in the queue, and if found stops and requests for devine
  # intervention. Manual intervention is required because routine_handler()
  # operates on an event notification, not a chron.
  
  # has_unprocessed_revs() TRUE/FALSE
  
  # Does the previous version of the next in queue have a child created by one 
  # of the routines listed below? If not, the next in queue may have wandered 
  # into the wrong queue and should be removed.
  
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
  # Prevents race conditions by indicating a routine in progress
  
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
  
  # Delete from queue ---------------------------------------------------------
  
  message("----- Deleting from queue")
  r <- delete_from_queue(niq$index, niq$id)
  
  # Clear workspace -----------------------------------------------------------
  
  on.exit(file.remove(list.files(config.path, full.names = T)), add = TRUE)
  
  return(NULL)
  
}








#' Get next item in ecocomDP-listener queue
#' 
#' @param filter (character) If "unprocessed" a full list of unprocessed items are returned.
#' 
#' @details The queue is /webapp/ecocomDP.sqlite in \href{https://github.com/EDIorg/ecocomDP-listener}{ecocomDP-listener} and is accessible via HTTP GET.
#'
#' @return
#' \item{index}{(integer) Index of item in queue. Is later used for removing the item from the queue.}
#' \item{id}{(character) Data package identifier}
#' 
get_from_queue <- function(filter = NULL) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  # Repository specific methods -----------------------------------------------
  
  if (repository == "EDI") {
    
    # EDI has listeners in "production" and "staging" environments
    url_prd <- 
      "https://regan.edirepository.org/ecocom-listener/package.lternet.edu"
    url_stg <- 
      "https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu"
    
    # Add filters
    if (!is.null(filter)) {
      url_prd <- paste0(url_prd, "?filter=", filter)
      url_stg <- paste0(url_stg, "?filter=", filter)
    }
    
    # Call
    prd <- httr::GET(url_prd)
    stg <- httr::GET(url_stg)
    
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








#' Delete item from ecocomDP-listener queue
#'
#' @param index (integer) Index of item to remove
#' @param id (character) Data package identifier, corresponding with \code{index}, to remove
#'
#' @return (logical) Indicates whether the item was successfully removed
#' 
delete_from_queue <- function(index, id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
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








#' Create config.R
#'
#' @param path (character) Path to which config.R will be created
#'
#' @return (logical) TRUE if written to \code{path} otherwise FALSE
#' 
#' @export
#'
create_config <- function(path) {
  file.copy(
    from = system.file("config.R", package = "ecocomDP"),
    to = path)
}