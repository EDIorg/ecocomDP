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
  
  # Is anybody waiting?
  prd <- httr::GET("https://regan.edirepository.org/ecocom-listener/package.lternet.edu")
  stg <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu")
  dev <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-d.lternet.edu")
  
  # TODO: First in line gets processed
  # - Set config.environment (global variable)
  
  # Identify workflows to call ------------------------------------------------
  # There can be more than one L1-to-L2 workflows
  
  # TODO: # Is it L0? (A data package with a derivative possessing the "ecocomDP" keyword is an L0.)
  test <- has_child("edi.95.5", "ecocomDP")
  # TODO: Is it L1? If so does it have an L2-DwC-A derivative?
  test <- has_child("edi.96.3", "Darwin Core Archive (DwC-A) Event Core")
  # TODO: Break if empty
  
  # Lock ----------------------------------------------------------------------
  
  # TODO: Create lock file in dir of ecocom-listener
  
  # Call workflow -------------------------------------------------------------
  
  # TODO: When more than one L1-to-L2 workflow exists, the following processes 
  # will have to be implemented with iteration.
  
  if (r$id == "L0") {
    
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
    
  } else if (r$id == "L1_w_dwca_package_descendant") {
    
    # TODO: Complete workflow and implement on server
    # Manual testing:
    update_L2_dwca(
      package.id.L1 = "edi.96.3", # from event notification
      core.name = "event",
      path = "C:\\Users\\Colin\\Documents\\EDI\\data_sets\\event_notification_workflow\\example_L2",
      url = "/some/url",
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  }
  
  # Compile log ---------------------------------------------------------------
  
  # Message maintainers -------------------------------------------------------
  
  message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
  # Remove from queue ---------------------------------------------------------
  
  r <- httr::DELETE(
    paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
  
  # Unlock --------------------------------------------------------------------
  
}