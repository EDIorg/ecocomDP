#' Event notification handler
#'
#' @description Runs on a chron. Checks for unprocessed items in SQlite and pops the longest waiting item into \code{update_L1()} or \code{update_L2_dwca()}. While in process
#'
#' @export
#'
routine_handler <- function(package.id, environment, index) {
  
  # Parameterize --------------------------------------------------------------
  
  # Read config.R for user name and password
  source("C:\\Users\\Colin\\Documents\\EDI\\data_sets\\event_notification_workflow\\config.R")
  
  # Get next item in SQLite ---------------------------------------------------
  
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package.lternet.edu")
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu")
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-d.lternet.edu")
  
  # Parse response
  # Is it L0?
  # Is it L1? If so does it have an L2-DwC-A derivative?
  
  # Create lock file ----------------------------------------------------------
  
  # Call appropriate workflow -------------------------------------------------
  
  if (r$id == "L0") {
    
    # update_L1(
    #   package.id.L0 = package.id, # newest L0 id arriving via event notification
    #   path = config.path, 
    #   url = config.www, 
    #   user.id = config.user.id,
    #   user.pass = config.user.pass)
    
    update_L1(
      package.id.L0 = "edi.95.5", # manual testing
      path = config.path, 
      url = config.www, 
      user.id = config.user.id,
      user.pass = config.user.pass)
    
  } else if (r$id == "L1_w_dwca_package_descendant") {
    
    # TODO: update this
    update_L2_dwca(
      package.id.L1,
      repository = "EDI",
      environment = "production",
      core.name,
      path,
      url,
      user.id,
      user.pass)
    
  }
  
  # Upload to repository ------------------------------------------------------
  
  # Remove from queue ---------------------------------------------------------
  
  r <- httr::DELETE(
    paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
  
  # Remove lock file ----------------------------------------------------------
  
  # TODO: Write to log --------------------------------------------------------
  
  # Message maintainers -------------------------------------------------------
  
  message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
  # TODO: Write to log
  
  
}