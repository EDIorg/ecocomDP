#' Event notification handler
#'
#' @description Checks for unprocessed items in SQlite and pops the longest waiting item into \code{update_L1()} or \code{update_L2_dwca()}.
#'
#' @export
#'
routine_handler <- function(package.id, environment, index) {
  
  # Parameterize --------------------------------------------------------------
  
  # Read config.R for user name and password
  source("C:\\Users\\Colin\\Documents\\EDI\\data_sets\\event_notification_workflow\\config.R")
  
  # Get next item in SQLite and run related workflow --------------------------
  
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package.lternet.edu")
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-s.lternet.edu")
  r <- httr::GET("https://regan.edirepository.org/ecocom-listener/package-d.lternet.edu")
  
  # Parse response
  # Is it L0?
  # Is it L1? If so does it have an L2-DwC-A derivative?
  
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
    
    # L1_to_L2_DwCA(
    #   path = config.path, 
    #   core.name = "event", 
    #   parent.package.id = id.L1, 
    #   child.package.id = id.L2, 
    #   data.table.url = config.www, 
    #   user.id = config.user.id,
    #   user.domain = config.repository)
    
    L1_to_L2_DwCA( # For manually getting the first revision in the repo
      path = config.path, 
      core.name = "event", 
      parent.package.id = "edi.96.2", 
      child.package.id = "edi.97.1", 
      data.table.url = config.www, 
      user.id = config.user.id,
      user.domain = config.repository)
    
  }

  
  # Remove from queue ---------------------------------------------------------
  
  r <- httr::DELETE(
    paste0("https://regan.edirepository.org/ecocom-listener/", event_id_index))
  
  # TODO: Write to log
  
  # Message maintainers -------------------------------------------------------
  
  message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
  # TODO: Write to log
  
  
}