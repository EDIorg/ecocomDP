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
    
    update_L1(
      package.id.L0 = package.id, # newest L0 id arriving via event notification
      path = path, 
      repository = repository,
      environment = environment,
      url = "someurl", 
      user.id = user.id,
      user.pass = user.pass)
    
  } else if (r$id == "L1_w_dwca_package_descendant") {
    
    # update_L2_dwca(
    #   package.id.L1 = package.id,
    #   repository = repository,
    #   environment = environment,
    #   core.name = "event",
    #   path = path,
    #   url = url,
    #   user.id = user.id,
    #   user.pass = user.pass)
    
    L1_to_L2_DwCA( # For manually getting the first revision in the repo
      path = path, 
      core.name = "event", 
      parent.package.id = "edi.96.2", 
      child.package.id = "edi.97.1", 
      environment = "staging", 
      data.table.url = url, 
      user.id = user.id,
      user.domain = repository)
    
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