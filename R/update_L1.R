#' Update L1 from updated L0
#'
#' @description Updates an L1 data package when it’s L0 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param package.id.L0 (character) Identifier of newest L0 data package.
#' @param repository (character) Data repository in which \code{package.id.L0} resides and associated with \code{environment}, \code{user.id}, and \code{user.pass}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which the L0 and L1 exist. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows execution for the \code{update_L1} workflow within the context of one of these environments. Default is "production".
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @details The updated L1 data package published in the data repository system specified by \code{repository} and in the sub-system specified by \code{environment}. Messaging from this routine is written to log_master.txt. Dependencies specified in conversion scripts are added to dependencies_update_L1.txt and installed in servers R distribution via \code{update_supported_libraries()}.
#' 
#' \code{user.id}, \code{repository}, and \code{user.pass} form a set of credentials allowing edits to all data packages within a repository system, otherwise evaluate/upload will fail if the creator is someone else.
#' 
#' Requires an L0 and L1 already exist in the repository, because the L1 creation and upload process may have errors that need resolution by a human.
#' 
#' @export
#'
update_L1 <- function(package.id.L0, 
                      repository = "EDI",
                      environment = "production",
                      path, 
                      url, 
                      user.id, 
                      user.pass) {
  
  # Read EML (L0) -------------------------------------------------------------
  
  # Newest
  
  eml_L0_newest <- ecocomDP::read_eml(
    package.id = package.id.L0, 
    repository = repository, 
    environment = environment)
  
  # Find the previous containing a valid L0 to L1 conversion script
  
  r <- ecocomDP::get_previous_L0_id_and_L1_conversion_script(
    package.id = package.id.L0, 
    repository = repository, 
    environment = environment)
  
  if (is.null(r)) {
    # TODO: Write "no script found" to log
    stop("No L0 to L1 conversion script found", call. = FALSE)
  } else {
    L0_previous <- r$id
    L0_previous_script_url <- r$script
  }
  
  eml_L0_previous <- ecocomDP::read_eml(
    package.id = L0_previous,
    repository = repository, 
    environment = environment)
  
  # Compare EML (L0) ----------------------------------------------------------
  
  r <- EDIutils::compare_eml(
    newest = eml_L0_newest,
    previous = eml_L0_previous)
  
  # TODO: Write to log
  
  # Read tables (L0) ----------------------------------------------------------
  
  tables_L0_newest <- EDIutils::read_tables(eml = eml_L0_newest)
  tables_L0_previous <- EDIutils::read_tables(eml = eml_L0_previous)
  
  # Compare tables (L0 newest to L0 previous) ---------------------------------
  
  r <- EDIutils::compare_tables(
    newest = tables_L0_newest,
    previous = tables_L0_previous)
  
  # TODO: Write to log
  
  # Download and source conversion script -------------------------------------
  
  ecocomDP::download_and_source_conversion_script(
    script = L0_previous_script_url,
    path = path)
  
  # Run conversion script -----------------------------------------------------
  
  # r <- create_ecocomDP()
  
  browser()
  
  # Upload to repository ------------------------------------------------------
  
  # TODO: Authenticate and use token instead of password
  
  r <- ecocomDP::upload_to_repository(
    path = path,
    package.id = L1.package.id, # Need to derive this 
    repository = repository, 
    environment = environment, 
    user.id = user.id, 
    user.pass = somepassword)
  
  # Update dirty bit ----------------------------------------------------------
  
  r <- update_dirty_bit()
  
  # Message maintainers -------------------------------------------------------
  
  message_maintainers()
  
  # Clear workspace -----------------------------------------------------------
  
  r <- clear_workspace(path = path)
  
}


















