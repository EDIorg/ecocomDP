#' Update L1 from updated L0
#'
#' @description Updates an L1 data package when it’s L0 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param package.id.L0 (character) Identifier of newest L0 data package.
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
                      path, 
                      url, 
                      user.id, 
                      user.pass) {
  
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
  
  # Read EML (L0) -------------------------------------------------------------
  
  # Newest
  
  eml_L0_newest <- ecocomDP::read_eml(package.id.L0)
  
  # TODO: Write to log
  
  # Find the previous containing a valid L0 to L1 conversion script
  
  r <- ecocomDP::get_previous_L0_id_and_L1_conversion_script(package.id.L0)
  
  # TODO: Write to log
  
  if (is.null(r)) {
    # TODO: Write "no script found" to log
    stop("No L0 to L1 conversion script found", call. = FALSE)
  } else {
    L0_previous <- r$id.L0
    L0_previous_script_url <- r$url
    L1_new <- stringr::str_split(r$id.L1, "\\.")[[1]]
    L1_new[3] <- as.character(as.numeric(L1_new[3]) + 1)
    L1_new <- paste(L1_new, collapse = ".")
  }
  
  # TODO: Write to log
  
  if (L0_previous == package.id.L0) {
    stop(
      "The L0 data package ", package.id.L0, " has an L1 descendant.", 
      call. = FALSE)
  }
  
  # TODO: Write to log
  
  eml_L0_previous <- ecocomDP::read_eml(L0_previous)
  
  # TODO: Write to log
  
  # Compare EML (L0) ----------------------------------------------------------
  
  r <- EDIutils::compare_eml(eml_L0_newest, eml_L0_previous)
  
  # TODO: Write to log
  # - Issue warning if differences are found and list all differences. 
  # Otherwise write "no differences found".
  
  # Read tables (L0) ----------------------------------------------------------
  
  tables_L0_newest <- EDIutils::read_tables(eml_L0_newest)
  tables_L0_previous <- EDIutils::read_tables(eml_L0_previous)
  
  # Compare tables (L0 newest to L0 previous) ---------------------------------
    
  r <- EDIutils::compare_tables(tables_L0_newest, tables_L0_previous)
  
  # TODO: Write to log
  # - Issue warning if differences are found and list all differences. 
  # Otherwise write "no differences found".
  
  # Download and source conversion script -------------------------------------
  
  ecocomDP::download_and_source_conversion_script(L0_previous_script_url, path)
  
  # TODO: Write to log
  
  # Run conversion script -----------------------------------------------------
  
  # Repository specific methods
  r <- run_conversion_script(
    path = path,
    package.id.L0 = package.id.L0, 
    package.id.L1 = L1_new,
    url = url)
  
  # TODO: Write to log
  
  # Upload to repository ------------------------------------------------------
  
  # TODO: Authenticate and use token instead of password

  r <- ecocomDP::upload_to_repository(
    path = path,
    package.id = L1_new,
    user.id = user.id, 
    user.pass = user.pass)
  
  # TODO: Write to log
  
}


















