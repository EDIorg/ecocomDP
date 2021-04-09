#' Update L1 from updated L0
#'
#' @description Updates an L1 data package when it’s L0 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param id.L0.newest (character) Identifier of newest L0 data package.
#' @param id.L1.newest (character) Identifier of L0's newest L1 child to be created by this function. The L0-to-L1 conversion script of \code{id.L1.newest} will be used to create the child of \code{id.L0.newest}.
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
update_L1 <- function(id.L0.newest, 
                      id.L1.newest,
                      path, 
                      url, 
                      user.id, 
                      user.pass) {
  
  message("----- Converting L0 (", id.L0.newest, ") to L1 (", 
          increment_package_version(id.L1.newest), ")")
  
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
  
  # Compare EML (L0) ----------------------------------------------------------

  message("----- Comparing EML (Looking for meaningful changes between ",
          "newest and previous versions)")
  
  eml_L0_newest <- ecocomDP::read_eml(id.L0.newest)
  eml_L0_previous <- ecocomDP::read_eml(get_previous_version(id.L0.newest))
  
  browser()
  r <- EDIutils::compare_eml(eml_L0_newest, eml_L0_previous)
  stringr::str_subset(r, "is different")
  
  message(
    capture.output(
      EDIutils::compare_eml(eml_L0_newest, eml_L0_previous, return.all = F)))

  # Compare tables (L0) -------------------------------------------------------

  message("----- Comparing data tables (Looking for meaningful changes ",
          "between newest and previous versions)")
  
  tables_L0_newest <- EDIutils::read_tables(eml_L0_newest)
  tables_L0_previous <- EDIutils::read_tables(eml_L0_previous)

  message(
    capture.output(
      EDIutils::compare_tables(tables_L0_newest, tables_L0_previous)))

  # Download and source conversion script -------------------------------------

  message("----- Downloading and sourcing L0-to-L1 conversion script")
  
  eml_L1_newest <- ecocomDP::read_eml(id.L1.newest)
  download_and_source_conversion_script(eml_L1_newest, path)

  # Create L1 -----------------------------------------------------------------

  message("----- Running L0-to-L1 conversion script")
  
  r <- run_conversion_script(
    path = path,
    id.L0.newest = id.L0.newest,
    id.L1.next = increment_package_version(id.L1.newest),
    url = url)

  # Upload to repository ------------------------------------------------------
  
  message("----- Uploading L1 (", increment_package_version(id.L1.newest), 
          ") to ", repository)
  
  r <- upload_to_repository(
    path = config.path,
    package.id = increment_package_version(id.L1.newest),
    user.id = config.user.id,
    user.pass = config.user.pass)
  
}


















