#' Update L2 DwC-A from updated L1
#' 
#' @description Updates an L2 DwC-A data package when it’s L1 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param package.id.L1 (character) Identifier of updated L1
#' @param package.id.L2 (character) Identifier of L1's new DwC-A child to be created by this function
#' @param core.name (character) The Darwin Core central table of the package. Can be: "event" (event core).
#' @param path (character) Directory to which L2 tables, meta.xml, and metadata will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @note \code{user.id} and \code{user.pass} should be a set of master credentials within \code{repository}, otherwise issues at the evaluation/upload step may arise. Requires an L2 already exists in the \code{repository}. A pre-existing L2 must exist because the human has to decide if the L0 data already exists in GBIF or should not be uploaded to GBIF for other reasons.
#' 
#' @details No comparisons of L1 newest and previous are required since they are both in a standardized format.
#'
#' @export
#'
#' @examples
#' 
update_L2_dwca <- function(package.id.L1,
                           package.id.L2,
                           core.name,
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
  
  # Create L2 -----------------------------------------------------------------
  
  L1_to_L2_DwCA(
    path = config.path, 
    core.name = core.name, 
    parent.package.id = package.id.L1, 
    child.package.id = package.id.L2, 
    data.table.url = config.www, 
    user.id = config.user.id,
    user.domain = config.repository)
  
  # Upload to repository ------------------------------------------------------
  
  r <- upload_to_repository(
    path = config.www,
    package.id = package.id.L2,
    user.id = config.user.id,
    user.pass = config.user.pass)
  
  # TODO: Write to log
  
}