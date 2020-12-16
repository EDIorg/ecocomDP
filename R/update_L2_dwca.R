#' Update L2 DwC-A from updated L1
#' 
#' @description Updates an L2 DwC-A data package when it’s L1 parent data package has been updated. This function is a wrapper to several subroutines.
#'
#' @param package.id.L1 (character) Identifier of updated L1
#' @param repository (character) Data repository in which \code{package.id.L1} resides and associated with \code{environment}, \code{user.id}, and \code{user.pass}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which the L0 and L1 exist. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows execution for the \code{update_L1} workflow within the context of one of these environments. Default is "production".
#' @param core.name (character) The Darwin Core central table of the package. Can be: "event" (event core).
#' @param path (character) Directory to which L2 tables, meta.xml, and metadata will be written.
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @note \code{user.id} and \code{user.pass} should be a set of master credentials within \code{repository}, otherwise issues at the evaluation/upload step may arise. Requires an L2 already exists in the \code{repository}. A pre-existing L2 must exist because the human has to decide if the L0 data already exists in GBIF or should not be uploaded to GBIF for other reasons.
#'
#' @export
#'
#' @examples
#' 
update_L2_dwca <- function(package.id.L1,
                           repository = "EDI",
                           environment = "production",
                           core.name,
                           path,
                           url,
                           user.id,
                           user.pass) {
  
  # Read EML (L1) -------------------------------------------------------------
  
  # Read tables (L1) ----------------------------------------------------------
  
  # Read EML (L0) -------------------------------------------------------------
  
  # L1 to L2 ------------------------------------------------------------------
  
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