#' Run L0 to L1 conversion script
#' 
#' @description Repository specific methods for handling script arguments is enabled by this function.
#'
#' @param path (character) Directory to which L1 tables, scripts, and metadata will be written.
#' @param package.id.L0 (character) Identifier of newest L0 data package.
#' @param package.id.L1 (character) Identifier of new L1
#' @param repository (character) Data repository in which \code{package.id.L0} resides and associated with \code{environment}, \code{user.id}, and \code{user.pass}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which the L0 and L1 exist. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows execution for the \code{update_L1} workflow within the context of one of these environments. Default is "production".
#' @param url (character) Publicly accessible URL to \code{path} for download by a data repository.
#'
#' @return
#' @export
#'
run_conversion_script <- function(path, 
                                  package.id.L0, 
                                  package.id.L1, 
                                  repository, 
                                  environment, 
                                  url) {
  
  if (repository == "EDI") {
    
    r <- create_ecocomDP(
      path = path,
      package.id.L0 = package.id.L0, 
      package.id.L1 = package.id.L1, 
      environment = environment,
      url = url)
    
  }
    
}