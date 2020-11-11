#' Get previous data package identifier
#'
#' @description A wrapper function to repository specific read methods (the repository arg drives the logic).
#'
#' @param package.id (character) Data package identifier
#' @param repository (character) Data repository in which \code{package.id} resides and associated with \code{environment}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which \code{package.id} exists. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows reading of EML from different environments. Default is "production".
#'
#' @return
#' @export
#'
get_previous_data_package_id <- function(package.id,
                                         repository = "EDI",
                                         environment = "production") {
  
  if (repository == "EDI") {
    
    package_id_components <- stringr::str_split(package.id, "\\.")[[1]]
    
    versions <- EDIutils::api_list_data_package_revisions(
      scope = package_id_components[1],
      identifier = package_id_components[2], 
      environment = environment)
    
    previous <- paste(
      c(package_id_components[1:2],
        L0_versions[length(L0_versions) - 1]),
      collapse = ".")
    
    return(previous)
    
  }
  
}