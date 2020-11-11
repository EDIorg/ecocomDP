#' Upload data package to a repository
#'
#' @description A wrapper function to repository specific upload methods.
#'
#' @param path (character) Directory in which L1 tables, scripts, and metadata exist.
#' @param package.id (character) Identifier of data package to be uploaded
#' @param repository (character) Data repository to which \code{package.id} will be uploaded and associated with \code{environment}, \code{user.id}, and \code{user.pass}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param environment (character) Repository environment to which the data package will be uploaded. Some repositories have development, staging, and production environments which are distinct from one another. This argument enables upload to one of these environments. Default is "production".
#' @param user.id (character) User identifier within a specified \code{repository}. This controls editing access in some \code{repository}.
#' @param user.pass (character) Password associated with \code{user.id} for repository upload.
#'
#' @return (character) Evaluation/upload summary
#' @export
#'
#' @examples
#' 
upload_to_repository <- function(path,
                                 package.id,
                                 repository = "EDI",
                                 environment = "production", 
                                 user.id,
                                 user.pass) {
  
  if (repository == "EDI") {
    
    eml <- EDIutils::api_update_data_package(
      path = path, 
      package.id = package.id, 
      environment = environment, 
      user.id = user.id, 
      user.pass = user.pass,
      affiliation = repository)
    
  }
  
}