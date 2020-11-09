#' Update list of supported R libraries
#' 
#' @description Identify a global set of R libraries used in all create_ecocomDP.R scripts, write to create_ecocomDP_dependencies.txt and install/upload for use in \code{update_L1()}.
#' 
#' @param repository (character) Data repository in which L1 data packages reside and associated with \code{environment}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which L1 data packages exist. Some repositories have development, staging, and production environments which are distinct from one another.
#'
#' @export
#'
update_supported_libraries <- function(repository = "EDI",
                                       environment = "production") {
  
}