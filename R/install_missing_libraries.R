#' Install missing libraries
#' 
#' @description Identify if a create_ecocomDP.R script uses uninstalled libraries and install them for use in \code{update_L1()}.
#' 
#' @param conversion.script (character) Full path to create_ecocomDP.R
#' 
#' @details Libraries used by create_ecocomDP.R are identified by reading the full file to memory and using regular expressions to extract the name of the library from within \code{library()} (i.e. \code{"(?<=library\\().*(?=\\))"}). All libraries found in this way are installed.
#'
#' @export
#'
install_missing_libraries <- function(conversion.script) {
  
  lines <-  readLines(conversion.script)
  
  libraries <- unlist(
    stringr::str_extract_all(lines, "(?<=library\\().*(?=\\))"))
  
  installed_libraries <- row.names(installed.packages())
  
  if (!(all(libraries %in% installed_libraries))) {
    missing_libraries <- libraries[which(!(libraries %in% installed_libraries))]
    install.packages(missing_libraries)
  }

}