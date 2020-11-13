#' Download and source L0 to L1 conversion script
#'
#' @param script (character) URL to download the L0 to L1 conversion script.
#' @param path (character) Directory to which create_ecocomDP.R will be written. Should be the same directory tables and metadata will be written to.
#' 
#' @details The script is downloaded \code{path} and then parsed to identify R libraries used by the script which are installed if not already.
#' 
#' @export
#'
download_and_source_conversion_script <- function(script, path) {
  
  r <- httr::GET(
    url = script, 
    httr::write_disk(paste0(path, "/create_ecocomDP.R"), overwrite=TRUE), 
    httr::user_agent("ecocomDP"))
  
  install_missing_libraries(
    conversion.script = paste0(path, "/create_ecocomDP.R"))
  
  suppressMessages(source(paste0(path, "/create_ecocomDP.R")))
  
}