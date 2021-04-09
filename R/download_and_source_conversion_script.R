#' Download and source L0 to L1 conversion script
#'
#' @param eml (xml_document xml_node) EML of newest L1 child. The L0-to-L1 conversion script listed in \code{eml} will be used to create the child of the newest L0.
#' @param path (character) Directory to which create_ecocomDP.R will be written. Should be the same directory tables and metadata will be written to.
#' 
#' @details The script is downloaded \code{path} and then parsed to identify R libraries used by the script which are installed if not already.
#' 
#' @export
#'
download_and_source_conversion_script <- function(eml, path) {
  
  other_entities <- xml2::xml_text(
    xml2::xml_find_all(eml, ".//otherEntity/physical/objectName"))
  
  script <- stringr::str_detect(
    tolower(other_entities), "^create_ecocomdp.r$")
  
  urls <- xml2::xml_text(
    xml2::xml_find_all(eml, ".//otherEntity/physical/distribution/online/url"))
  
  r <- httr::GET(
    url = urls[script], 
    httr::write_disk(paste0(path, "/create_ecocomDP.R"), overwrite = TRUE), 
    httr::user_agent("ecocomDP"))
  
  install_missing_libraries(
    conversion.script = paste0(path, "/create_ecocomDP.R"))
  
  suppressMessages(source(paste0(path, "/create_ecocomDP.R")))
  
}