#' Download and source L0 to L1 conversion script
#'
#' @description Follows provenance from the newest L0 to the newest L1 (which, in the context of \code{update_L1()} was derived from the previous L0), downloads the create_ecocomDP.R script, installs listed dependencies, and sources the script to memory for use.
#'
#' @param eml.L0 (xml_document, xml_node) L0 EML metadata returned from \code{read_eml()}.
#' @param path (character) Directory to which create_ecocomDP.R will be written. Should be the same directory tables, and metadata will be written to.
#' 
#' @details Logic follows data package provenance to the newest L1 data package, not older L1 revisions or any other derived data packages. Dependencies are written to dependencies_update_L1.txt, a global listing of dependencies used among L0 to L1 create_ecocomDP.R scripts. Dependencies are installed if not yet available.
#' 
#' @export
#'
download_and_source_conversion_script <- function(eml.L0, path) {
  
}