#' Annotations of published data
#'
#' @description  
#'     View the collection of dataset- and attribute-level annotations
#'     from existing ecocomDP datasets.
#'     
#' @details
#'     Use the search field to find the annotation terms and URIs.
#'     
#' @export
#' 
#' @examples 
#' \dontrun{
#' View(annotation_dictionary())
#' }
#'     
annotation_dictionary <- function(){
  
  ping_edi() # Warn if EDI is down
  
  # Download this object once per session and save to tempdir() for future calls
  if ("annotation_dictionary_table.rda" %in% dir(tempdir())) {
    load(paste0(tempdir(), "/annotation_dictionary_table.rda"))
  } else {
    newrev <- suppressMessages(api_list_data_package_revisions("edi", "1000", filter = "newest"))
    objurls <- suppressMessages(api_read_data_package(paste0("edi.1000.", newrev)))
    objurls <- stringr::str_subset(objurls, "/data/")
    objids <- stringr::str_extract(objurls, "(?<=/)[:alnum:]+$")
    objnames <- suppressMessages(
      lapply(objids, api_read_data_entity_name, package.id = paste0("edi.1000.", newrev)))
    objnames <- unlist(objnames)
    isdata <- !stringr::str_detect(objnames, "Function")
    objurls <- objurls[isdata]
    for (objurl in objurls) {
      load(url(objurl))
    }
    ecocomDP_annotation_dictionary <- annotation_dictionary_table
    save(ecocomDP_annotation_dictionary, 
         file = paste0(tempdir(), "/annotation_dictionary_table.rda"), 
         version = 3)
  }
  return(ecocomDP_annotation_dictionary)
} 
