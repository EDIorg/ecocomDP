#' Annotations of published data
#'
#' @description  
#'     View the collection of dataset- and attribute-level annotations
#'     from existing ecocomDP datasets.
#'     
#' @details
#'     Use the search field to find the annotation terms and URIs.
#'     
#' @note Access to EDI repository endpoints requires authentication. Set the environment variable \code{EDI_API_KEY} (e.g., via \code{Sys.setenv(EDI_API_KEY = "your_key")} or in your \code{.Renviron} file).
#' 
#' This function may not work between 01:00 - 03:00 UTC on Wednesdays due to regular maintenance of the EDI Data Repository.
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
    newrev <- suppressMessages(EDIutils::list_data_package_revisions("edi", 1000, filter = "newest"))
    objurls <- suppressMessages(EDIutils::read_data_package(paste0("edi.1000.", newrev)))
    objurls <- stringr::str_subset(objurls, "/data/")
    objids <- stringr::str_extract(objurls, "(?<=/)[:alnum:]+$")
    objnames <- suppressMessages(
      lapply(objids, function(id) {
        EDIutils::read_data_entity_name(packageId = paste0("edi.1000.", newrev), entityId = id)
      })
    )
    objnames <- unlist(objnames)
    isdata <- !stringr::str_detect(objnames, "Function")
    objurls <- objurls[isdata]
    for (objurl in objurls) {
      con <- url(add_api_key(objurl))
      load(con)
      close(con)
    }
    ecocomDP_annotation_dictionary <- annotation_dictionary_table
    save(ecocomDP_annotation_dictionary, 
         file = paste0(tempdir(), "/annotation_dictionary_table.rda"), 
         version = 3)
  }
  return(ecocomDP_annotation_dictionary)
} 
