#' Write ecocomDP tables to file
#'
#' @param path
#'     (character) Directory to which the files will be written.
#' @param sep
#'     (character) Field delimiter used in the ecocomDP tables. Valid options 
#'     are "," or "\\t".
#' @param study.name
#'     (character) Name to be prefixed to each ecocomDP table name (e.g. 
#'     'bird_survey_').
#' @param observation
#'     (data frame) observation table
#' @param location
#'     (data frame) location table
#' @param taxon
#'     (data frame) taxon table
#' @param dataset_summary
#'     (data frame) dataset_summary table
#' @param observation_ancillary
#'     (data frame) observation_ancillary table
#' @param location_ancillary
#'     (data frame) location_ancillary table
#' @param taxon_ancillary
#'     (data frame) taxon_ancillary table
#' @param variable_mapping
#'     (data frame) variable_mapping table
#'
#' @return 
#'     ecocomDP tables written to file.
#'     
#' @export
#'
write_tables <- function(
  path, sep, study.name = NULL, observation = NULL, location = NULL, 
  taxon = NULL, dataset_summary = NULL, observation_ancillary = NULL,
  location_ancillary = NULL, taxon_ancillary = NULL, variable_mapping = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is required.', call. = FALSE)
  }
  
  if (missing(sep)){
    stop('Input argument "sep" is required.', call. = FALSE)
  } else if (!missing(sep)){
    if ((!stringr::str_detect(sep, ',')) & (!stringr::str_detect(sep, '\\t'))) {
      stop('Input argument "sep" must be "," or "\\t".', call. = FALSE)
    }
  }
  
  # Write tables to file ------------------------------------------------------
  
  message('Writing tables to file:')
  
  if (sep == ",") {
    suffix <- ".csv"
  } else if (sep == "\\t") {
    suffix <- ".txt"
  }
  
  if (!is.null(observation)) {
    message("  observation")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("observation", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = observation, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(location)) {
    message("  location")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("location", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = location, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(taxon)) {
    message("  taxon")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("taxon", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = taxon, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(dataset_summary)) {
    message("  dataset_summary")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("dataset_summary", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = dataset_summary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(observation_ancillary)) {
    message("  observation_ancillary")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("observation_ancillary", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = observation_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(location_ancillary)) {
    message("  location_ancillary")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("location_ancillary", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = location_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(taxon_ancillary)) {
    message("  taxon_ancillary")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("taxon_ancillary", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = taxon_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(variable_mapping)) {
    message("  variable_mapping")
    f <- paste0(
      path, 
      "/", 
      paste0(
        c(study.name, paste0("variable_mapping", suffix)), 
        collapse = "_"))
    data.table::fwrite(x = variable_mapping, file = f, sep = sep, na = "NA")
  }

}
