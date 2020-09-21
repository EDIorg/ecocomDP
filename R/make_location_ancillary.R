#' Make the location ancillary table
#'
#' @param x
#'     (data frame) The master data frame composed of all joined tables from the 
#'     parent data package for which the core ecocomDP tables have been, 
#'     created. This master data frame is linked to core tables via primary 
#'     keys and has all ecocomDP column names added to this point. This funciton 
#'     will not work otherwise.
#' @param cols
#'     (character) Names of all columns from \code{x} needed to create 
#'     location_ancillary. This function assumes that all columns with names
#'     corresponding to the ecocomDP location_ancillary table will be treated
#'     as such. If any \code{cols} have a different meaning, i.e. within the 
#'     context of the parent data package, then this function will not work.
#' @param eml
#'     (xml_document xml_node) EML metadata listing units for variables listed 
#'     in \code{cols}. Use \code{EDIutils::api_read_metadata()} or 
#'     \code{xml2::read_xml()} to read the EML file.
#'
#' @return 
#'     (data frame) The location_ancillary table.
#'     
#' @details 
#'     This is possible because at this point we can distinguish between 
#'     standard column names and non-standard column names (variables).
#'     
#'     This function will not work if variables listed in \code{cols} have 
#'     duplicate listing in \code{eml}.
#'     
#' @export
#'
make_location_ancillary <- function(x = NULL, cols = NULL, eml = NULL) {
  
  message('Creating location_ancillary')
  
  # Validate arguments --------------------------------------------------------
  
  if (is.null(x) | is.null(cols)) {
    stop('Both "x" and "cols" are required', call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop('"x" must be a data frame', call. = FALSE)
  }
  if (!is.character(cols)) {
    stop('"cols" must be character', call. = FALSE)
  }
  
  # Parameterize --------------------------------------------------------------
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Make location_ancillary ---------------------------------------------------
  
  # Create data frame of select variables
  d <- dplyr::select(x, cols)
  
  # Rename observation_datetime (a standard variable) if listed
  d <- tryCatch(
    dplyr::rename(d, datetime = observation_datetime),
    error = function(cond) {d})
  
  # Now assume variables not belonging to ecocomDP are variables that need to 
  # be gathered into long format.
  colnames(d)

  # Return
  d
  
}
