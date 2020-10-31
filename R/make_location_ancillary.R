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
#'     as such. If any \code{cols} are shared between the parent tables (now
#'     joined in the master data frame) and location_antillary, then there is
#'     ambiguous meaning and this function will not work.
#' @param eml
#'     (xml_document xml_node) EML metadata listing units for variables listed 
#'     in \code{cols}. Use \code{EDIutils::api_read_metadata()} or 
#'     \code{xml2::read_xml()} to read the EML file.
#'
#' @return 
#' A list containing:
#' \item{location_ancillary}{A data frame of the location_ancillary table}
#' \item{x}{The input \code{x} with an added location_ancillary_id column linking unique \code{cols} to the location_ancillary table.}
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
  
  if (is.null(x) | is.null(cols) | is.null(eml)) {
    stop('"x", "cols", and "eml" are required', call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop('"x" must be a data frame', call. = FALSE)
  }
  if (!is.character(cols)) {
    stop('"cols" must be character', call. = FALSE)
  }
  if (!any(class(eml) %in% c("xml_document", "xml_node"))) {
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
  variables_to_gather <- colnames(d)[
    !(colnames(d) %in% criteria$column[
      criteria$table %in% "location_ancillary"])]
  d <- tidyr::gather(
    d, "variable_name", "value", variables_to_gather)
  
  # Add missing location_ancillary columns and fill with NA
  location_ancillary_columns <- criteria$column[
    criteria$table %in% "location_ancillary"]
  missing_columns <- na.omit(
    location_ancillary_columns[!(location_ancillary_columns %in% colnames(d))])
  d[, missing_columns] <- NA_character_

  # Match variables to units and list in the table. If more than one match 
  # occurs (due to duplicate variable names in the EML) then a warning and 
  # suggested course of action is returned.
  attributes <- xml2::xml_find_all(eml, ".//attributeList/attribute")
  attributeNames <- xml2::xml_text(
    xml2::xml_find_all(eml, ".//attributeList/attribute/attributeName"))
  for (col in variables_to_gather) {
    if (all(is.na(d$unit[d$variable_name %in% col]))) {
      # No units imported from the master table so look up in eml
      unit <- xml2::xml_text(
        xml2::xml_find_all(
          attributes[attributeNames %in% col], ".//unit"))
      if (length(unit) == 1) {
        d$unit[d$variable_name %in% col] <- unit
      } else if (length(unit) > 1) {
        # unit may be ambiguous if col is listed more than once
        warning(
          "Variable ", col, " occurs more than once in the EML making a ",
          "one-to-one match with the variables unit ambiguous. Remove ",
          "duplicate variables from the EML when running this function.",
          call. = FALSE)
      }
    }
  }
  
  # Keep only unique rows, create the location_ancillary_id, and reorder 
  # columns to finalize this table
  d <- dplyr::distinct(d)
  d$location_ancillary_id <- paste0('loan_', seq(nrow(d)))
  d <- dplyr::select(
    d, na.omit(criteria$column[criteria$table %in% "location_ancillary"]))

  # Return
  d
  
}
