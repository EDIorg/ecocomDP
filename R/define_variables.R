#' Define ecocomDP variables
#'
#' @description  
#'     This function identifies unique variables listed in the ecocomDP tables
#'     and writes them to tables for you to provide definitions and units for.
#'
#' @usage define_variables(data.path = "", sep = "")
#'
#' @param data.path 
#'     A character string specifying a path to the dataset working directory 
#'     containing ecocomDP tables.
#' @param sep
#'     The field separator string. Values within each row of ecocomDP tables
#'     are separated by this string. Valid options are "," or "\\t".
#'
#' @return 
#'     A tab delimited UTF-8 file in the dataset working directory titled 
#'     \emph{ecocomDPtablename_variables.txt} with fields:
#'     \itemize{
#'         \item \strong{attributeName} Name of the variable field from the
#'         an ecocomDP table.
#'         \item \strong{code} Unique variable names detected in an ecocomDP 
#'         table.
#'         \item \strong{definition} A field for users to supply definitions 
#'         for variables listed in the code field.
#'         \item \strong{units} A field for users to supply the units for each 
#'         defined variable.
#'     }
#'     
#' @details 
#'     Run this function after you have created and validated ecocomDP tables but
#'     prior to create EML with \code{make_eml}. 
#' 
#'     Variables of ecocomDP tables are stored in long format. Use 
#'     \code{define_variables} to identify unique variables and assign 
#'     definitions and associated units.
#'     
#'     Refer to the standard unit dictionary when selecting units. Open the 
#'     standard unit dictionary by calling \code{view_unit_dictionary}. Search 
#'     for a unit and enter the value listed in the \emph{name} field of the 
#'     dictionary into the \emph{units} field of 
#'     \emph{ecocomDPtablename_variables.txt}. NOTE these values are case 
#'     sensitive. If you cannot find the unit you are looking for, enter the 
#'     unit in the tab delimited UTF-8 formatted file \emph{custom_units.txt} 
#'     that is imported into your dataset working directory when calling 
#'     \code{import_templates}. Valid custom units must be convertible to SI 
#'     Units (i.e. International System of Units). If it cannot be converted to
#'     SI then list it in the units field as "dimensionless". To create a 
#'     custom unit define the:
#'     \itemize{
#'         \item \strong{id} This is equivalent to the unit name. Reference 
#'         the standard unit dictionary formatting
#'         \item \strong{unitType} The type of unit being defined.
#'         Reference the dictionary for examples.
#'         \item \strong{parentSI} The SI equivalent of the id you have entered.
#'         \item \strong{multiplierToSI} This is the multiplier to convert 
#'         from your custom unit to the SI unit equivalent.
#'         \item \strong{description} A description of the custom unit. 
#'         Reference the dictionary for examples.
#'         }
#'
#' @seealso \code{\link{import_templates}} to import ecocomDP templates.
#' @seealso \code{\link{make_eml}} to make EML for an ecocomDP.
#'
#' @export
#'


define_variables <- function(data.path, data.list = NULL, parent.pkg.id){
  
  criteria <- read.table(
    system.file('validation_criteria.txt', package = 'ecocomDP'),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA")
  
  L1_table_names <- criteria$table[is.na(criteria$column)]
  
  file_names <- suppressMessages(
    validate_table_names(
      data.path = data.path,
      criteria = criteria
    )
  )
  
  data.list <- lapply(file_names, read_ecocomDP_table, data.path = data.path)
  
  names(data.list) <- unlist(lapply(file_names, is_table_rev, L1_table_names))
  
  cat_vars <- ecocomDP::make_variable_mapping(
    observation = data.list$observation$data,
    observation_ancillary = data.list$observation_ancillary$data,
    taxon_ancillary = data.list$taxon_ancillary$data,
    location_ancillary = data.list$location_ancillary
  )[ , c('variable_name', 'table_name')]
  
  cat_vars$attributeName <- rep('variable_name', nrow(cat_vars))
  cat_vars$definition <- NA_character_
  cat_vars$unit <- NA_character_
  
  cat_vars <- dplyr::select(
    cat_vars,
    table_name,
    attributeName,
    variable_name,
    definition,
    unit)
  
  colnames(cat_vars) <- c(
    'tableName',
    'attributeName',
    'code', 
    'definition', 
    'unit'
  )
  
  # Parse variable metadata into cat_vars object
  
  for (i in 1:nrow(cat_vars)){
    
    var_metadata <- get_var_metadata(
      var.name = cat_vars$code[i],
      pkg.id = parent.pkg.id
    )
    
    cat_vars[i, c('definition', 'unit')] <- unlist(var_metadata)[c('definition', 'unit')]
    
  }
  
  # Get definitions from L0 metadata
  # Inputs:
  # var.name = (character) variable name
  # pkg.id = (character) EDI data package ID (e.g. knb-lter-cap.627.7)
  # Outputs:
  # Definition, unit
  get_var_metadata <- function(var.name, pkg.id){
    scope <- unlist(stringr::str_split(pkg.id, '\\.'))[1]
    identifier <- unlist(stringr::str_split(pkg.id, '\\.'))[2]
    revision <- unlist(stringr::str_split(pkg.id, '\\.'))[3]
    metadata <- xmlParse(paste("http://pasta.lternet.edu/package/metadata/eml",
                               "/",
                               scope,
                               "/",
                               identifier,
                               "/",
                               revision,
                               sep = ""))
    entity_names <- unlist(
      xmlApply(metadata["//dataset/dataTable/entityName"],
               xmlValue)
    )
    
    for (i in 1:length(entity_names)){
      definition <- unlist(
        try(xmlApply(
          metadata[
            paste0("//dataTable[./entityName = '",
                   entity_names[i],
                   "']//attribute[./attributeName = '",
                   var.name,
                   "']//attributeDefinition")],
          xmlValue
        ), silent = TRUE)
      )
      unit <- unlist(
        try(xmlApply(
          metadata[
            paste0("//dataTable[./entityName = '",
                   entity_names[i],
                   "']//attribute[./attributeName = '",
                   var.name,
                   "']//standardUnit")],
          xmlValue
        ), silent = TRUE)
      )
      if (!is.character(definition)){
        definition <- NA_character_
      }
      if (!is.character(unit)){
        unit <- NA_character_
      }
    }
    
    list(
      name = var.name,
      definition = definition,
      unit = unit
    )
    
  }
  
}