#' Define ecocomDP variables
#'
#' @description  
#'     Get variable definitions and units from the parent data package EML 
#'     metadata.
#'
#' @usage define_variables(data.path, parent.pkg.id)
#'
#' @param data.path
#'     (character) Path to ecocomDP tables.
#' @param parent.pkg.id
#'     (character) ID of parent data package from which the ecocomDP tables
#'     were created. The data package must be stored in the Environmental Data 
#'     Initiative repository. Example data package ID: knb-lter-cap.627.3
#'
#' @return 
#'     (data frame) A data frame with columns:
#'     \itemize{
#'         \item{tableName} Name of ecocomDP table containing the variable.
#'         \item{attributeName} Name of the column in ecocomDP tables 
#'         containing the variable.
#'         \item{code} Name of the variable listed in the ecocomDP tables.
#'         \item{definition} Definition of the varible, if found in the parent
#'         metadata.
#'         \item{unit} Unit of the variable, if found in the parent metadata.  
#'     }
#'     
#' @details 
#'     Extract unique variables listed in the "variable_name" column fromm
#'     all ecocomDP tables, then look up the variable names in the parent
#'     data package EML record and output in a data frame for user supply to 
#'     `make_eml`. When a variable name can't be found in the parent package
#'     EML, the definition is assigned a value of NA.
#'     
#' @note 
#'     Variables that can't be defined by the parent package EML should be 
#'     manually defined by editing this object after it is created.
#'
#' @export
#'

define_variables <- function(data.path, parent.pkg.id){
  
  # Get variables from tables at data.path ------------------------------------
  
  message('Getting variables from ecocomDP tables')
  
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
    location_ancillary = data.list$location_ancillary$data
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
  
  # Add place holder so get_eml_attribute doesn't break
  
  cat_vars[nrow(cat_vars)+1, ] <- NA_character_
  
  
  # Get variable definitions and units from metadata --------------------------
  
  var_metadata <- mapply(
    EDIutils::get_eml_attribute, 
    attr.name = cat_vars$code, 
    MoreArgs = list(package.id = parent.pkg.id)
    )
  
  var_metadata <- as.data.frame(t(var_metadata), row.names = F)
  
  # Combine into cat_vars object
  
  cat_vars[ , c('definition', 'unit')] <- var_metadata[ , c('definition', 'unit')]
  
  use_i <- stringr::str_detect(cat_vars$definition, 'Error in UseMethod')
  use_i[is.na(use_i)] <- FALSE
  cat_vars[use_i , c('definition', 'unit')] <- NA_character_
  
  # Remove place holder added above
  
  cat_vars <- cat_vars[1:(nrow(cat_vars)-1), ]
  
  
  # Return --------------------------------------------------------------------
  
  cat_vars
  
}


