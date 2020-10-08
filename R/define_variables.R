#' Define categorical variables of ecocomDP tables
#'
#' @description  
#'     Get variable definitions and units from parent EML.
#'
#' @param data.path
#'     (character) Path to ecocomDP tables.
#' @param parent.pkg.id
#'     (character) Parent data package ID from the EDI Data Repository (e.g. 
#'     knb-lter-cap.627.3).
#'
#' @return 
#'     (data frame) A data frame with columns:
#'     \itemize{
#'         \item{tableName} Table in which variable is found.
#'         \item{attributeName} Field in which variable is found.
#'         \item{code} Variable name.
#'         \item{definition} Variable definition, if found, blank if otherwise.
#'         \item{unit} Variable unit, if found, blank otherwise.
#'     }
#'
#' @export
#'

define_variables <- function(data.path, parent.pkg.id) {
  message('Retrieving variable definitions and units')
  
  # Define what a variable is
  criteria <- criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  L1_table_names <- criteria$table[is.na(criteria$column)]
  
  # Read ecocomDP tables
  data.list <- ecocomDP::read_from_files(data.path)
  
  # Get values listed in variable_name columns of ecocomDP tables and retrieve
  # definitions for them from the L0 EML metadata.
  cat_vars <- dplyr::select(
    data.list[[1]]$tables$variable_mapping, 
    variable_name, table_name)
  cat_vars$attributeName <- 'variable_name'
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
    'tableName', 'attributeName', 'code', 'definition', 'unit')
  
  # Get definitions and reshape output
  var_metadata <- mapply(
    EDIutils::get_eml_attribute, 
    attr.name = cat_vars$code, 
    MoreArgs = list(package.id = parent.pkg.id))
  var_metadata <- as.data.frame(t(var_metadata), row.names = F)
  
  # Combine into cat_vars object
  cat_vars[ , c('definition', 'unit')] <- var_metadata[ , c('definition', 'unit')]
  use_i <- stringr::str_detect(cat_vars$definition, 'Error in UseMethod')
  use_i[is.na(use_i)] <- FALSE
  cat_vars[use_i , c('definition', 'unit')] <- NA_character_
  
  # Remove place holder added above
  cat_vars <- cat_vars[1:(nrow(cat_vars)-1), ]
  
  # Return
  cat_vars
  
}


