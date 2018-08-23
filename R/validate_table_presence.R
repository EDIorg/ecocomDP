#' validate_table_presence
#'
#' @description  
#'     Check that required L1 tables are present.
#'
#' @usage validate_table_presence(tables, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If required tables are missing, then an error message is produced.
#'          
#' @details 
#'         
#' @export
#'

validate_table_presence <- function(tables, criteria){
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Which table are required? -------------------------------------------------
  
  required_tables <- criteria$table[(
    is.na(criteria$class)) & (criteria$required == "yes")
    ]

  # Report required tables that are missing -----------------------------------
  
  message("Checking for required tables ...")
  
  
  is_required_table <- function(required_table, tables){
    required_table_pattern <- paste0(required_table, "\\b")
    if (sum(str_detect(tables, required_table_pattern)) == 1){
      required_table
    }
  }
  
  required_tables_found <- unlist(lapply(required_tables, is_required_table, tables = tables))
  use_i <- is.na(match(required_tables, required_tables_found))
  required_tables_missing <- required_tables[use_i]
  
  if (length(required_tables_missing) != 0){
    stop(paste("\n",
               "One or more required tables are missing.\n",
               "These are the missing tables:\n",
               paste(required_tables_missing, collapse = ", ")))
  }

  # Send validation notice ----------------------------------------------------
  
  message('... required tables are present')
  
}
