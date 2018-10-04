#' validate_datetime
#'
#' @description  
#'     Validate datetime formats of input file. Valid datetime format is
#'     YYYY-MM-DD hh:mm:ss
#'
#' @usage validate_datetime(tables, data.path, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param data.path
#'     (character) Path to the directory containing the L1 tables.
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     Return an error if datetime formats are invalid.
#'          
#' @export
#'

validate_datetime <- function(tables, data.path, criteria){
  
  # Validation parameters -----------------------------------------------------
  
  L1_tables <- criteria$table[
    ((complete.cases(criteria$column)) & 
       ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime')))]
  
  criteria <- criteria[
    ((complete.cases(criteria$column)) & 
       ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime'))), ]
  
  # Does datetime format match the standard ? ---------------------------------
  
  message('Validating datetime format ...')
  
  lapply(tables, is_datetime_format, data.path = data.path, criteria = criteria)
  
  # Send validation notice ----------------------------------------------------
  
  message('... datetime format is correct.')
  
  
}
is_datetime_format <- function(table.name, data.path, criteria) {
  
  # Get L1 name and name of composite key columns
  
  L1_table_found <- unlist(lapply(L1.tables, is_table, table.name))
  L1_table_ck <- criteria$column[criteria$table == L1_table_found]
  
  if (length(L1_table_ck) > 0){
    
    # Read input file
    
    os <- detect_os()
    sep <- detect_delimeter(path = data.path,
                            data.files = table.name,
                            os = os)
    x <- read.table(paste0(data.path, "/", table.name),
                    header = T,
                    sep = sep,
                    as.is = T,
                    quote = "\"",
                    comment.char = "")
    
    # Is datetime format correct? -------------------------------------------
    # Read in vector & remove NA
    # Coerce to datetime standard (YYYY-MM-DD hh:mm:ss)
    # Check for NAs
    
    raw_datetime <- x[ , L1_table_ck]
    raw_datetime <- raw_datetime[!is.na(raw_datetime)]
    con_datetime <- suppressWarnings(lubridate::ymd_hms(raw_datetime))
    if (sum(is.na(con_datetime)) > 0){
      stop(paste0("\n",
                  'This table:\n',
                  table.name,
                  "\ncontains an unsupported datetime formats.",
                  '\nThe format should be "YYYY-MM-DD hh:mm:ss".'),
           call. = F
      )
    }
  }
  
}
