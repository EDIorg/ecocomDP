#' Validate table presence
#'
#' @description  
#'     This function checks for missing ecocomDP (L1) tables. Required tables
#'     include location, taxon, observation, dataset_summary.
#'
#' @usage validate_table_presence(data.path, criteria)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#' @param criteria
#'     A data frame of the validation criteria located in 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     A validation report printed in the RStudio console window.
#'          
#' @details 
#' 
#'    The full suite of L1 validation checks are performed by the 
#'    \code{validate_ecocomDP} function. The sequence of checks performed in
#'    \code{validate_ecocomDO} are not random, rather some checks are dependent
#'    upon others. 
#' 
#'         
#' @export
#'

validate_table_presence <- function(data.path, criteria){
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  
  # Validate path
  
  validate_path(data.path)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)
  
  
  # Load validation criteria --------------------------------------------------
  
  table_names_required <- criteria$table[(is.na(criteria$class))
                                         & (criteria$required == "yes")]
  
  table_names_required_regexpr <- paste0("_",
                                         table_names_required,
                                         "\\b")

  
  # Report required tables that are missing -----------------------------------
  
  message("Checking for required tables ...")
  
  missing_tables <- c()
  tables <- dir_files[attr(regexpr(paste(table_names_required_regexpr, 
                                         collapse = "|"), 
                                   dir_files),
                           "match.length")
                      != -1]
  for (i in 1:length(table_names_required_regexpr)){
    table_in <- grep(table_names_required_regexpr[i], tables, value = T)
    if (identical(table_in, character(0))){
      missing_tables[i] <- str_sub(table_names_required_regexpr[i], 2, nchar(table_names_required_regexpr[i])-2)
    }
  }
  if (!identical(missing_tables, NULL)){
    missing_tables <- missing_tables[!is.na(missing_tables)]
    stop(paste("\n",
               "One or more required tables are missing.\n",
               "These are the missing tables:\n",
               paste(missing_tables, collapse = ", ")))
  }
  

  # Send validation notice ----------------------------------------------------
  
  message('... required tables are present')
  
  
}
