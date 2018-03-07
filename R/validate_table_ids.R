#' Validate table IDs
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables contain valid 
#'     relational IDs (keys).
#'
#' @usage validate_table_ids(data.path)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#'
#' @return 
#'     A validation report printed in the console window of RStudio. The 
#'     validation checks run until an error is encountered. You must address 
#'     errors before continuing to the next check. Once all validation checks 
#'     have successfully completed, you will be notified with a congratulatory 
#'     message.
#'          
#' @details 
#'    The full suite of L1 validation checks are performed by the 
#'    \code{validate_ecocomDP} function. The sequence of checks performed in
#'    \code{validate_ecocomDO} are not random, rather some checks are dependent
#'    upon others. 
#'         
#' @export
#'

validate_table_ids <- function(data.path) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }

  # Validate path
  
  validate_path(data.path)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)
  
  
  # Load validation criteria --------------------------------------------------
  
  message("Loading validation criteria ")
  
  criteria <- read.table(paste(path.package("ecocomDP"),
                               "/validation_criteria.txt",
                               sep = ""),
                         header = T,
                         sep = "\t",
                         as.is = T,
                         na.strings = "NA")
  
  column_names <- unique(criteria$column[!is.na(criteria$column)])
  
  table_names <- unique(criteria$table)
  
  table_names_regexpr <- paste0("_",
                                table_names,
                                "\\b")
  
  table_names_required <- criteria$table[(is.na(criteria$class))
                                         & (criteria$required == "yes")]
  
  table_names_required_regexpr <- paste0("_",
                                         table_names_required,
                                         "\\b")
  
  
  # Validate inter-table IDs -------------------------------------------
  #
  # What is included in observation table must be found in the other tables,
  # then within each table should be a unique set of IDs with no duplication.
  #
  # Iterate through each table linked to observation and perform checks.
  # If observation value doesn't have key in linked table, then report missing 
  # value and line number within observation.
  # If duplicate IDs within a table, report duplicate values, IDs, and lines.
  
  message("Checking table IDs")
  
  tables <- dir_files[attr(regexpr(paste(table_names_regexpr, 
                                         collapse = "|"), 
                                   dir_files),
                           "match.length")
                      != -1]
  study_names <- unique(gsub(paste(table_names_regexpr, 
                                   collapse = "|"), 
                             "", tables))
  study_names <- str_replace(study_names, "\\.[:alnum:]*$", replacement = "")
  if (length(study_names) > 1){
    stop(paste("\n",
               "More than one study name found in your ecocomDP tables.\n",
               "Only one study name is allowed.\n",
               "Here are the unique study names found:\n",
               paste(study_names, collapse = ", ")),
         call. = F)
  }
  
  
  # Validate IDs within location table -------------------------------------------
  

  # Validation complete -------------------------------------------------------
  
  message('Table IDs are valid')
  
  
}
