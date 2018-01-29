#' Validate presence of ecocomDP tables
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables include all 
#'     required tables (i.e. \emph{location, taxon, observation, 
#'     dataset_summary}).
#'
#' @usage validate_table_presence(data.path)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#'
#' @return 
#'     A validation report printed in the RStudio console window. The 
#'     validation check runs until an error is encountered. You must address 
#'     errors of one table before the next table is checked. Once all 
#'     tables have been checked you will be notified with a congratulatory 
#'     message.
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

validate_table_presence <- function(data.path) {
  
  
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
  
  message("Loading validation criteria")
  
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

  
  # Report required tables that are missing -----------------------------------
  
  message("Checking for required tables")
  
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
  
  message('Required tables are accounted for')
  
  
}
