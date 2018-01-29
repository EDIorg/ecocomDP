#' Validate column classes
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables contain columns of 
#'     the correct class (i.e. character, numeric, etc.).
#'
#' @usage validate_column_classes(data.path)
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
#' @export
#'

validate_column_classes <- function(data.path) {
  
  
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
  
  # Validate column classes ---------------------------------------------------
  
  message("Checking column classes")
  
  for (i in 1:length(table_names_regexpr)){
    table_in <- grep(table_names_regexpr[i], dir_files, value = T)
    if (!identical(table_in, character(0))){
      sep <- detect_delimeter(path = data.path,
                              data.files = table_in,
                              os = os)
      data_in <- read.table(paste(data.path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = sep,
                            as.is = T,
                            na.strings = "NA")
      found_column_names <- colnames(data_in)
      found_column_classes <- unname(unlist(lapply(data_in, class)))
      found_numeric_classes <- (found_column_classes == "numeric") | (found_column_classes == "integer")
      found_column_classes[found_numeric_classes] <- "numeric"
      found_column_criteria <- data.frame(colname = found_column_names,
                                          colclass = found_column_classes,
                                          stringsAsFactors = F)
      expected_column_names <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
      expected_column_classes <- criteria$class[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
      expected_numeric_classes <- (expected_column_classes == "numeric") | (expected_column_classes == "integer")
      expected_column_criteria <- data.frame(colname = expected_column_names,
                                             colclass = expected_column_classes,
                                             stringsAsFactors = F)
      use_i <- expected_column_criteria$colclass == "Date"
      if (sum(use_i) > 0){
        expected_column_criteria$colclass[use_i] <- "character"
      }
      use_i <- expected_column_criteria$colclass == "character"
      if (sum(use_i > 0)){
        found_column_criteria$colclass[use_i] <- "character"
      }
      use_i <- match(found_column_criteria$colname,
                     expected_column_criteria$colname)
      use_i <- found_column_criteria$colclass == expected_column_criteria$colclass[use_i]
      if (sum(!use_i) > 0){
        stop(paste("\n",
                   "This table:\n",
                   table_in, "\n",
                   "contains these columns:\n",
                   paste(found_column_criteria$colname[!use_i], collapse  = ", "), "\n",
                   "with these incorrect column classes:\n",
                   paste(found_column_criteria$colclass[!use_i], collapse = ", "), "\n",
                   "The correct classes for these columns are:\n",
                   paste(expected_column_criteria$colclass[!use_i], collapse = ", ")))                                                    
      }
    }
  }
  
  
  # Send validation notice ----------------------------------------------------
  
  message('Column classes are correct')
  
  
}
