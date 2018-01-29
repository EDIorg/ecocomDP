#' Validate column presence
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables contain 
#'     required columns.
#'
#' @usage validate_column_presence(data.path)
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

validate_column_presence <- function(data.path) {
  
  
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
  
  # Validate column presence ----------------------------------
  
  message("Checking for required columns")
  
  for (i in 1:length(table_names_regexpr)){
    table_in <- grep(table_names_regexpr[i], dir_files, value = T)
    if (!identical(table_in, character(0))){
      sep <- detect_delimeter(path = data.path,
                              data.files = table_in,
                              os = os)
      data_in <- read.table(paste0(data.path,
                                   "/",
                                   table_in),
                            header = T,
                            sep = sep,
                            as.is = T,
                            na.strings = "NA")
      use_i <- criteria$table %in% substr(table_names_regexpr[i], 2, nchar(table_names_regexpr[i]) - 2)
      use_i2 <- criteria$required == "yes"
      use_i3 <- (use_i == T) & (use_i2 == T) 
      required_column_names <- criteria$column[use_i3]
      required_column_names <- required_column_names[!is.na(criteria$column[use_i3])]
      found_column_names <- colnames(data_in)
      use_i <- required_column_names %in% found_column_names
      missing_column_names <- required_column_names[!use_i]
      if (sum(use_i) < length(use_i)){
        stop(paste("\n",
                   "This table:\n",
                   table_in, "\n",
                   "is missing these required columns:\n",
                   paste(missing_column_names, collapse = ", ")))
      }
    }
  }
  
  # Send validation notice ----------------------------------------------------
  
  message('Required columns are accounted for')
  
}
