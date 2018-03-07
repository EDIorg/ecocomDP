#' Validate column presence
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables contain required 
#'     columns.
#'
#' @usage validate_column_presence(data.path, criteria)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#' @param criteria
#'     A data frame of the validation criteria located in 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     A validation report printed in the console window of RStudio.
#'          
#' @details 
#'    The full suite of L1 validation checks are performed by the 
#'    \code{validate_ecocomDP} function. The sequence of checks performed in
#'    \code{validate_ecocomDO} are not random, rather some checks are dependent
#'    upon others. 
#'         
#' @export
#'

validate_column_presence <- function(data.path, criteria){
  
  
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
  
  column_names <- unique(criteria$column[!is.na(criteria$column)])
  
  table_names <- unique(criteria$table)
  
  table_names_regexpr <- paste0("_",
                                table_names,
                                "\\b")
  
  
  # Validate column presence ----------------------------------
  
  message("Checking for required columns ...")
  
  for (i in 1:length(table_names_regexpr)){
    table_in <- grep(table_names_regexpr[i], dir_files, value = T)
    if (!identical(table_in, character(0))){
      sep <- detect_delimeter(path = data.path,
                              data.files = table_in,
                              os = os)
      data <- read.table(paste0(data.path,
                                "/",
                                table_in),
                         header = T,
                         sep = sep,
                         as.is = T,
                         quote = "\"",
                         comment.char = "")
      use_i <- criteria$table %in% substr(table_names_regexpr[i], 2, nchar(table_names_regexpr[i]) - 2)
      use_i2 <- criteria$required == "yes"
      use_i3 <- (use_i == T) & (use_i2 == T) 
      required_column_names <- criteria$column[use_i3]
      required_column_names <- required_column_names[!is.na(criteria$column[use_i3])]
      found_column_names <- colnames(data)
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
  
  message('... required columns are present')
  
}
