#' Validate table column names
#'
#' @description  
#'     This function ensures that the column names of your ecocomDP (L1) tables 
#'     are valid.
#'
#' @usage validate_column_names(data.path)
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

validate_column_names <- function(data.path, criteria) {
  
  
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
  
  # Validate column names -----------------------------------------------------
  
  message("Checking column names ...")
  
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
                         comment.char = "#")
      colnames_in <- colnames(data)
      index <- match(colnames_in, column_names)
      index_2 <- 1:length(index)
      invalid_column_names <- colnames_in[index_2[is.na(index)]]
      if (sum(is.na(index)) > 0){
        stop(paste("\n",
                   "This table:\n",
                   table_in, "\n",
                   "contains these invalid column names:\n",
                   paste(invalid_column_names, collapse = ", ")))
      }
    }
  }
  

  # Send validation notice ----------------------------------------------------
  
  message('... column names are valid')

}
