#' Load validation criteria
#'
#' @description  
#'     Load criteria against which user supplied ecocomDP tables are validated.
#'
#' @usage load_validation_criteria(data.path)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#'          
#' @details 
#'    This function loads and parses validation criteria from 
#'    \emph{validation_criteria.txt} located in the \emph{\inst} directory of
#'    the \code{ecocomDP} package.
#'         
#' @export
#'

validate_table_names <- function(data.path) {
  
  
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
  
}
