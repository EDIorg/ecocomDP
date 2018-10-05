#' validate_composite_keys
#'
#' @description  
#'     Check that composite keys of a table are unique.
#'
#' @usage validate_composite_keys(tables, data.path, criteria)
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
#'     If composite keys are not unique, a warning is returned, not an error.
#'     An error is not returned because some usecases (e.g. replicate 
#'     observations) may result in non-unique composite keys.
#'         
#' @export
#'


validate_composite_keys <- function(tables, data.path, criteria) {
  
  # Check inputs --------------------------------------------------------------
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(data.path)){
    stop('Input argument "data.path" is missing!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Validate path
  
  validate_path(data.path)
  
  # Validation parameters -----------------------------------------------------
  
  L1_tables <- criteria$table[(
    is.na(criteria$class))
    ]
  
  # Are primary keys unique? --------------------------------------------------
  
  message('Validating composite keys ...')
  
  is_composite_key <- function(table.name, data.path, L1.tables) {
    
    # Get L1 name and name of composite key columns
    
    L1_table_found <- unlist(lapply(L1.tables, is_table, table.name))
    L1_table_ck <- criteria$column[(
      (!is.na(criteria$class)) & (criteria$composite_key == 'yes') & (criteria$table == L1_table_found)
    )]
    
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
      
      # Are composite keys unique? ----------------------------------------------
      
      # Create composite key
      
      ck <- apply(x[ , L1_table_ck], 1, paste, collapse = '_')
      
      if (!isTRUE(length(unique(ck)) == nrow(x))){
        
        use_i <- seq(nrow(x))[duplicated(ck)]
        
        warning(paste0("\n",
                    'This table:\n',
                    table.name,
                    "\ncontains non-unique composite keys in rows:\n",
                    paste(use_i, collapse = " "),
                    '\nThis may be due to replicate observations. If so, consider',
                    '\nrevising the variable name to reflect this.'),
             call. = F
        )
      } else {
        message('... composite keys are unique.')
      }
      
    }

  }
  
  use_i <- lapply(tables, is_composite_key, data.path = data.path, L1.tables = L1_tables)
  
  # Send validation notice ----------------------------------------------------
  
}
