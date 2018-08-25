#' validate_referential_integrity
#'
#' @description  
#'     Check that foreign keys match primary keys.
#'
#' @usage validate_referential_integrity(tables, data.path, criteria)
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
#'     If foreign keys do not match primary keys, then an error is returned.
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
  
  # outline -------------
  # for each existing table - read table and get columns
  #   for each existing foreign key
  #      if the parent table exists
  #         if the primary key exists
  #            if foreign keys have primary key match NULL, else error w/list of foreign keys w/o match ... match_keys(IN: foreign_key_value_unique, primary_key_value_unique, OUT: NULL, unaccounted_foreign_keys)
  # 
  # data structure:
  #  foreign_key/table/unique_values
  #  primary_key/table/unique_values
  #  
  #  for each foreign_key+table pair do the unique_values match unique primary_key+table pairs? if not, error foreign keys in tableX missing primary keys
  #
  
  # Distill foreign keys + tables + values
  
  x <- unique.data.frame(criteria[(criteria$foreign_key == 'yes') & (!is.na(criteria$foreign_key)), 
                   c('table', 'column')])
  values <- vector('list', length(x$table))
  
  get_values <- function(key, table.name, data.path){
    dir_tables <- list.files(data.path)
    table_pattern <- paste0(table.name, "\\b")
    use_i <- str_detect(dir_tables, table_pattern)
    if (sum(use_i) > 0){
      os <- detect_os()
      sep <- detect_delimeter(path = data.path,
                              data.files = dir_tables[use_i],
                              os = os)
      data_in <- read.table(paste0(data.path, "/", dir_tables[use_i]),
                      header = T,
                      sep = sep,
                      as.is = T,
                      quote = "\"",
                      comment.char = "")
      uni_values <- unique(data_in[ , key])
      uni_values
    } else {
      uni_values <- NULL
      uni_values
    }
    
  }
  values <- mapply(get_values, key = x$column, table.name = x$table, data.path = data.path)
  # ----------------------
  
  
  
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
      }
      
    }
    
  }
  
  use_i <- lapply(tables, is_composite_key, data.path = data.path, L1.tables = L1_tables)
  
  # Send validation notice ----------------------------------------------------
  
  message('... composite keys are unique.')
  
}
