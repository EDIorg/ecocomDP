#' validate_primary_keys
#'
#' @description  
#'     Checks that primary keys of a table are unique and issues an error if
#'     otherwise.
#'
#' @usage validate_primary_keys(tables, data.path, criteria)
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
#'     If primary keys are not unique, then an error is returned.
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
  
  message('Validating primary keys ...')
  
  prep_primary_key <- function(table.name, data.path, L1.tables) {
    
    # Get L1 name and name of primary key
    
    L1_table_found <- unlist(lapply(L1.tables, is_table, table.name))
    L1_table_pk <- criteria$column[(
      (!is.na(criteria$class)) & (criteria$primary_key == 'yes') & (criteria$table == L1_table_found)
    )]
    
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
    
    # Are primary keys unique? --------------------------------------------------
    
    is_primary_key(x, pk = L1_table_pk, table.name = table.name)
    
  }
  
  use_i <- lapply(tables, prep_primary_key, data.path = data.path, L1.tables = L1_tables)
  
  # Send validation notice ----------------------------------------------------
  
  message('... primary keys are unique.')
  
}

is_primary_key <- function(x, pk, table.name){
  if (!isTRUE(length(unique(x[ , pk])) == nrow(x))){
    
    use_i <- seq(nrow(x))[duplicated(x[ , pk])]
    
    stop(paste0("\n",
                'This table:\n',
                table.name,
                "\ncontains non-unique primary keys in rows:\n",
                paste(use_i, collapse = " ")),
         call. = F
    )
  }
}