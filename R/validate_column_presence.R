#' validate_column_presence
#'
#' @description  
#'     Check that required columns of L1 tables are present.
#'
#' @usage validate_column_presence(tables, data.path, criteria)
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
#'     If required column names are absent, then an error is returned.
#'         
#' @export
#'

validate_column_presence <- function(tables, data.path, criteria){
  
  
  # Check arguments and parameterize ------------------------------------------
  
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
  
  # Report missing required columns -------------------------------------------
  
  message('Validating column presence ...')
  
  is_required_column <- function(table.name, data.path, L1.tables){
    
    # Get L1 name and required columns of input table

    output <- lapply(L1.tables, is_table, table.name)
    L1_table_found <- unlist(output)
    L1_table_columns <- criteria$column[(
      !is.na(criteria$column) & (criteria$table == L1_table_found)
      )]
    L1_file_name <- unlist(output)
    cols_required <- criteria[(criteria$required == 'yes') & 
                                (!is.na(criteria$column)) & 
                                (L1_file_name == criteria$table), 
                              'column']
    
    # Get input file column names
    
    os <- detect_os()
    sep <- detect_delimeter(path = data.path,
                            data.files = table.name,
                            os = os)
    data <- read.table(paste0(data.path, "/", table.name),
                       header = T,
                       sep = sep,
                       as.is = T,
                       quote = "\"",
                       comment.char = "")
    cols <- colnames(data)

    # Report missing columns
    
    is_valid_col_name <- function(cols, L1.table_columns, table.name){
      use_i <- match(L1.table_columns, cols)
      missing_columns <- L1.table_columns[is.na(use_i)]
      if (length(missing_columns) > 0){
        stop(paste0("\n",
                    "This table:\n",
                    table.name, "\n",
                    "is missing these columns:\n",
                    paste(missing_columns, collapse = ", ")))
        }
      }
    
    is_valid_col_name(cols, L1.table_columns = cols_required, table.name)
    
  }
  
  use_i <- lapply(tables, is_required_column, data.path = data.path, L1.tables = L1_tables)

  # Send validation notice ----------------------------------------------------
  
  message('... required columns are present')
  
}
