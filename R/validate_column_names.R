#' validate_column_names
#'
#' @description  
#'     Check that column names of L1 tables are valid.
#'
#' @usage validate_column_names(data.path, criteria)
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
#'     If required column names are missing, then an error is returned.
#'         
#' @export
#'

validate_column_names <- function(tables, data.path, criteria) {
  
  
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
  
  validate_path(data.path)
  
  # Get validation criteria ---------------------------------------------------
  
  message('Validating column names')
  
  L1_tables <- criteria$table[(
    is.na(criteria$class))
    ]
  
  # Detect invalid column names -----------------------------------------------
  
  is_column_name <- function(table.name, data.path, L1_tables){
    
    # Get L1 table name

    output <- lapply(L1_tables, is_table, table.name)
    L1_table_found <- unlist(output)
    L1_table_columns <- criteria$column[(
      !is.na(criteria$column) & (criteria$table == L1_table_found)
      )]
    
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

    # Which column names are invalid?
    
    is_valid_name(cols, L1_table_columns, table.name)
    
  }
  
  use_i <- lapply(tables, is_column_name, data.path = data.path, L1_tables = L1_tables)

  # Send validation notice ----------------------------------------------------
  
  message('... column names are valid')

}

# Retrieves the name of the L1 table from the input file name

is_table <- function(L1.table, table.name){
  required_table_pattern <- paste0(L1.table, "\\b")
  if (sum(str_detect(table.name, required_table_pattern)) == 1){
    L1.table
  }
}

is_valid_name <- function(cols, L1.table_columns, table.name){
  use_i <- match(cols, L1.table_columns)
  invalid_columns <- cols[is.na(use_i)]
  if (length(invalid_columns) > 0){
    stop(paste0("\n",
                "This table:\n",
                table.name, "\n",
                "contains these invalid column names:\n",
                paste(invalid_columns, collapse = ", ")))
  }
}
