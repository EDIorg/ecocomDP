#' Run all validation checks on a set of ecocomDP tables
#'
#' @description  
#'     Once you've created an ecocomDP (L1) you will need to check that it 
#'     is accurately formatted. This validation process ensures your L1 is 
#'     correct and can be combined with other L1.
#'
#' @usage validate_ecocomDP(data.path = NULL, data.list = NULL)
#' 
#' @param data.path 
#'     (character) The path to the directory containing L1
#'     tables.
#' @param data.list 
#'     (list of data frames) A named list of data frames containing the L1
#'     tables. Data frame tables must be named after the file name from 
#'     which they were read, including the extension.
#'
#' @return 
#'     A validation report to the RStudio console window. When an issue is 
#'     encountered a message will be displayed with recommendations for fixing
#'     it. Each issue must be resolved before the next check will be implemented.
#'     Once all validation checks have been passed, you will be notified with 
#'     a congratulatory message and you can move on to creating EML metadata with
#'     \code{make_eml}.
#'          
#' @details 
#' 
#'     Run this function after creating a L1 and before making EML for it.
#' 
#'    Validation checks performed by this function:
#'    \itemize{
#'        \item \strong{Table names} Table names must follow the ecocomDP 
#'        naming convention (i.e. \emph{studyName_ecocomDPTableName.ext}, e.g. 
#'        \emph{gleon_chloride_observation.csv}). 
#'        \item \strong{Table presence} Some L1 tables are 
#'        required, others are not.
#'        \item \strong{Column names} Column names must be those specified by
#'        the data pattern.
#'        \item \strong{Column presence} Some columns are 
#'        required, others are not.
#'        \item \strong{Datetime format} One datetime format is valid, namely
#'        'YYYY-MM-DDThh:mm:ss'.
#'        \item \strong{Column classes} Column classes must match the 
#'        ecocomDP specification.
#'        \item \strong{Primary keys} Primary keys must be valid.
#'        \item \strong{Composite keys} Composite keys must be valid.
#'        \item \strong{Referential integrity} Referential integrity must be
#'        valid.
#'    }
#'         
#' @export
#'

validate_ecocomDP <- function(data.path = NULL, data.list = NULL){
  
  
  # Check arguments
  
  if (is.null(data.path) & is.null(data.list)){
    stop('One of the arguments "data.path" or "data.list" must be used.')
  } else if (!is.null(data.path) & !is.null(data.list)){
    stop('Both arguments "data.path" and "data.list" cannot be used.')
  }
  
  # Load validation criteria
  
  message("Loading validation criteria")
  
  criteria <- read.table(
    system.file('validation_criteria.txt', package = 'ecocomDP'),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA")
  
  # Validate locally stored files ...
  
  if (!is.null(data.path)){
    
    # Validate path

    EDIutils::validate_path(data.path)
    
    # Detect operating system
    
    os <- EDIutils::detect_os()
    
    # Misc.
    
    dir_files <- list.files(
      data.path, 
      recursive = F
      )
    
    # Validate table names
    # Inputs should be target(dir.files) and criteria
    
    table_names <- validate_table_names(
      data.path = data.path,
      criteria = criteria
      )
    
    # Report required tables that are missing
    
    validate_table_presence(
      tables = table_names,
      criteria = criteria
      )
    
    # Validate column names
    
    validate_column_names(
      tables = table_names,
      data.path = data.path,
      criteria = criteria
      )
    
    # Validate column presence
    
    validate_column_presence(
      tables = table_names,
      data.path = data.path,
      criteria = criteria
      )
    
    # # Validate datetime format
    # 
    # validate_datetime(
    #   tables = table_names,
    #   data.path = data.path,
    #   criteria = criteria
    #   )
    
    # Validate column classes
    
    # validate_column_classes()
    
    # Validate primary keys
    
    validate_primary_keys(
      tables = table_names, 
      data.path = data.path,
      criteria = criteria
      )
    
    # Validate composite keys
    
    validate_composite_keys(
      tables = table_names, 
      data.path = data.path,
      criteria = criteria
      )
    
    # Validate referential integrity
    
    validate_referential_integrity(
      tables = table_names, 
      data.path = data.path, 
      criteria = criteria
      )
    
    # Validation complete
    
    message(paste('Congratulations! Your ecocomDP tables have passed validation!\n',
                  'Now make metadata for it with the "make_eml" function.'))
    
    
  }
  
  if (!is.null(data.list)){
    
  }
  
}










#' validate_table_names
#' 
#' @description
#'     This function ensures that your ecocomDP (L1) tables follow the
#'     table naming convention (i.e. \emph{studyName_ecocomDPTableName.ext},
#'     e.g. \emph{gleon_chloride_observation.csv}).
#' 
#' @usage validate_table_names(data.path, criteria)
#' 
#' @param data.path
#'     (character) Path to the directory containing the L1 tables.
#'     tables.
#' @param criteria
#'     (data frame) Validation criteria located at
#'     /inst/validation_criteria.txt.
#' 
#' @return
#'     If table names are valid, then the corresponding table names are
#'     returned. If table names are invalid, then an error message is
#'     returned.
#' 
#' @export
#' 
validate_table_names <- function(data.path, criteria) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Validate path
  
  EDIutils::validate_path(data.path)
  
  # Detect operating system
  
  os <- EDIutils::detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)
  
  
  # Load validation criteria --------------------------------------------------
  
  table_names <- unique(criteria$table)
  
  table_names_regexpr <- paste0("_",
                                table_names,
                                "\\b")
  
  # Validate file naming convention -------------------------------------------
  
  message("Checking table names ...")
  
  msg <- list()
  tables <- dir_files[attr(regexpr(paste(table_names_regexpr, 
                                         collapse = "|"), 
                                   dir_files),
                           "match.length")
                      != -1]
  study_names <- unique(gsub(paste(table_names_regexpr, 
                                   collapse = "|"), 
                             "", tables))
  study_names <- str_replace(study_names, "\\.[:alnum:]*$", replacement = "")
  if (length(study_names) > 1){
    stop(paste("\n",
               "More than one study name found in your ecocomDP tables.\n",
               "Only one study name is allowed.\n",
               "Here are the unique study names found:\n",
               paste(study_names, collapse = ", ")),
         call. = F)
  } else {
    message('... table names are valid')
    tables
  }
  
}










#' validate_table_presence
#'
#' @description  
#'     Check that required L1 tables are present.
#'
#' @usage validate_table_presence(tables, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If required tables are missing, then an error message is produced.
#'          
#' @details 
#'         
#' @export
#'

validate_table_presence <- function(tables, criteria){
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Which table are required? -------------------------------------------------
  
  required_tables <- criteria$table[(
    is.na(criteria$class)) & (criteria$required == "yes")
    ]
  
  # Report required tables that are missing -----------------------------------
  
  message("Checking for required tables ...")
  
  
  is_required_table <- function(required_table, tables){
    required_table_pattern <- paste0(required_table, "\\b")
    if (sum(str_detect(tables, required_table_pattern)) == 1){
      required_table
    }
  }
  
  required_tables_found <- unlist(lapply(required_tables, is_required_table, tables = tables))
  use_i <- is.na(match(required_tables, required_tables_found))
  required_tables_missing <- required_tables[use_i]
  
  if (length(required_tables_missing) != 0){
    stop(paste("\n",
               "One or more required tables are missing.\n",
               "These are the missing tables:\n",
               paste(required_tables_missing, collapse = ", ")))
  }
  
  # Send validation notice ----------------------------------------------------
  
  message('... required tables are present')
  
}










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
  
  EDIutils::validate_path(data.path)
  
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
    
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
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
  
  EDIutils::validate_path(data.path)
  
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
    
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
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










validate_datetime <- function(tables, data.path, criteria){
  
  # Validation parameters -----------------------------------------------------
  
  L1_tables <- criteria$table[
    ((complete.cases(criteria$column)) & 
       ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime')))]
  
  criteria <- criteria[
    ((complete.cases(criteria$column)) & 
       ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime'))), ]
  
  # Does datetime format match the standard ? ---------------------------------
  
  message('Validating datetime format ...')
  
  lapply(X = L1_tables, FUN = is_datetime_format, tables = tables, data.path = data.path, criteria = criteria)
  
  # Send validation notice ----------------------------------------------------
  
  message('... datetime format is correct.')
  
  
}

is_datetime_format <- function(L1.table, tables, data.path, criteria) {
  
  # Get L1 name and name of composite key columns
  
  L1_table_found <- is_table(L1.table = L1.table, table.name = tables)
  L1_table_ck <- criteria$column[criteria$table == L1_table_found]
  
  if (length(L1_table_ck) > 0){
    
    table.name <- tables[str_detect(tables, paste0(L1_table_found, '\\b'))]
    
    # Read input file
    
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
                            data.files = table.name,
                            os = os)
    x <- read.table(paste0(data.path, "/", table.name),
                    header = T,
                    sep = sep,
                    as.is = T,
                    quote = "\"",
                    comment.char = "")
    
    # Is datetime format correct? -------------------------------------------
    # Read in vector & remove NA
    # Coerce to datetime standard (YYYY-MM-DD hh:mm:ss)
    # Check for NAs
    
    raw_datetime <- x[ , L1_table_ck]
    raw_datetime <- raw_datetime[!is.na(raw_datetime)]
    con_datetime <- suppressWarnings(lubridate::ymd_hms(raw_datetime))
    if (sum(is.na(con_datetime)) > 0){
      stop(paste0("\n",
                  'This table:\n',
                  table.name,
                  "\ncontains an unsupported datetime formats.",
                  '\nThe format should be "YYYY-MM-DD hh:mm:ss".'),
           call. = F
      )
    }
  }
  
}










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
  
  EDIutils::validate_path(data.path)
  
  # Detect operating system
  
  os <- EDIutils::detect_os()
  
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
      sep <- EDIutils::detect_delimeter(path = data.path,
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


validate_primary_keys <- function(tables, data.path, criteria) {
  
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
  
  EDIutils::validate_path(data.path)
  
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
    
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
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
  
  EDIutils::validate_path(data.path)
  
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
      
      os <- EDIutils::detect_os()
      sep <- EDIutils::detect_delimeter(path = data.path,
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


validate_referential_integrity <- function(tables, data.path, criteria) {
  
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
  
  EDIutils::validate_path(data.path)
  
  # Message
  
  message('Checking referential integrity ...')
  
  # Get foreign key values ----------------------------------------------------
  
  x <- unique.data.frame(criteria[(criteria$foreign_key == 'yes') & (!is.na(criteria$foreign_key)), c('table', 'column')])
  
  foreign_key_tables <- x$table
  
  foreign_key_values <- mapply(get_foreign_keys, key = x$column, table.name = x$table, data.path = data.path)
  
  # Get primary key values ----------------------------------------------------
  
  x <- unique.data.frame(criteria[(criteria$primary_key == 'yes') & (!is.na(criteria$primary_key)), 
                                  c('table', 'column')])
  
  primary_key_tables <- x$table
  
  primary_key_values <- mapply(get_primary_keys, key = x$column, table.name = x$table, data.path = data.path)
  
  # Foreign keys should have a matching primary key ---------------------------
  
  is_foreign_key(fk.tables = foreign_key_tables, 
                 fk.values = foreign_key_values, 
                 pk.values = primary_key_values
  )
  
  # Send validation notice ----------------------------------------------------
  
  message('... tables have referential integrity.')
  
}




# get_foreign_keys - Get a list of foreign key values
# key = (character) key name
# table.name = (character) full table name with file extension
# data.path = (character) path to table.name
get_foreign_keys <- function(key, table.name, data.path){
  dir_tables <- list.files(data.path)
  table_pattern <- paste0(table.name, "\\b")
  use_i <- str_detect(dir_tables, table_pattern)
  if (sum(use_i) > 0){
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
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




# get_primary_keys - Get a list of primary key values
# key = (character) key name
# table.name = (character) full table name with file extension
# data.path = (character) path to table.name
get_primary_keys <- function(key, table.name, data.path){
  dir_tables <- list.files(data.path)
  table_pattern <- paste0(table.name, "\\b")
  use_i <- str_detect(dir_tables, table_pattern)
  if (sum(use_i) > 0){
    os <- EDIutils::detect_os()
    sep <- EDIutils::detect_delimeter(path = data.path,
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




# is_foreign_key - Check that foreign key has matching primary key
# fk.tables = (character) names of tables containing foreign keys
# fk.values = (list) values of foreign keys
# pk.values = (list) values of primary keys
is_foreign_key <- function(fk.tables, fk.values, pk.values){
  
  fk.names <- names(fk.values)
  pk.names <- names(pk.values)
  
  # Error if foreign key names don't have a primary match (missing).
  
  use_i <- match(fk.names[fk.names != 'parent_location_id'], pk.names)
  if (sum(is.na(use_i)) > 0){
    stop(paste0("\n",
                'These foreign keys:\n',
                paste(fk.names(is.na(use_i)), collapse = ', '),
                '\nare missing primary keys'
    )
    )
  }
  
  # Validate parent_location_id
  
  if (sum(fk.names == 'parent_location_id') > 0){
    fk_val <- fk.values[['parent_location_id']]
    fk_val <- fk_val[!is.na(fk_val)] # Remove NA (NAs are allowed)
    pk_val <- pk.values[['location_id']]
    use_i <- is.na(match(fk_val, pk_val))
    if (sum(use_i) > 0){
      stop(paste0('\nThe foreign key "parent_location_id"',
                  '\ncontains values that cannot be found in the primary key',
                  '\n"location_id" of the location table, specifically\n',
                  paste(fk_val[use_i], collapse = ', ')
      )
      )
    }
    fk.tables <- fk.tables[fk.tables != 'location']
    fk.values <- fk.values[names(fk.values) != 'parent_location_id']
    fk.names <- fk.names[fk.names != 'parent_location_id']
  }
  
  # Validate event_id
  
  if (sum(fk.names == 'event_id') > 0){
    fk_val <- fk.values[['event_id']]
    fk_val <- fk_val[!is.na(fk_val)] # Remove NA (NAs are allowed)
    pk_val <- pk.values[['event_id']]
    use_i <- is.na(match(fk_val, pk_val))
    if (sum(use_i) > 0){
      stop(paste0('\nThe foreign key "event_id"',
                  '\ncontains values that cannot be found in the primary key',
                  '\n"event_id" of the observation_ancillary table, specifically\n',
                  paste(fk_val[use_i], collapse = ', ')
      )
      )
    }
    use_i <- fk.names == 'event_id'
    fk.tables <- fk.tables[!use_i]
    fk.values <- fk.values[names(fk.values) != 'event_id']
    fk.names <- fk.names[!use_i]
  }
  
  # Validate other foreign keys
  
  other_foreign_keys <- function(fk.table, fk.name, fk.value, pk.value){
    use_i <- match(fk.value, pk.value)
    if (sum(is.na(use_i)) > 0){
      stop(paste0('\nThe foreign key:\n',
                  fk.name,
                  '\nin this table:\n',
                  fk.table,
                  '\ncontains values that do not have a primary key match, specifically:\n',
                  paste(fk.value[is.na(use_i)], collapse = ', ')
      )
      )
    }
  }
  mapply(other_foreign_keys, 
         fk.table = fk.tables,
         fk.name = fk.names,
         fk.value = fk.values,
         pk.value = pk.values[fk.names]
  )
  
}
