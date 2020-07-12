#' Validate a dataset against the ecocomDP
#'
#' @description  
#'     Use this function to verify a dataset conforms to the ecocomDP.
#' 
#' @param data.path 
#'     (character) The path to the directory containing ecocomDP tables.
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'     
#' @note
#'     This function is used by ecocomDP creators (to ensure what has been 
#'     created is valid), maintainers (to improve the quality of archived
#'     ecocomDP datasets), and users (to ensure the data being used is free of
#'     model error).
#'          
#' @details 
#'    Validation checks:
#'    \itemize{
#'        \item{File names - When using \code{data.path} the data are read from
#'        files and must follow an expected pattern, namely 
#'        \emph{studyName_ecocomDPTableName.ext} (e.g. 
#'        \emph{gleon_chloride_observation.csv}).}
#'        \item{Table presence - Some tables are required, others are not.}
#'        \item{Column names - Column names must follow specification.}
#'        \item{Column presence - Some columns are required, others are not.}
#'        \item{Column classes - Column classes must follow specification.}
#'        \item{Datetime format - Date and datetime formats must follow 
#'        specification.}
#'        \item{Primary keys - Primary keys of tables must be valid.}
#'        \item{Composite keys - Composite keys of tables must be valid.}
#'        \item{Referential integrity - Referential integrity among tables must 
#'        be valid.}
#'    }
#'    
#' @examples 
#' # Validate a set files (example data)
#' validate_ecocomDP(data.path = system.file("/data", package = "ecocomDP"))
#'         
#' @export
#'

validate_ecocomDP <- function(
  data.path = NULL, 
  data.list = NULL) {
  
  # Check arguments -----------------------------------------------------------
  
  if (is.null(data.path) & is.null(data.list)) {
    stop('One of the arguments "data.path", or "d" must be used.')
  }
  if (!is.null(data.path)) {
    EDIutils::validate_path(data.path)
  }
  
  # Parameterize --------------------------------------------------------------
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Read data -----------------------------------------------------------------
  
  if (!is.null(data.path)) {
    d <- read_from_files(data.path)
    invisible(validate_table_names(data.path = data.path))
  } else {
    d <- data.list
  }

  # Validate data -------------------------------------------------------------
  
  message("Validating:")
  
  validate_table_presence(d)
  validate_column_names(d)
  validate_column_presence(d)
  # FIXME: Uncomment the following line
  # validate_datetime(d)
  
  # FIXME: Implement validate_column_classes()
  validate_column_classes(d)
  
  # Validate primary keys
  
  validate_primary_keys(
    tables = file_names, 
    d = d,
    criteria = criteria
  )
  
  # Validate composite keys
  
  validate_composite_keys(
    tables = file_names, 
    d = d,
    criteria = criteria
  )
  
  # Validate referential integrity
  
  validate_referential_integrity(
    tables = file_names, 
    d = d, 
    criteria = criteria
  )
  
  # Validation complete
  
  message('Congratulations! Your ecocomDP tables have passed validation!\n')

}










#' Check for file name errors
#' 
#' @description
#'     This function ensures that your ecocomDP (L1) tables follow the
#'     file naming convention (i.e. \emph{studyName_ecocomDPTableName.ext},
#'     e.g. \emph{gleon_chloride_observation.csv}).
#' 
#' @param data.path
#'     (character) The path to the directory containing ecocomDP tables.
#' 
#' @return
#'     If table names are valid, then the corresponding table names are
#'     returned. If table names are invalid, then an error message is
#'     returned.
#'     
#' @export
#'
validate_table_names <- function(data.path = NULL) {
  
  message("File names")
  
  # Validate inputs
  
  EDIutils::validate_path(data.path)
  
  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  table_names <- unique(criteria$table)
  table_names_regexpr <- paste0("_", table_names, "\\b")
  
  msg <- list()
  tables <- list.files(data.path)[attr(regexpr(paste(table_names_regexpr, 
                                         collapse = "|"), 
                                   list.files(data.path)),
                           "match.length")
                      != -1]
  study_names <- unique(gsub(paste(table_names_regexpr, 
                                   collapse = "|"), 
                             "", tables))
  study_names <- stringr::str_replace(study_names, "\\.[:alnum:]*$", replacement = "")
  if (length(study_names) > 1){
    stop(paste("\n",
               "More than one study name found in your ecocomDP tables.\n",
               "Only one study name is allowed.\n",
               "Here are the unique study names found:\n",
               paste(study_names, collapse = ", ")),
         call. = F)
  } else {
    tables
  }

}










#' Check for required tables
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If required tables are missing, then an error message is produced.
#'         
validate_table_presence <- function(data.list) {
  
  message("Required tables")
  
  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  expected <- criteria$table[
    (is.na(criteria$class)) & (criteria$required == "yes")]
  required_missing <- expected[!(expected %in% names(data.list))]
  if (any(required_missing)) {
    stop("These tables are missing tables: ", 
         paste(required_missing, collapse = ", "),
         call. = FALSE)
  }
  
}










#' Check for invalid column names
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If required column names are missing, then an error is returned.
#'
validate_column_names <- function(data.list) {

  message('Column names')
  
  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      expected <- criteria$column[
        (criteria$table %in% x) & !is.na(criteria$column)]
      invalid_columns <- !(colnames(data.list[[x]]) %in% expected)
      if (any(invalid_columns)) {
        stop(
          "The ", x, " table has these invalid column names: ",
          paste(colnames(data.list[[x]])[invalid_columns], collapse = ", "),
          call. = FALSE)
      }
    })
  
}













#' Check for required columns
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If required column names are absent, then an error is returned.
#'         
#' @export
#'
validate_column_presence <- function(data.list){
  
  message('Required columns')

  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      expected <- criteria$column[
        (criteria$table %in% x) & 
          !is.na(criteria$column) & 
          (criteria$required == "yes")]
      missing_columns <- !(expected %in% colnames(data.list[[x]]))
      if (any(missing_columns)) {
        stop(
          "The ", x, " table is missing these required columns: ",
          paste(colnames(data.list[[x]])[missing_columns], collapse = ", "),
          call. = FALSE)
      }
    })

}








#' Check for valid date and time formats
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If date and time data are not formmated to specification, then an error 
#'     is returned.
#'         
#' @export
#'
validate_datetime <- function(data.list) {
  
  message('Datetime formats')

  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      datetime_column <- criteria$column[
        (criteria$table %in% x) &
          (criteria$class == "Date") &
          !is.na(criteria$column)]
      if (length(datetime_column) > 0) {
        # Difference in NA count induced by coercion indicates a non-valid 
        # format
        v <- data.list[[x]][[datetime_column]]
        na_count_raw <- sum(is.na(v))
        use_i <- suppressWarnings(
          list(
            lubridate::parse_date_time(v, "ymdHMS"),
            lubridate::parse_date_time(v, "ymdHM"),
            lubridate::parse_date_time(v, "ymdH"),
            lubridate::parse_date_time(v, "ymd")))
        na_count_parsed <- unlist(
          lapply(
            use_i,
            function(k) {
              sum(is.na(k))
            }))
        if (min(na_count_parsed) > na_count_raw) {
          use_i <- seq(
            length(v))[
              is.na(
                use_i[[
                  which(na_count_parsed %in% min(na_count_parsed))]])]
          stop(
            "The ", x, "table has unsupported datetime formats at rows: ", 
            paste(use_i, collapse = ' '),
            call. = F)
        }
      }
    })

}












#' Check that ecocomDP table column classes are valid
#'
#' @description
#'     This function ensures that your ecocomDP (L1) tables contain columns of
#'     the correct class (i.e. character, numeric, etc.).
#'
#' @usage validate_column_classes(data.path)
#'
#' @param d
#'     (list of data frames) A list of of data frames. Use this argument if
#'     tables are read into the R environment. The list should be of the following 
#'     structure:
#'     \itemize{
#'         \item \strong{"list name"} Name of the list object. This can be anything.
#'         \itemize{
#'             \item \strong{"L1 table name"} i.e. observation, location, taxon, etc.
#'             \itemize{
#'                 \item \strong{"data"} Data frame representation of the L1 table
#'                 \item \strong{"fname"} File name the data frame was created from
#'             }
#'         }
#'     }
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
  
  browser()
  # Check arguments and parameterize

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


  # Load validation criteria

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

  # Validate column classes

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


  # Send validation notice

  message('Column classes are correct')


}










#' Check that primary keys of ecocomDP tables are valid
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
#' @param d
#'     (list of data frames) A list of of data frames. Use this argument if
#'     tables are read into the R environment. The list should be of the following 
#'     structure:
#'     \itemize{
#'         \item \strong{"list name"} Name of the list object. This can be anything.
#'         \itemize{
#'             \item \strong{"L1 table name"} i.e. observation, location, taxon, etc.
#'             \itemize{
#'                 \item \strong{"data"} Data frame representation of the L1 table
#'                 \item \strong{"fname"} File name the data frame was created from
#'             }
#'         }
#'     }
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If primary keys are not unique, then an error is returned.
#'         
#' @export
#'


validate_primary_keys <- function(tables, d, criteria) {
  
  # Check inputs
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(d)){
    stop('Input argument "d" is missing!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Validation parameters
  
  L1_tables <- criteria$table[(
    is.na(criteria$class))
    ]
  
  # Are primary keys unique?
  
  message('Validating primary keys ...')
  
  prep_primary_key <- function(table.name, d, L1.tables) {
    
    # Get L1 name and name of primary key
    
    L1_table_found <- unlist(lapply(L1.tables, is_table, table.name))
    L1_table_pk <- criteria$column[(
      (!is.na(criteria$class)) & (criteria$primary_key == 'yes') & (criteria$table == L1_table_found)
    )]
    
    # Read input file
    
    x <- d[[L1_table_found]][['data']]
    
    # Are primary keys unique?
    
    is_primary_key(x, pk = L1_table_pk, table.name = table.name)
    
    
  }
  
  use_i <- lapply(tables, prep_primary_key, d = d, L1.tables = L1_tables)
  
  # Send validation notice
  
  message('... primary keys are unique\n')
  
}











#' Check that composite keys of ecocomDP tables are valid
#'
#' @description  
#'     Check that composite keys of a table are unique.
#'
#' @usage validate_composite_keys(tables, data.path, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param d
#'     (list of data frames) A list of of data frames. Use this argument if
#'     tables are read into the R environment. The list should be of the following 
#'     structure:
#'     \itemize{
#'         \item \strong{"list name"} Name of the list object. This can be anything.
#'         \itemize{
#'             \item \strong{"L1 table name"} i.e. observation, location, taxon, etc.
#'             \itemize{
#'                 \item \strong{"data"} Data frame representation of the L1 table
#'                 \item \strong{"fname"} File name the data frame was created from
#'             }
#'         }
#'     }
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


validate_composite_keys <- function(tables, d, criteria) {
  
  # Check inputs
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(d)){
    stop('Input argument "d" is missing!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Validation parameters
  
  L1_tables <- criteria$table[(
    is.na(criteria$class))
    ]
  
  # Are primary keys unique?
  
  message('Validating composite keys ...')
  
  is_composite_key <- function(table.name, d, L1.tables) {
    
    # Get L1 name and name of composite key columns
    
    L1_table_found <- unlist(lapply(L1.tables, is_table, table.name))
    L1_table_ck <- criteria$column[(
      (!is.na(criteria$class)) & (criteria$composite_key == 'yes') & (criteria$table == L1_table_found)
    )]
    
    if (length(L1_table_ck) > 0){
      
      # Read input file
      
      x <- d[[L1_table_found]][['data']]
      
      # Are composite keys unique?
      
      # Create composite key
      
      ck <- apply(x[ , L1_table_ck], 1, paste, collapse = '_')
      
      if (!isTRUE(length(unique(ck)) == nrow(x))){
        
        use_i <- seq(nrow(x))[duplicated(ck)]
        
        warning(paste0("\n",
                       'This table:\n',
                       table.name,
                       "\ncontains non-unique composite keys.",
                       '\nThis may be due to replicate observations. If so, consider',
                       '\nrevising the variable name to reflect this.'),
                call. = F
        )
      }
      
    }
    
  }
  
  use_i <- lapply(tables, is_composite_key, d = d, L1.tables = L1_tables)
  
  # Send validation notice
  
  message('... composite keys are unique\n')
  
}










#' Check that referential integrity of ecocomDP tables is valid
#'
#' @description  
#'     Check that foreign keys match primary keys.
#'
#' @usage validate_referential_integrity(tables, data.path, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param d
#'     (list of data frames) A list of of data frames. Use this argument if
#'     tables are read into the R environment. The list should be of the following 
#'     structure:
#'     \itemize{
#'         \item \strong{"list name"} Name of the list object. This can be anything.
#'         \itemize{
#'             \item \strong{"L1 table name"} i.e. observation, location, taxon, etc.
#'             \itemize{
#'                 \item \strong{"data"} Data frame representation of the L1 table
#'                 \item \strong{"fname"} File name the data frame was created from
#'             }
#'         }
#'     }
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If foreign keys do not match primary keys, then an error is returned.
#'         
#' @export
#'


validate_referential_integrity <- function(tables, d, criteria) {
  
  # Check inputs
  
  if (missing(tables)){
    stop('Input argument "tables" is missing!')
  }
  if (!is.character(tables)){
    stop('Input argument "tables" is not of class = character!')
  }
  if (missing(d)){
    stop('Input argument "d" is missing!')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Message
  
  message('Checking referential integrity ...')
  
  # Get foreign key values
  
  x <- unique.data.frame(criteria[(criteria$foreign_key == 'yes') & (!is.na(criteria$foreign_key)), c('table', 'column')])
  
  foreign_key_tables <- x$table

  # get_foreign_keys - Get a list of foreign key values
  # key = (character) key name
  # table.name = (character) full table name with file extension
  # data.path = (character) path to table.name
  get_foreign_keys <- function(key, table.name){
    dir_tables <- unlist(
      lapply(
        seq(length(d)),
        fnames_from_list,
        i = 'fname',
        lis = d)
    )
    table_pattern <- paste0(table.name, "\\b")
    use_i <- stringr::str_detect(dir_tables, table_pattern)
    if (sum(use_i) > 0){
      data_in <- d[[table.name]][['data']]
      if (key %in% colnames(data_in)){
        uni_values <- unique(data_in[ , key])
      } else {
        uni_values <- NULL
      }
      uni_values
    } else {
      uni_values <- NULL
      uni_values
    }
  }

  foreign_key_values <- mapply(get_foreign_keys, key = x$column, table.name = x$table)
  
  # is.null(foreign_key_values)
  
  # Get primary key values
  
  x <- unique.data.frame(criteria[(criteria$primary_key == 'yes') & (!is.na(criteria$primary_key)), 
                                  c('table', 'column')])
  
  # get_primary_keys - Get a list of primary key values
  # key = (character) key name
  # table.name = (character) full table name with file extension
  # d = (character) path to table.name
  get_primary_keys <- function(key, table.name){
    dir_tables <- unlist(
      lapply(
        seq(length(d)), 
        fnames_from_list, 
        i = 'fname', 
        lis = d)
    )
    table_pattern <- paste0(table.name, "\\b")
    use_i <- stringr::str_detect(dir_tables, table_pattern)
    if (sum(use_i) > 0){
      data_in <- d[[table.name]][['data']]
      uni_values <- unique(data_in[ , key])
      uni_values
    } else {
      uni_values <- NULL
      uni_values
    }
    
  }
  
  primary_key_tables <- x$table
  
  primary_key_values <- mapply(get_primary_keys, key = x$column, table.name = x$table)

  # Foreign keys should have a matching primary key
  
  is_foreign_key(fk.tables = foreign_key_tables, 
                 fk.values = foreign_key_values, 
                 pk.tables = primary_key_tables,
                 pk.values = primary_key_values
                 )
  
  # Send validation notice
  
  message('... tables have referential integrity\n')
  
}






# is_foreign_key - Check that foreign key has matching primary key
# fk.tables = (character) names of tables containing foreign keys
# fk.values = (list) values of foreign keys
# pk.values = (list) values of primary keys
is_foreign_key <- function(fk.tables, fk.values, pk.tables, pk.values){
  
  fk.names <- names(fk.values)
  pk.names <- names(pk.values)
  
  # Error if foreign key names don't have a primary match (missing).
  
  use_i <- match(fk.names[fk.names != 'parent_location_id'], pk.names)
  if (sum(is.na(use_i)) > 0){
    stop(paste0("\n",
                'These foreign keys:\n',
                paste(fk.names[is.na(use_i)], collapse = ', '),
                '\nare missing primary keys'
    )
    )
  }
  
  # Validate parent_location_id
  
  if (sum(fk.names == 'parent_location_id') > 0){
    fk_val <- fk.values[['parent_location_id']]
    if (!is.null(fk_val)){
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
    if ((!is.null(pk_val)) & (!is.null(fk_val))){
      use_i <- is.na(match(fk_val, pk_val))
      if (sum(use_i) > 0){
        stop(paste0('\nThe foreign key "event_id"',
                    '\ncontains values that cannot be found in the primary key',
                    '\n"event_id" of the observation_ancillary table, specifically\n',
                    paste(fk_val[use_i], collapse = ', ')
        )
        )
      }
    }
    use_i <- fk.names == 'event_id'
    fk.tables <- fk.tables[!use_i]
    fk.values <- fk.values[names(fk.values) != 'event_id']
    fk.names <- fk.names[!use_i]
  }
  
  # Validate variable_names
  # Aggregate all foreign key variable_names from tables
  # Match against variable_mappings table content
  
  
  # Validate remaining foreign keys
  
  other_foreign_keys <- function(fk.table, fk.name, fk.value){
    use_i <- seq(length(pk.names))[pk.names == fk.name]
    use_i <- match(fk.value, pk.values[[use_i]])
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
         fk.value = fk.values
         )
  
}










# Helper functions ------------------------------------------------------------










# Get 'fname' from d
fnames_from_list <- function(n, i, lis){
  lis[[n]][[i]]
} 











is_primary_key <- function(x, pk, table.name){
  
  if (!isTRUE(length(unique(x[ , pk])) == nrow(x))){
    
    use_i <- seq(nrow(x))[duplicated(x[ , pk])]
    
    stop(paste0("\n",
                'This table:\n',
                table.name,
                "\ncontains non-unique primary keys in rows:\n",
                paste(use_i, collapse = " "),
                "\nof this column:\n",
                pk),
         call. = F
    )
  }
}








is_required_table <- function(required_table, tables){
  required_table_pattern <- paste0(required_table, "\\b")
  if (sum(stringr::str_detect(tables, required_table_pattern)) == 1){
    required_table
  }
}










# Retrieves the name of the L1 table from the input file name.
is_table <- function(L1.table, table.name){
  required_table_pattern <- paste0(L1.table, "\\b")
  if (sum(stringr::str_detect(table.name, required_table_pattern)) == 1){
    L1.table
  }
}










# Retrieves the name of the L1 table from the input file name.
is_table_rev <- function(file.name, L1.table.names){
  required_table_pattern <- paste0(L1.table.names, "\\b")
  if (sum(stringr::str_detect(file.name, required_table_pattern)) == 1){
    L1.table.names[stringr::str_detect(file.name, required_table_pattern)]
  }
}








read_ecocomDP_table <- function(data.path = NULL, file.name, package.id = NULL, entity.id = NULL){
  
  if (!is.null(data.path)){
    
    sep <- EDIutils::detect_delimeter(
      path = data.path,
      data.files = file.name,
      os = EDIutils::detect_os()
    )
    
    x <- read.table(
      paste0(data.path, "/", file.name),
      header = T,
      sep = sep,
      as.is = T,
      quote = "\"",
      comment.char = ""
    )
    
  } else if (!is.null(package.id)){
    
    eml <- suppressMessages(
      EDIutils::api_read_metadata(package.id)
    )
    
    files <- xml2::xml_text(
      xml2::xml_find_all(eml, "//dataset/dataTable/entityName"))
    
    use_i <- match(file.name, files)
    
    sep <- xml2::xml_text(
      xml2::xml_find_all(
        eml, 
        "//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"))[use_i]
    
    data_url <- xml2::xml_text(
      xml2::xml_find_all(
        eml, 
        "//dataset/dataTable/physical/distribution/online/url"))[use_i]
    
    x <- data.table::fread(data_url)
    
  }
  
  list(data = x, fname = file.name)
  
}









#' Read ecocomDP from files
#'
#' @param data.path 
#'     (character) The path to the directory containing ecocomDP tables. 
#'     Duplicate file names are not allowed.
#'
#' @return
#'     (list) A named list of data frames, each with the contents of the 
#'     corresponding file. Names follow ecocomDP specification.
#'     
#' @note 
#'     This function will deprecate \code{read_ecocomDP_table()}
#'
#' @examples
#' d <- read_from_files(system.file("/data", package = "ecocomDP"))
#' 
read_from_files <- function(data.path) {
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  d <- lapply(
    unique(criteria$table),
    function(x) {
      ecocomDP_table <- stringr::str_detect(
        list.files(data.path), 
        paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
      if (any(ecocomDP_table)) {
        data.table::fread(
          paste0(data.path, "/", list.files(data.path)[ecocomDP_table]))
      }
    })
  names(d) <- unique(criteria$table)
  d[sapply(d, is.null)] <- NULL
  d
}
