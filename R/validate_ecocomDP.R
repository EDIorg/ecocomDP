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
  validate_column_classes(d)
  # FIXME: Uncomment the following lines
  # validate_primary_keys(d)
  # validate_composite_keys(d)
  validate_referential_integrity(d)
  
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
    (is.na(criteria$class)) & (criteria$required == TRUE)]
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
          (criteria$required == TRUE)]
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








#' Check for valid column classes
#'
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If column classes are not formmated to specification, then an error 
#'     is returned.
#'
validate_column_classes <- function(data.list) {
  
  message("Column classes")
  
  # Parameterize

  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))

  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      lapply(
        colnames(data.list[[x]]),
        function(k) {
          expected <- criteria$class[
            (criteria$table %in% x) &
              !is.na(criteria$column) &
              (criteria$column %in% k)]
          detected <- class(data.list[[x]][[k]])
          if (expected == "numeric") {
            if ((detected != "integer") &
                (detected != "integer64") &
                (detected != "double") &
                (detected != "numeric")) {
                issues <- list(
                  column = k, expected = expected, detected = detected)
            }
          } else if (expected == "Date") {
            if (detected != "character") {
              issues <- list(
                column = k, expected = expected, detected = detected)
            }
          }
          if (exists("issues")) {
            stop(
              "The ", k, " column in the ", x, " table has a ", detected, 
              " class but a ", expected, " class is expected.", 
              call. = FALSE)
          }
        })
    })

}








#' Check primary keys
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If primary keys are not unique, then an error is returned.
#'
validate_primary_keys <- function(data.list) {

  message("Primary keys")
  
  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      primary_key <- criteria$column[
        (criteria$table == x) & 
          !is.na(criteria$primary_key) & 
          (criteria$primary_key == TRUE)]
      v <- data.list[[x]][[primary_key]]
      use_i <- seq(nrow(v))[duplicated(v)]
      if (length(use_i) > 0) {
        stop("The column ", primary_key, " in the table ", x, 
             " contains non-unique primary keys in rows: ", 
             paste(use_i, collapse = " "), call. = FALSE)
      }
    })

}








#' Check composite column uniqueness
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If composite keys are not unique an error is returned.
#'
validate_composite_keys <- function(data.list) {
  
  message("Composite keys")
  
  # Parameterize
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Validate
  
  lapply(
    names(data.list),
    function(x) {
      composite_columns <- criteria$column[
        (criteria$table == x) & 
          !is.na(criteria$column) & 
          (criteria$composite_key == TRUE)]
      if (length(composite_columns) > 0) {
        d <- dplyr::select(data.list[[x]], composite_columns)
        duplicates <- seq(nrow(d))[duplicated.data.frame(d)]
        if (length(duplicates) > 0) {
          stop("The composite keys composed of the columns ", 
               paste(composite_columns, collapse = ", "), ", in the table ", 
               x, " contain non-unique values in rows: ", 
               paste(duplicates, collapse = " "), call. = FALSE)
        }
      }
    })
  
}








#' Check referential integrity
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     If foreign keys do not match primary keys, then an error is returned.
#'
validate_referential_integrity <- function(data.list) {

  message("Referential integrity")
  browser()  
  
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
