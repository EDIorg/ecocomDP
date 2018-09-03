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
  
  validate_path(data.path)
  
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




# get_primary_keys - Get a list of primary key values
# key = (character) key name
# table.name = (character) full table name with file extension
# data.path = (character) path to table.name
get_primary_keys <- function(key, table.name, data.path){
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
