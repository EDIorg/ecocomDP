context('Tables should have referential integrity.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

data.path <- paste0(path.package('ecocomDP'), '/tests/test_data')

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

table_names <- validate_table_names(data.path = data.path,
                                    criteria = criteria
                                    )

# get_foreign_keys returns a non-zero length vector ---------------------------

testthat::test_that('get_foreign_keys returns a non-zero length vector.', {
  
  output <- get_foreign_keys(key = 'taxon_id',
                             table.name = 'Project_name_taxon_ancillary.csv',
                             data.path = data.path
                             )
  
  expect_equal(
    (length(output) > 0),
    TRUE
  )
})

# get_primary_keys returns a non-zero length vector ---------------------------

testthat::test_that('get_primary_keys returns a non-zero length vector.', {
  
  output <- get_primary_keys(key = 'taxon_id',
                             table.name = 'Project_name_taxon.csv',
                             data.path = data.path
                             )
  
  expect_equal(
    (length(output) > 0),
    TRUE
  )
})

# Error if foreign keys don't match primary keys ------------------------------

# Is valid

# is_foreign_key(fk.tables, fk.values, pk.values)

testthat::test_that('Error if foreign keys do not match primary keys.', {

  # Foreign keys
  
  x <- unique.data.frame(criteria[(criteria$foreign_key == 'yes') & (!is.na(criteria$foreign_key)), 
                                  c('table', 'column')])
  foreign_key_tables <- x$table
  foreign_key_values <- mapply(get_foreign_keys, key = x$column, table.name = x$table, data.path = data.path)
  
  # Primary keys
  
  x <- unique.data.frame(criteria[(criteria$primary_key == 'yes') & (!is.na(criteria$primary_key)), 
                                  c('table', 'column')])
  primary_key_tables <- x$table
  primary_key_values <- mapply(get_primary_keys, key = x$column, table.name = x$table, data.path = data.path)
  
  # Violate referential integrity
  
  use_i <- !is.na(primary_key_values[['taxon_id']])
  primary_key_values[['taxon_id']] <- primary_key_values[['taxon_id']][use_i]
  
  expect_error(
    is_foreign_key(fk.tables = foreign_key_tables, 
                   fk.values = foreign_key_values, 
                   pk.values = primary_key_values
                   )
    )
})

# Valid referential integrity results in a message ----------------------------

testthat::test_that('Valid referential integrity results in a message.', {
  
  expect_message(
    validate_referential_integrity(tables = table_names, 
                                   data.path = data.path, 
                                   criteria = criteria
    )
  )
  
})
