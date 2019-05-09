context('Required columns are present.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")
L1_table_names <- unique(criteria$table[!is.na(criteria$column)])

data.path <- paste0(path.package('ecocomDP'), '/tests/test_data')
file_names <- list.files(
  paste0(path.package('ecocomDP'), '/tests/test_data')
)
data.list <- lapply(file_names, read_ecocomDP_table, data.path = data.path)
names(data.list) <- unlist(lapply(file_names, is_table_rev, L1_table_names))

# Column spelling is correct --------------------------------------------------

testthat::test_that('Return message when required columns are present.', {
  
  expect_message(
    validate_column_presence(
      tables = file_names,
      data.list = data.list,
      criteria = criteria)
  )
  
})

# Missing required columns ----------------------------------------------------

# is_valid_col_name <- function(cols, L1.table_columns, table.name)

testthat::test_that('Return error when required columns are missing.', {
  
  expect_error(
    is_valid_col_name(
      cols = c('taxon_rank',
               'taxon_name'),
      L1.table_columns = c('taxon_id',
                           'taxon_name'),
      table.name = 'Project_name_taxon.csv')
  )
  
})

