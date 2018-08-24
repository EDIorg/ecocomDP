context('Required columns are present.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

all_required_tables <- list.files(
  paste0(path.package('ecocomDP'), '/tests/test_data')
)

data.path <- paste0(path.package('ecocomDP'), '/tests/test_data')

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

# Column spelling is correct --------------------------------------------------

testthat::test_that('Return message when required columns are present.', {
  
  expect_message(
    validate_column_presence(
      tables = all_required_tables,
      data.path = data.path,
      criteria = criteria)
  )
  
})

# Missing required columns ----------------------------------------------------

is_valid_col_name <- function(cols, L1.table_columns, table.name)

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

