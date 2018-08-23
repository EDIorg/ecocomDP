context('Spelling of column names is valid.')

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

testthat::test_that('Return message when column spelling is correct.', {
  
  expect_message(
    validate_column_names(
      tables = all_required_tables,
      data.path = data.path,
      criteria = criteria)
  )
  
})

# Column spelling is incorrect ------------------------------------------------



testthat::test_that('Return error when column spelling is incorrect.', {

  expect_error(
    is_valid_name(
      cols = c('taxon_ids',
               'taxon_rank',
               'taxon_names',
               'authority_system',
               'authority_taxon_ids'),
      L1_table_columns = c('taxon_id',
                           'taxon_rank',
                           'taxon_name',
                           'authority_system',
                           'authority_taxon_id'),
      table.name = 'Project_name_taxon.csv')
  )
  
})

