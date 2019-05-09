context('Column names have correct spelling.')

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
EDIutils::validate_path(data.path)
data.list <- lapply(file_names, read_ecocomDP_table, data.path = data.path)
names(data.list) <- unlist(lapply(file_names, is_table_rev, L1_table_names))

# Column spelling is correct --------------------------------------------------

testthat::test_that('Return message when column spelling is correct.', {
  
  expect_message(
    validate_column_names(
      tables = file_names,
      data.list = data.list,
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
      L1.table_columns = c('taxon_id',
                           'taxon_rank',
                           'taxon_name',
                           'authority_system',
                           'authority_taxon_id'),
      table.name = 'Project_name_taxon.csv')
  )

})

