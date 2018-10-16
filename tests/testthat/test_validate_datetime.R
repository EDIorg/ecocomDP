context('Datetime format should match the standard.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

tables <- list.files(
  paste0(path.package('ecocomDP'), '/tests/test_data')
)

data.path <- paste0(path.package('ecocomDP'), '/tests/test_data')

data.list <- lapply(tables, read_ecocomDP_table, data.path = data.path)

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

criteria_2 <- criteria[
  ((complete.cases(criteria$column)) & 
     ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime'))), ]

# Datetime format is not correct ----------------------------------------------

# Test data contains invalid datetime formats

testthat::test_that('Invalid datetime formats result in error (check sub-function).', {
  expect_error(
    is_datetime_format(
      table.name = tables[2], 
      data.path = data.path,
      criteria = criteria_2
      )
  )
})

testthat::test_that('Invalid datetime formats result in error.', {
  expect_error(
    validate_datetime(
      tables = tables, 
      data.path = data.path,
      criteria = criteria
    )
  )
})

