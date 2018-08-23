context('Required tables must be present.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

criteria <- read.table(
  paste0(path.package("ecocomDP"),
         "/validation_criteria.txt"),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

all_required_tables <- list.files(
  paste0(path.package("ecocomDP"), "/test_data/")
  )

missing_required_tables <- all_required_tables[3:8]

# Required tables present -----------------------------------------------------

testthat::test_that('Return message when all required tables are present.', {
  
  expect_message(
    validate_table_presence(
      tables = all_required_tables,
      criteria = criteria)
  )
  
})

# Required tables missing -----------------------------------------------------

testthat::test_that('Error when required tables are missing.', {
  
  expect_error(
    validate_table_presence(
      tables = missing_required_tables,
      criteria = criteria)
  )
  
})

