context('Composite keys should be unique.')

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

# Composite keys are unique ---------------------------------------------------

testthat::test_that('Unique keys result in NULL output.', {
  
  expect_equal(
    is.null(validate_composite_keys(tables = all_required_tables[c(1,2,4,5,6,7,8)],
                                    data.path = data.path,
                                    criteria = criteria)),
    TRUE
  )
})

# Composite keys are not unique -----------------------------------------------
# There are valid explanations for this.

testthat::test_that('Non-unique keys result in warning, not error.', {
  
  expect_warning(
    validate_composite_keys(tables = all_required_tables,
                            data.path = data.path,
                            criteria = criteria)
  )
  
})

