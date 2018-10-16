context('Composite keys should be unique.')

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

# Composite keys are unique ---------------------------------------------------

testthat::test_that('Unique keys result in NULL output.', {
  expect_message(
    validate_composite_keys(tables = file_names[c(1,2,4,5,6,7,8)],
                                    data.list = data.list,
                                    criteria = criteria)
  )
})

# Composite keys are not unique -----------------------------------------------
# There are valid explanations for this.

testthat::test_that('Non-unique keys result in warning, not error.', {
  
  expect_warning(
    validate_composite_keys(tables = file_names,
                            data.list = data.list,
                            criteria = criteria)
  )
  
})

