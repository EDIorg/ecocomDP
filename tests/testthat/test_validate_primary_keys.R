context('Primary keys should be unique.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

observation <- read.table(
  paste0(path.package('ecocomDP'), '/tests/test_data/Project_name_observation.csv'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA"
  )

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

# Primary keys are unique -----------------------------------------------------

testthat::test_that('Unique keys result in NULL output.', {
  
  expect_equal(
    is.null(is_primary_key(x = observation, pk = 'observation_id', table.name = 'Project_name_observation.csv')),
    TRUE
  )
})

testthat::test_that('Unique keys result in message.', {
  
  expect_message(
    validate_primary_keys(tables = all_required_tables, data.path = data.path, criteria = criteria)
    )
  
  })

# Primary keys are not unique -------------------------------------------------

# Create duplicate primary keys

observation[1:5, 'observation_id'] <- 'ob_1'

testthat::test_that('Unique keys result in NULL output.', {
  
  expect_error(
    is_primary_key(x = observation, pk = 'observation_id', table.name = 'Project_name_observation.csv')
    )
  })


