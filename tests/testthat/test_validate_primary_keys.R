context('Primary keys should be unique.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

observation <- read.table(
  paste0(path.package("ecocomDP"), "/test_data/Project_name_observation.csv"),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA"
  )

# Primary keys are unique -----------------------------------------------------

testthat::test_that('Unique keys result in NULL output.', {
  
  expect_equal(
    is.null(validate_primary_keys(x = observation, col = 'observation_id')),
    TRUE
    )
  })

# Primary keys are not unique -------------------------------------------------

# Create duplicate primary keys

observation[1:5, 'observation_id'] <- 'ob_1'

testthat::test_that('Unique keys result in NULL output.', {
  
  expect_error(
    validate_primary_keys(x = observation, col = 'observation_id')
    )
  })


