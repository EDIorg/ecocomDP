context("create_observation()")

library(ecocomDP)

# create_observation() --------------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  wide <- ants_L0_wide
  res <- create_observation(
    L0_wide = wide, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "abundance",
    unit = "unit_abundance")
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "observation"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
