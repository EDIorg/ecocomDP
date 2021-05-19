context("create_location_ancillary()")

library(ecocomDP)

# create_location_ancillary() -------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  wide <- ants_L0_wide
  res <- create_location_ancillary(
    L0_wide = wide,
    location_id = "location_id",
    variable_name = "treatment")
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "location_ancillary"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
