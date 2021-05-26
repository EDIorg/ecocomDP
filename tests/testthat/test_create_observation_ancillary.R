context("create_observation_ancillary()")

library(ecocomDP)

# create_observation_ancillary() ----------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  flat <- ants_L0_flat
  res <- create_observation_ancillary(
    L0_flat = flat,
    observation_id = "observation_id", 
    variable_name = c("trap.type", "trap.num", "moose.cage"))
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "observation_ancillary"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
