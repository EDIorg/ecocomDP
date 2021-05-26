context("create_location()")

library(ecocomDP)

# create_location() ----------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  flat <- ants_L0_flat
  res <- create_location(
    L0_flat = flat, 
    location_id = "location_id", 
    location_name = c("block", "plot"), 
    latitude = "latitude", 
    longitude = "longitude", 
    elevation = "elevation")
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "location"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
