context("create_dataset_summary()")

library(ecocomDP)

# create_dataset_summary() ----------------------------------------------------

testthat::test_that("create_dataset_summary()", {
  crit <- read_criteria()
  wide <- ants_L0_wide
  res <- create_dataset_summary(
    L0_wide = wide, 
    package_id = "package_id", 
    original_package_id = "original_package_id", 
    length_of_survey_years = "length_of_survey_years",
    number_of_years_sampled = "number_of_years_sampled", 
    std_dev_interval_betw_years = "std_dev_interval_betw_years", 
    max_num_taxa = "max_num_taxa", 
    geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2")
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "dataset_summary"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})

# calc_geo_extent_bounding_box_m2() -------------------------------------------

testthat::test_that("calc_geo_extent_bounding_box_m2()", {
  res <- calc_geo_extent_bounding_box_m2(west = -123, east = -120, north = 45, 
                                         south = 23)
  expect_true(is.numeric(res))
})

# calc_length_of_survey_years() -----------------------------------------------

testthat::test_that("calc_length_of_survey_years()", {
  wide <- ants_L0_wide
  res <- calc_length_of_survey_years(dates = wide$datetime)
  expect_true(is.numeric(res))
})

# calc_number_of_years_sampled() ----------------------------------------------

testthat::test_that("calc_number_of_years_sampled()", {
  wide <- ants_L0_wide
  res <- calc_number_of_years_sampled(dates = wide$datetime)
  expect_true(is.numeric(res))
})

# calc_std_dev_interval_betw_years() ------------------------------------------

testthat::test_that("calc_std_dev_interval_betw_years()", {
  wide <- ants_L0_wide
  res <- calc_std_dev_interval_betw_years(dates = wide$datetime)
  expect_true(is.numeric(res))
})


