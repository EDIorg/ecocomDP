context("plotting")

library(ecocomDP)

# Warns if datetimes are chars ------------------------------------------------

testthat::test_that("Warns if datetimes are chars", {
  d <- read_example_dataset(parse.datetime = FALSE)
  id <- names(d)
  expect_warning(
    format_for_comm_plots(d),
    regexp = "Input datetimes are character strings. Accuracy may be improved")
})

# Warns if duplicate observations ---------------------------------------------

testthat::test_that("Warns if duplicate observations", {
  d <- read_example_dataset()
  id <- names(d)
  expect_warning(
    format_for_comm_plots(d),
    regexp = "duplicate observations. Consider aggregating")
})

# Warns if > 1 measurement variable -------------------------------------------

testthat::test_that("Warns if > 1 measurement variable", {
  d <- read_example_dataset()
  obs2 <- d[[1]]$tables$observation                      # add a var and unit
  obs2$variable_name <- "Another var"
  obs2$unit <- "Another unit"
  d[[1]]$tables$observation <- rbind(d[[1]]$tables$observation, obs2)
  id <- names(d)
  expect_warning(
    format_for_comm_plots(d),
    regexp = "duplicate observations. Consider aggregating")
})

