context("plotting")

library(ecocomDP)

# Warns if datetimes are chars ------------------------------------------------

testthat::test_that("Warns if datetimes are chars", {
  id <- names(ants_L1)
  d <- ants_L1[[1]]$tables$observation
  d$datetime <- as.character(d$datetime)
  expect_warning(
    format_for_comm_plots(observation = d, id = id),
    regexp = "Input datetimes are character strings. Accuracy may be improved")
})

# Warns if duplicate observations ---------------------------------------------

testthat::test_that("Warns if duplicate observations", {
  d <- ants_L1
  d[[1]]$tables$observation <- rbind(
    d[[1]]$tables$observation,
    d[[1]]$tables$observation[1, ])
  id <- names(d)
  d <- d[[1]]$tables$observation
  expect_warning(
    format_for_comm_plots(observation = d, id = id),
    regexp = "duplicate observations. Consider aggregating")
})

# Warns if > 1 measurement variable -------------------------------------------

testthat::test_that("Warns if > 1 measurement variable", {
  d <- ants_L1
  obs2 <- d[[1]]$tables$observation                      # add a var and unit
  obs2$variable_name <- "Another var"
  obs2$unit <- "Another unit"
  d[[1]]$tables$observation <- rbind(d[[1]]$tables$observation, obs2)
  id <- names(d)
  d <- d[[1]]$tables$observation
  expect_warning(
    format_for_comm_plots(observation = d, id = id),
    regexp = "can only handle one. Consider splitting this dataset")
})

