context("plot_data()")

library(ecocomDP)

testthat::test_that("Plots are written to file", {
  d <- read_example_dataset()
  id <- names(d)
  dname <- paste0(tempdir(), "/plots")
  unlink(dname, recursive = TRUE, force = TRUE)
  dir.create(dname)
  suppressWarnings(plot_data(d, type = "community", path = dname))
  fnames <- list.files(dname)
  expect_true(length(fnames) != 0)                                             # dir is not empty
  suffix <- c("alpha_diversity", "sampling_effort", 
              "sp_accumulation_over_space", "sp_accumulation_over_time", 
              "sp_shared_among_sites")
  expect_true(all(stringr::str_detect(fnames, paste(suffix, collapse = "|")))) # file suffix describes plot
  unlink(dname, recursive = TRUE, force = TRUE)
})


testthat::test_that("Warns if datetimes are chars", {
  d <- read_example_dataset(parse.datetime = FALSE)
  id <- names(d)
  dname <- paste0(tempdir(), "/plots")
  unlink(dname, recursive = TRUE, force = TRUE)
  dir.create(dname)
  expect_warning(
    plot_data(d, type = "community", path = dname),
    regexp = "Input datetimes are character strings. Accuracy may be improved")
  unlink(dname, recursive = TRUE, force = TRUE)
})

