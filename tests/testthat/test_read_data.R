context("Data readers")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

test_data <- read_from_files(
  system.file("/data", package = "ecocomDP"))[[1]]$tables

# read_data() -----------------------------------------------------------------

testthat::test_that("read_data()", {
  
})

# read_from_files() -----------------------------------------------------------

testthat::test_that("read_from_files()", {
  
  file.copy(
    system.file("/data", package = "ecocomDP"),
    tempdir(),
    recursive = TRUE)
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Structure of returned object should be valid
  
  d <- read_from_files(
    data.path = paste0(tempdir(), "/data"))
  
  expect_true(is.list(d))
  expect_true(
    all(names(d[[1]]) %in% c("metadata", "tables")))
  expect_true(
    all(names(d[[1]]$tables) %in% unique(criteria$table)))
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/data"), 
    recursive = TRUE, 
    force = TRUE)
  
})
