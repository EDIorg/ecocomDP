context("Data readers")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

criteria <- data.table::fread(
  system.file('validation_criteria.txt', package = 'ecocomDP'))

# read_data() -----------------------------------------------------------------

testthat::test_that("read_data()", {

  # Read from EDI
  d <- try(
    read_data(id = "edi.193.3"), 
    silent = TRUE)
  if (class(d) != "try-error") {
    # Check for expected structure
    expect_true(is.list(d))
    expect_true(
      all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues")))
    expect_true(
      all(names(d[[1]]$tables) %in% unique(criteria$table)))
    # globally.unique.keys - Are created only if TRUE
    expect_false(
      all(
        stringr::str_detect(
          d[[1]]$tables$location$location_id, 
          "edi.193.3")))
    d <- try(
      read_data(id = "edi.193.3", globally.unique.keys = TRUE), 
      silent = TRUE)
    if (class(d) != "try-error") {
      expect_true(
        all(
          stringr::str_detect(
            d[[1]]$tables$location$location_id, 
            "edi.193.3")))
    }
  }
  
  # Read from NEON
  d <- try(
    read_data(
      id = "DP1.20166.001", 
      site = c("MAYF", "PRIN"), 
      startdate = "2016-01", 
      enddate = "2018-11"), 
    silent = TRUE)
  if (class(d) != "try-error") {
    # Check for expected structure
    expect_true(is.list(d))
    expect_true(
      all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues")))
    expect_true(
      all(names(d[[1]]$tables) %in% unique(criteria$table)))
    # globally.unique.keys - Are created only if TRUE
    expect_false(
      all(
        stringr::str_detect(
          d[[1]]$tables$location$location_id, 
          "DP1.20166.001")))
    d <- try(
      read_data(
        id = "DP1.20166.001", 
        site = c("MAYF", "PRIN"), 
        startdate = "2016-01", 
        enddate = "2018-11",
        globally.unique.keys = TRUE), 
      silent = TRUE)
    if (class(d) != "try-error") {
      expect_true(
        all(
          stringr::str_detect(
            d[[1]]$tables$location$location_id, 
            "DP1.20166.001")))
    }
  }
  
})

# read_from_files() -----------------------------------------------------------

testthat::test_that("read_from_files()", {
  
  file.copy(
    system.file("/data", package = "ecocomDP"),
    tempdir(),
    recursive = TRUE)
  
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
