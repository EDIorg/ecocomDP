# Tests are organized around function calls (e.g. all tests listed under 
# search_data() are relevant to the argument inputs to that function).

context("validate_arguments()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

test_data <- read_from_files(system.file("/data", package = "ecocomDP"))
search_index <- search_data()

# search_data() ---------------------------------------------------------------

testthat::test_that("search_data()", {
  
  # text
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(text = c(1, 2, 3)))))
  
  # taxa
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(taxa = c(1, 2, 3)))))
  
  # taxa.num
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(num.taxa = "Non-numeric value"))))
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(num.taxa = 1))))
  
  # years
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(years = "Non-numeric value"))))
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(years = 1))))
  
  # sd.between.surveys
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(sd.between.surveys = "Non-numeric value"))))
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(sd.between.surveys = 1))))
  
  # geographic.area
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(geographic.area = "Non-numeric value"))))
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(geographic.area = 1))))
  
  # boolean.operator
  
  expect_error(
    validate_arguments(
      "search_data",
      as.list(
        list(boolean.operator = "ANDrew"))))
  
})


# validate_ecocomDP() ---------------------------------------------------------

testthat::test_that("validate_ecocomDP()", {
  
  # data.path - Is valid
  
  expect_null(
    validate_arguments(
      "validate_ecocomDP",
      as.list(
        list(data.path = tempdir()))))
  
  expect_error(
    validate_arguments(
      "validate_ecocomDP",
      as.list(
        list(data.path = paste0(tempdir(), "/aoihebqlnvo333")))))
  
  # data.list - Is valid
  
  expect_null(
    validate_arguments(
      "validate_ecocomDP",
      as.list(
        list(data.list = test_data))))
  
  test_data2 <- test_data
  test_data2$unsupported_table <- NA
  expect_error(
    validate_arguments(
      "validate_ecocomDP",
      as.list(
        list(data.list = test_data2))),
    regexp = "Input \'data.list\' has unsupported tables:")
  
})

# read_data() -----------------------------------------------------------------

testthat::test_that("read_data()", {
  
  # id - If not valid, then a warning and NULL value is returned.
  
  expect_warning(
    validate_arguments(
      "read_data",
      as.list(
        list(
          id = c(
            "invalid_identifier_1", 
            "invalid_identifier_2", 
            "edi.359.1")))),
    regexp = "Invalid identifier \'.+\' cannot be read.")
  
  r <- suppressWarnings(
    validate_arguments(
      "read_data",
      as.list(
        list(
          id = c(
            "invalid_identifier_1", 
            "invalid_identifier_2", 
            "edi.359.1")))))
  expect_false("invalid_identifier_1" %in% r$id)
  expect_false("invalid_identifier_2" %in% r$id)
  expect_true("edi.359.1" %in% r$id)
  
  # id - If a newer revision exists, then id and a warning is returned.
  
  expect_warning(
    validate_arguments(
      "read_data",
      as.list(
        list(
          id = c("edi.275.3", "edi.359.1")))),
    regexp = "A newer version of \'.+\' is available.")
  
  r <- suppressWarnings(
    validate_arguments(
      "read_data",
      as.list(
        list(
          id = c("edi.275.3", "edi.359.1")))))
  expect_true(
    all(r$id %in% c("edi.275.3", "edi.359.1")))
  
  
  # path - Is valid
  
  # file.type - Is a supported type
  
  # site - Exists in the search_data() default output
  
  # startdate - Character of YYYY-MM format
  
  # enddate - Character of YYYY-MM format
  
  # check.size - Is logical
  
  # nCores - Is iteger
  
  # forceParallel - Is logical
  
})
