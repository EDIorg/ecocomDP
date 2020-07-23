# Tests are organized around function calls (e.g. all tests listed under 
# search_data() are relevant to the argument inputs to that function).

context("validate_arguments()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

# Read example data

test_data <- read_from_files(system.file("/data", package = "ecocomDP"))

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
  
  # data.path
  
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
  
  # data.list
  
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
        list(data.list = test_data2))))
  
})
