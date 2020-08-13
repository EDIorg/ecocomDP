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
  
  # TODO: Test input of list of id and nested attributes
  
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
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(path = "/some/invalid/path"))),
    regexp = "Input \'path\' doesn\'t exist.")
  
  # file.type - Is a supported type
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(file.type = ".txt"))),
    regexp = "Unsupported 'file.type'. One of '.rda', '.csv' is expected.")
  
  # TODO: site - Exists for the data package/product identifier in the 
  # search_data() default output
  
  expect_error(
    validate_arguments(
      "read_data",
      list(id = "DP1.20120.001", site = c("ARIK", "not a site"))),
    regexp = "Unsupported \'site\'. Expected format is YYYY-MM.")
  
  # startdate - Character of YYYY-MM format, and MM is 1-12
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(startdate = "2020-08-12"))),
    regexp = "Unsupported \'startdate\'. Expected format is YYYY-MM.")
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(startdate = "2020-13"))),
    regexp = "Unsupported \'startdate\'. Expected format is YYYY-MM.")
  
  r <- validate_arguments(
    "read_data",
    as.list(list(startdate = "2020-12")))
  expect_equal(r$startdate, "2020-12")
  
  # enddate - Character of YYYY-MM format
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(enddate = "2020-08-12"))),
    regexp = "Unsupported \'enddate\'. Expected format is YYYY-MM.")
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(enddate = "2020-13"))),
    regexp = "Unsupported \'enddate\'. Expected format is YYYY-MM.")
  
  r <- validate_arguments(
    "read_data",
    as.list(list(enddate = "2020-12")))
  expect_equal(r$enddate, "2020-12")
  
  # check.size - Is logical
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(check.size = 2))),
    regexp = "Unsupported \'check.size\' input. Expected is TRUE or FALSE.")
  
  r <- validate_arguments(
    "read_data",
    as.list(list(check.size = TRUE)))
  expect_equal(r$check.size, TRUE)
  
  # nCores - Is iteger
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(nCores = 2.5))),
    regexp = "Unsupported \'nCores\' input. Expected is an integer value.")
  
  r <- validate_arguments(
    "read_data",
    as.list(list(nCores = 2)))
  expect_equal(r$nCores, 2)
  
  # forceParallel - Is logical
  
  expect_error(
    validate_arguments(
      "read_data",
      as.list(list(forceParallel = "yes"))),
    regexp = "Unsupported \'forceParallel\' input. Expected is TRUE or FALSE.")
  
  r <- validate_arguments(
    "read_data",
    as.list(list(forceParallel = TRUE)))
  expect_equal(r$forceParallel, TRUE)
  
})
