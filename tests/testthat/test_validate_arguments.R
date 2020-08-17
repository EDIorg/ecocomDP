# Tests are organized around function calls (e.g. all tests listed under 
# search_data() are relevant to the argument inputs to that function).

context("validate_arguments()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

test_data <- read_from_files(
  system.file("/data", package = "ecocomDP"))[[1]]$tables
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
  
  # format - Because inputs to read_data() can vary (i.e. can be a vector of 
  # id, list of id with associated arguments), they need to be converted to a 
  # consistent format for processing.
  
  # format - NEON id has default values of optional filter arguments listed 
  # under id. EDI id has NULL listed.
  
  input <- formals(ecocomDP::read_data)
  input$id <- c("edi.193.3", "DP1.20166.001")
  r <- validate_arguments("read_data", input)
  expect_null(r$id[[1]])
  expect_true(
    all(unlist(r$id[[2]]) %in% unlist(formals(ecocomDP::read_data))))
  
  # format - Single NEON id and custom filter and download arguments are listed 
  # in the correct locations
  
  input <- formals(ecocomDP::read_data)
  input$id <- list(
    edi.193.3 = NULL,
    DP1.20166.001 = list(
      site = c("MAYF", "PRIN"),
      startdate = "2016-01",
      enddate = "2018-11"),
    DP1.20120.001 = list(
      site = "ARIK",
      startdate = "2016-01",
      enddate = "2018-11"))
  
  r <- validate_arguments("read_data", input)
  expect_equal(r$id[[2]]$site, c("MAYF", "PRIN"))
  expect_true(r$id[[2]]$startdate == "2016-01")
  expect_true(r$id[[2]]$enddate == "2018-11")
  expect_equal(r$id[[3]]$site, c("ARIK"))
  expect_true(r$id[[3]]$startdate == "2016-01")
  expect_true(r$id[[3]]$enddate == "2018-11")
  
  # format - Two NEON id and custom filter and download arguments input as 
  # recommended
  
  input <- formals(ecocomDP::read_data)
  input$id <- "DP1.20166.001"
  input$site <- "ARIK"
  input$startdate <- "2020-07"
  input$enddate <- "2020-08"
  input$check.size <- TRUE
  input$nCores <- 2
  input$forceParallel <- TRUE
  input$path <- tempdir()
  input$file.type <- ".csv"
  
  r <- validate_arguments("read_data", input)
  expect_true(r$id[[1]]$site == "ARIK")
  expect_true(r$id[[1]]$startdate == "2020-07")
  expect_true(r$id[[1]]$enddate == "2020-08")
  expect_true(r$path == tempdir())
  expect_true(isTRUE(r$check.size))
  expect_true(isTRUE(r$forceParallel))
  expect_true(r$nCores == 2)
  expect_true(r$file.type == ".csv")

  # id - If not valid, then error
  
  input <- formals(ecocomDP::read_data)
  input$id <- "invalid_identifier_1"
  expect_error(
    validate_arguments("read_data", input),
    regexp = "Invalid identifier \'.+\' cannot be read.")
  
  input <- formals(ecocomDP::read_data)
  input$id <- "edi.359.1"
  expect_silent(
    suppressWarnings(validate_arguments("read_data", input)))
  
  input <- formals(ecocomDP::read_data)
  input$id <- "DP1.20120.001"
  expect_silent(
    validate_arguments("read_data", input))
  
  # id - If a newer revision exists, then id and a warning is returned.
  
  input <- formals(ecocomDP::read_data)
  input$id <- "edi.275.3"
  expect_warning(
    validate_arguments("read_data", input),
    regexp = "A newer version of \'.+\' is available.")
  
  # path - Is valid
  
  input <- formals(ecocomDP::read_data)
  input$path <- "/some/invalid/path"
  input$id <- "edi.359.1"
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Input \'path\' .+ doesn\'t exist.")
  
  # file.type - Is a supported type
  
  input <- formals(ecocomDP::read_data)
  input$file.type <- ".txt"
  input$id <- "edi.359.1"
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported 'file.type'. One of '.rda', '.csv' is expected.")
  
  # site - Exists for the data package/product identifier in the search_data() 
  # default output

  input <- formals(ecocomDP::read_data)
  input$id <- list(
    DP1.20120.001 = list(
      site = c("ARIK", "not a site")))
  expect_error(
    validate_arguments("read_data", input),
    regexp = "Sites not available in .+:")
  
  # startdate - Character of YYYY-MM format, and MM is 1-12
  
  input <- formals(ecocomDP::read_data)
  input$id <- list(
    DP1.20120.001 = list(
      startdate = "2020-08-12"))
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'startdate\'. Expected format is YYYY-MM.")
  
  input <- formals(ecocomDP::read_data)
  input$id <- list(
    DP1.20120.001 = list(
      startdate = "2020-13"))
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'startdate\'. Expected format is YYYY-MM.")
  
  # enddate - Character of YYYY-MM format
  
  input <- formals(ecocomDP::read_data)
  input$id <- list(
    DP1.20120.001 = list(
      enddate = "2020-08-12"))
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'enddate\'. Expected format is YYYY-MM.")
  
  input <- formals(ecocomDP::read_data)
  input$id <- list(
    DP1.20120.001 = list(
      enddate = "2020-13"))
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'enddate\'. Expected format is YYYY-MM.")
  
  # check.size - Is logical
  
  input <- formals(ecocomDP::read_data)
  input$check.size <- 2
  input$id <- "edi.359.1"
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'check.size\' input. Expected is TRUE or FALSE.")
  
  # nCores - Is iteger
  
  input <- formals(ecocomDP::read_data)
  input$nCores <- 2.5
  input$id <- "edi.359.1"
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'nCores\' input. Expected is an integer value.")
  
  # forceParallel - Is logical
  
  input <- formals(ecocomDP::read_data)
  input$forceParallel <- "yes"
  input$id <- "edi.359.1"
  expect_error(
    suppressWarnings(validate_arguments("read_data", input)),
    regexp = "Unsupported \'forceParallel\' input. Expected is TRUE or FALSE.")
  
})
