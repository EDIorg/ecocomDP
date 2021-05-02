# Tests are organized around function calls (e.g. all tests listed under 
# search_data() are relevant to the argument inputs to that function).

context("validate_arguments()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

test_data <- read_from_files(
  system.file("/data", package = "ecocomDP"))[[1]]$tables
search_index <- search_data()

# plot_*() --------------------------------------------------------------------

testthat::test_that("plot_*()", {
  # dataset (see validate_dataset_structure() tests)
  # alpha
  expect_null(validate_arguments("plot", as.list(list(alpha = 1))))         # is between 0 and 1
  expect_error(validate_arguments("plot", as.list(list(alpha = -1))))
  expect_error(validate_arguments("plot", as.list(list(alpha = 2))))
})

# read_data() -----------------------------------------------------------------

testthat::test_that("read_data()", {
  # id
  expect_error(
    validate_arguments("read_data", as.list(list(id = 1))), 
    regexp = "Input 'id' should be character.")
  expect_error(                                                             # exists
    validate_arguments("read_data", as.list(list(id = "edi.x.x"))), 
    regexp = "Invalid identifier 'edi.x.x' cannot be read.")
  expect_warning(                                                           # warns if newer revision
    validate_arguments("read_data", as.list(list(id = "edi.124.3"))), 
    regexp = "A newer version of 'edi.124.3' is available.")
  # path
  expect_null(validate_arguments("read_data", as.list(list(path = tempdir())))) # exists
  expect_error(
    validate_arguments("read_data", as.list(list(path = "/some/invalid/path"))),
    regexp = "Input \'path\' .+ doesn\'t exist.")
  # parse.datetime
  expect_null(                                                                 # is logical
    validate_arguments("read_data", as.list(list(parse.datetime = TRUE))))
  expect_error(
    validate_arguments("read_data", as.list(list(parse.datetime = "TRUE"))),
    regexp = "Input 'parse.datetime' should be logical.")
  # globally.unique.keys
  expect_null(                                                                 # is logical
    validate_arguments("read_data", as.list(list(globally.unique.keys = TRUE))))
  expect_error(
    validate_arguments("read_data", as.list(list(globally.unique.keys = "TRUE"))),
    regexp = "Input 'globally.unique.keys' should be logical.")
  # site
  expect_null(                                                                 # site exists for id
    validate_arguments("read_data", as.list(list(id = "neon.ecocomdp.20120.001.001",
                                               site = c("ARIK")))))
  expect_error(
    validate_arguments("read_data", as.list(list(id = "neon.ecocomdp.20120.001.001",
                                                 site = c("ARIK", "not an id")))),
    regexp = "Sites not available in neon.ecocomdp.20120.001.001: not an id")
  # startdate
  expect_null(                                                                 # has YYYY-MM format and MM is 1-12
    validate_arguments("read_data", as.list(list(startdate = "2020-12"))))
  expect_error(
    validate_arguments("read_data", as.list(list(startdate = "2020-12-30"))),
    regexp = "Unsupported 'startdate'. Expected format is YYYY-MM.")
  expect_error(
    validate_arguments("read_data", as.list(list(startdate = "2020-13"))),
    regexp = "Unsupported 'startdate'. Expected format is YYYY-MM.")
  # enddate
  expect_null(                                                                 # has YYYY-MM format and MM is 1-12
    validate_arguments("read_data", as.list(list(enddate = "2020-12"))))
  expect_error(
    validate_arguments("read_data", as.list(list(enddate = "2020-12-30"))),
    regexp = "Unsupported 'enddate'. Expected format is YYYY-MM.")
  expect_error(
    validate_arguments("read_data", as.list(list(enddate = "2020-13"))),
    regexp = "Unsupported 'enddate'. Expected format is YYYY-MM.")
  # package
  expect_null(                                                                 # has expected type
    validate_arguments("read_data", as.list(list(package = "basic"))))
  expect_null(
    validate_arguments("read_data", as.list(list(package = "expanded"))))
  expect_error(
    validate_arguments("read_data", as.list(list(package = "invalid value"))),
    regexp = "Input 'package' should be 'basic' or 'expanded'.")
  # check.size
  expect_null(                                                                 # is logical
    validate_arguments("read_data", as.list(list(check.size = TRUE))))
  expect_error(
    validate_arguments("read_data", as.list(list(check.size = "TRUE"))),
    regexp = "Input 'check.size' should be logical.")
  # nCores
  expect_null(                                                                 # is logical
    validate_arguments("read_data", as.list(list(nCores = 1))))
  expect_error(
    validate_arguments("read_data", as.list(list(nCores = 2.5))),
    regexp = "Input 'nCores' should be integer.")
  # forceParallel
  expect_null(                                                                 # is logical
    validate_arguments("read_data", as.list(list(forceParallel = TRUE))))
  expect_error(
    validate_arguments("read_data", as.list(list(forceParallel = "TRUE"))),
    regexp = "Input 'forceParallel' should be logical.")
  # neon.data.save.dir
  expect_null(validate_arguments("read_data", as.list(list(neon.data.save.dir = tempdir())))) # exists
  expect_error(
    validate_arguments("read_data", as.list(list(neon.data.save.dir = "/some/invalid/path"))),
    regexp = "Input 'neon.data.save.dir' .+ doesn\'t exist.")
  # from.file
  expect_null(                                                                 # file or dir exists
    validate_arguments("read_data", as.list(list(from.file = tempdir()))))
  expect_error(
    validate_arguments("read_data", as.list(list(from.file = "/not/a/dir"))),
    regexp = "Input 'from.file' is a non-existant file or directory.")
})

# save_data() -----------------------------------------------------------------

testthat::test_that("save_data()", {
  # path
  expect_null(validate_arguments("save_data", as.list(list(path = tempdir())))) # exists
  expect_error(
    validate_arguments("read_data", as.list(list(path = "/some/invalid/path"))),
    regexp = "Input \'path\' .+ doesn\'t exist.")
  # file.type
  expect_null(                                                                 # has expected type
    validate_arguments("save_data", as.list(list(file.type = ".rds"))))
  expect_null(
    validate_arguments("save_data", as.list(list(file.type = ".csv"))))
  expect_error(
    validate_arguments("save_data", as.list(list(file.type = "invalid value"))),
    regexp = "Input 'file.type' should be '.rds' or '.csv'.")
})

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


# validate_data() ---------------------------------------------------------

testthat::test_that("validate_data()", {
  test_data <- read_example_dataset()
  # data.path - Is valid
  expect_null(
    validate_arguments("validate_data", as.list(list(data.path = tempdir()))))
  expect_error(
    validate_arguments("validate_data", as.list(list(data.path = paste0(tempdir(), "/aoihebqlnvo333")))))
  # data.list - Is valid
  expect_null(
    validate_arguments("validate_data", as.list(list(dataset = test_data))))
})

# validate_dataset_structure() ------------------------------------------------

testthat::test_that("validate_dataset_structure()", {
  test_data <- read_example_dataset()
  expect_null(validate_dataset_structure(test_data))
  d <- unlist(test_data)                       # obj is a list
  expect_error(validate_dataset_structure(d))
  d <- unname(test_data)                       # 1st level name is id
  expect_error(validate_dataset_structure(d))
  d <- test_data                               # 2nd level has tables
  names(d[[1]]) <- c("metadata", "invalid name", "validation_issues")
  expect_error(validate_dataset_structure(d))
  d <- test_data                               # table names are valid
  nms <- names(d[[1]]$tables)
  nms[1] <- "invalid name"
  names(d[[1]]$tables) <- nms
  expect_error(validate_dataset_structure(d))
  d <- test_data                               # tables are data.frames
  d[[1]]$tables[[1]] <- as.list(d[[1]]$tables[[1]])
  expect_error(validate_dataset_structure(d))
})
