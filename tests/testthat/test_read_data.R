context("read_data()")

library(ecocomDP)

# Reads from source APIs ------------------------------------------------------

testthat::test_that("Reads from source APIs", {
  testthat::skip_on_cran()
  
  criteria <- read_criteria()
  # From EDI
  id <- "edi.193.3"
  d <- try(suppressWarnings(read_data(id = id)), silent = TRUE)
  if (class(d) != "try-error") {                                                      # Has expected structure
    expect_true(is.list(d))                                                           # obj is a list
    expect_true(names(d) == id)                                                       # 1st level name is id
    expect_true(all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues"))) # 2nd level has 3 values
    for (i in names(d[[1]])) {                                                        # 2nd level objs are lists
      expect_true(is.list(d[[1]][[i]]))
    }
    expect_true(all(names(d[[1]]$tables) %in% unique(criteria$table)))                # table names are valid
    for (i in names(d[[1]]$tables)) {                                                 # tables are data.frames
      expect_true(class(d[[1]]$tables[[i]]) == "data.frame")
    }
  }
  # From NEON
  id <- "neon.ecocomdp.20107.001.001"
  d <- try(
    suppressWarnings(
      read_data(
        id = id, 
        site = c("MAYF", "CARI"), 
        startdate = "2016-01", 
        enddate = "2016-12", 
        check.size = FALSE)), 
    silent = TRUE)
  if (class(d) != "try-error") {                                                      # Has expected structure
    expect_true(is.list(d))                                                           # obj is a list
    expect_true(names(d) == id)                                                       # 1st level name is id
    expect_true(all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues"))) # 2nd level has 3 values
    for (i in names(d[[1]])) {                                                        # 2nd level objs are lists
      expect_true(is.list(d[[1]][[i]]))
    }
    expect_true(all(names(d[[1]]$tables) %in% unique(criteria$table)))                # table names are valid
    for (i in names(d[[1]]$tables)) {                                                 # tables are data.frames
      expect_true(class(d[[1]]$tables[[i]]) == "data.frame")
    }
    expect_equal(                                                                     # datetimes are parsed
      class(d[[1]]$tables$observation$datetime),
      "Date")
    expect_equal(
      class(d[[1]]$tables$location_ancillary$datetime),
      "Date")
  }
})


# Reads from local .rds -------------------------------------------------------

testthat::test_that("Reads from local .rds", {
  criteria <- read_criteria()
  # From .rds
  d <- ants_L1
  id <- names(d)
  save_data(d, tempdir())
  d <- read_data(from = paste0(tempdir(), "/d.rds"))                                  # Has expected structure
  expect_true(is.list(d))                                                             # obj is a list
  expect_true(names(d) == id)                                                         # 1st level name is id
  expect_true(all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues")))   # 2nd level has 3 values
  for (i in names(d[[1]])) {                                                          # 2nd level objs are lists
    expect_true(is.list(d[[1]][[i]]))
  }
  expect_true(all(names(d[[1]]$tables) %in% unique(criteria$table)))                  # table names are valid
  for (i in names(d[[1]]$tables)) {                                                   # tables are data.frames
    expect_true(any(class(d[[1]]$tables[[i]]) == "data.frame"))
  }
  unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE) 
})

# Reads from local .csv directories -------------------------------------------

testthat::test_that("Reads from local .csv directories", {
  criteria <- read_criteria()
  # From .csv
  d <- ants_L1                                                         # create example datasets
  d <- c(d, d, d)
  ids <- c("edi.193.3", "edi.262.1", "edi.359.1")
  names(d) <- ids
  id <- "d"
  unlink(paste0(tempdir(),"/", ids), recursive = TRUE, force = TRUE) 
  save_data(d, tempdir(), type = ".csv")
  d <- read_data(from = tempdir())            # Has expected structure
  expect_true(is.list(d))                                                             # obj is a list
  expect_true(all(names(d) %in% ids))                                                 # 1st level name is id
  for (i in names(d)) {
    expect_true(all(names(d[[i]]) %in% c("metadata", "tables", "validation_issues"))) # 2nd level has 3 values
    for (j in names(d[[i]])) {                                                        # 2nd level objs are lists
      if (j != "metadata") {                                                          # metadata is lost when written to .csv
        expect_true(is.list(d[[i]][[j]]))
      }
    }
    expect_true(all(names(d[[i]]$tables) %in% unique(criteria$table)))                # table names are valid
    for (j in names(d[[i]]$tables)) {                                                 # tables are data.frames
      expect_true(class(d[[i]]$tables[[j]]) == "data.frame")
    }
  }
  unlink(paste0(tempdir(),"/", ids), recursive = TRUE, force = TRUE) 
})

# Reads tables with valid names in path ---------------------------------------

testthat::test_that("Reads tables with valid names in path", {
  # Parameterize
  mytopdir <- tempdir()
  mypath <- paste0(mytopdir, "/datasets")
  unlink(mypath, recursive = TRUE, force = TRUE)
  dir.create(mypath)
  crit <- read_criteria()
  d <- ants_L1
  # Set up test files
  save_data(d, mypath, type = ".csv")
  readpath <- paste0(mypath, "/", names(d))
  # Test
  d_fromfile <- read_data(from = readpath)
  expect_true(all(names(d_fromfile[[1]]$tables) %in% unique(crit$table)))
  expect_equal(length(d_fromfile[[1]]$tables), length(dir(readpath)))
  unlink(mypath, recursive = TRUE, force = TRUE)
})

# Ignores tables with invalid names in path -----------------------------------

testthat::test_that("Ignores tables with invalid names in path", {
  # Parameterize
  mytopdir <- tempdir()
  mypath <- paste0(mytopdir, "/datasets")
  unlink(mypath, recursive = TRUE, force = TRUE)
  dir.create(mypath)
  crit <- read_criteria()
  d <- ants_L1
  # Set up test files
  tblnms <- names(d[[1]]$tables)
  tblnms[c(1,2)] <- c("1", "2") 
  names(d[[1]]$tables) <- tblnms
  save_data(d, mypath, type = ".csv")
  readpath <- paste0(mypath, "/", names(d))
  # Test
  expect_warning(read_data(from = readpath), regexp = "Validation issues")
  d_fromfile <- suppressWarnings(read_data(from = readpath))
  expect_true(!any(c("1", "2") %in% names(d_fromfile[[1]]$tables)))
  expect_true(all(names(d_fromfile[[1]]$tables) %in% unique(crit$table)))
  expect_false(length(d_fromfile[[1]]$tables) == length(dir(readpath)))
  unlink(mypath, recursive = TRUE, force = TRUE)
})

# Has datetime parsing option -------------------------------------------------

testthat::test_that("Has datetime parsing option", {
  criteria <- read_criteria()
  d <- ants_L1
  id <- names(d)
  save_data(d, tempdir())
  d <- suppressWarnings(
    read_data(from = paste0(tempdir(), "/d.rds"), parse_datetime = TRUE))      # not character
  for (tbl in names(d[[1]]$tables)) {
    for (colname in colnames(d[[1]]$tables[[tbl]]))
      if (stringr::str_detect(colname, "datetime")) {
        expect_true(class(d[[1]]$tables[[tbl]][[colname]]) != "character")
      }
  }
  d <- suppressWarnings(
    read_data(from = paste0(tempdir(), "/d.rds"), parse_datetime = FALSE))     # character
  for (tbl in names(d[[1]]$tables)) {
    for (colname in colnames(d[[1]]$tables[[tbl]]))
      if (stringr::str_detect(colname, "datetime")) {
        expect_true(class(d[[1]]$tables[[tbl]][[colname]]) == "character")
      }
  }
  unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE)
})

