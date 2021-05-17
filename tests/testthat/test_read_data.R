context("read_data()")

library(ecocomDP)

# Reads from source APIs ------------------------------------------------------

testthat::test_that("Reads from source APIs", {
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


# Reads from local files ------------------------------------------------------

testthat::test_that("Reads from local files", {
  criteria <- read_criteria()
  # From .rds
  d <- read_example_dataset()
  id <- names(d)
  save_data(d, tempdir())
  d <- suppressWarnings(read_data(from = paste0(tempdir(), "/d.rds")))           # Has expected structure
  expect_true(is.list(d))                                                             # obj is a list
  expect_true(names(d) == id)                                                         # 1st level name is id
  expect_true(all(names(d[[1]]) %in% c("metadata", "tables", "validation_issues")))   # 2nd level has 3 values
  for (i in names(d[[1]])) {                                                          # 2nd level objs are lists
    expect_true(is.list(d[[1]][[i]]))
  }
  expect_true(all(names(d[[1]]$tables) %in% unique(criteria$table)))                  # table names are valid
  for (i in names(d[[1]]$tables)) {                                                   # tables are data.frames
    expect_true(class(d[[1]]$tables[[i]]) == "data.frame")
  }
  unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE) 
  # From .csv
  d <- read_example_dataset()                                                         # create example datasets
  d <- c(d, d, d)
  ids <- c("edi.193.3", "edi.262.1", "edi.359.1")
  names(d) <- ids
  id <- "d"
  unlink(paste0(tempdir(),"/", id), recursive = TRUE, force = TRUE) 
  save_data(d, tempdir(), type = ".csv")
  d <- suppressWarnings(read_data(from = paste0(tempdir(), "/", id)))            # Has expected structure
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
  unlink(paste0(tempdir(),"/", id), recursive = TRUE, force = TRUE) 
})


# Has datetime parsing option -------------------------------------------------

testthat::test_that("Has datetime parsing option", {
  criteria <- read_criteria()
  d <- read_example_dataset()
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

