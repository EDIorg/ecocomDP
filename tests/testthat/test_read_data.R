context("read_data()")

library(ecocomDP)

testthat::test_that("Supports APIs, local files, and datetime parsing", {
  criteria <- read_criteria()
  # From API (EDI)
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
      expect_true(is.data.frame(d[[1]]$tables[[i]]))
    }
  }
  # From API (NEON)
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
      expect_true(is.data.frame(d[[1]]$tables[[i]]))
    }
  }
  # From files
  if (class(d) != "try-error") {
    # .rds
    save_data(d, tempdir())
    dread <- suppressWarnings(read_data(from.file = paste0(tempdir(), "/d.rds")))         # Has expected structure
    expect_true(is.list(dread))                                                           # obj is a list
    expect_true(names(dread) == id)                                                       # 1st level name is id
    expect_true(all(names(dread[[1]]) %in% c("metadata", "tables", "validation_issues"))) # 2nd level has 3 values
    for (i in names(dread[[1]])) {                                                        # 2nd level objs are lists
      expect_true(is.list(dread[[1]][[i]]))
    }
    expect_true(all(names(dread[[1]]$tables) %in% unique(criteria$table)))                # table names are valid
    for (i in names(dread[[1]]$tables)) {                                                 # tables are data.frames
      expect_true(is.data.frame(dread[[1]]$tables[[i]]))
    }
    unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE) 
    # .csv
    unlink(paste0(tempdir(),"/", id), recursive = TRUE, force = TRUE) 
    save_data(d, tempdir(), file.type = ".csv")
    dread <- suppressWarnings(read_data(from.file = paste0(tempdir(), "/", id)))          # Has expected structure
    expect_true(is.list(dread))                                                           # obj is a list
    expect_true(names(dread) == id)                                                       # 1st level name is id
    expect_true(all(names(dread[[1]]) %in% c("metadata", "tables", "validation_issues"))) # 2nd level has 3 values
    for (i in names(dread[[1]])) {                                                        # 2nd level objs are lists
      if (i != "metadata") {                                                              # metadata is lost when written to .csv
        expect_true(is.list(dread[[1]][[i]]))
      }
    }
    expect_true(all(names(dread[[1]]$tables) %in% unique(criteria$table)))                # table names are valid
    for (i in names(dread[[1]]$tables)) {                                                 # tables are data.frames
      expect_true(is.data.frame(dread[[1]]$tables[[i]]))
    }
    unlink(paste0(tempdir(),"/", id), recursive = TRUE, force = TRUE) 
  }
  # Return datetimes as character if directed
  if (class(d) != "try-error") {
    save_data(d, tempdir())
    dread <- suppressWarnings(
      read_data(from.file = paste0(tempdir(), "/d.rds"), parse.datetime = TRUE))          # not character
    for (tbl in names(dread[[1]]$tables)) {
      for (colname in colnames(dread[[1]]$tables[[tbl]]))
        if (stringr::str_detect(colname, "datetime")) {
          expect_true(class(dread[[1]]$tables[[tbl]][[colname]]) != "character")
        }
    }
    dread <- suppressWarnings(
      read_data(from.file = paste0(tempdir(), "/d.rds"), parse.datetime = FALSE))         # character
    for (tbl in names(dread[[1]]$tables)) {
      for (colname in colnames(dread[[1]]$tables[[tbl]]))
        if (stringr::str_detect(colname, "datetime")) {
          expect_true(class(dread[[1]]$tables[[tbl]][[colname]]) == "character")
        }
    }
    unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE)
  }
})

