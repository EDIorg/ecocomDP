context("save_data()")

library(ecocomDP)


testthat::test_that("Objects are saved to fomat and have expected structure", {
  criteria <- read_criteria()
  d <- ants_L1                                               # create example datasets
  datasets <- c(d, d, d)
  ids <- c("edi.193.3", "edi.262.1", "edi.359.1")
  names(datasets) <- ids
  # .rds
  pathrds <- paste0(tempdir(),"/datasets.rds")
  unlink(pathrds, recursive = TRUE, force = TRUE)
  save_data(datasets, tempdir(), type = ".rds")              # .rds
  expect_true(file.exists(pathrds))                          # was created
  unlink(pathrds, recursive = TRUE, force = TRUE)
  # .csv
  pathcsv <- paste0(tempdir(),"/datasets")
  unlink(pathcsv, recursive = TRUE, force = TRUE)
  dir.create(pathcsv)
  save_data(datasets, pathcsv, type = ".csv")                # .csv
  expect_true(all(ids %in% dir(pathcsv)))                    # dir has subdirs
  r <- lapply(
    dir(pathcsv),
    function(dataset) {
      dname <- paste0(pathcsv, "/", dataset)
      fnames <- tools::file_path_sans_ext(list.files(dname)) # dir has tables
      expect_true(all(fnames %in% unique(criteria$table)))
      fext <- unique(tools::file_ext(list.files(dname)))     # has file extension
      expect_equal(fext, "csv")
    })
  unlink(pathcsv, recursive = TRUE, force = TRUE)
})


testthat::test_that("control names with name", {
  criteria <- read_criteria()
  d <- ants_L1                                               # create example datasets
  datasets <- c(d, d, d)
  ids <- c("edi.193.3", "edi.262.1", "edi.359.1")
  names(datasets) <- ids
  save_data(datasets, tempdir(), type = ".rds", name = "mydata") # .rds
  expect_true(file.exists(paste0(tempdir(), "/mydata.rds")))
  unlink(paste0(tempdir(), "/mydata.rds"), recursive = TRUE, force = TRUE)
  pathcsv <- paste0(tempdir(), "/datasets") # .csv (doesn't change names)
  dir.create(pathcsv)
  save_data(datasets, pathcsv, type = ".csv", name = "mydata")
  expect_true(!any("mydata" %in% dir(pathcsv)))
  unlink(pathcsv, recursive = TRUE, force = TRUE)
})

