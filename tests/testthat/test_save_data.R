context("save_data()")

library(ecocomDP)

testthat::test_that("Objects are saved to fomat and have expected structure", {
  criteria <- read_criteria()
  id <- "edi.193.3"
  d <- try(suppressWarnings(read_data(id = id)), silent = TRUE)
  id <- names(d)
  fname <- paste0(tempdir(),"/", id, ".rds")
  dname <- paste0(tempdir(),"/", id)
  unlink(fname, recursive = TRUE, force = TRUE)
  unlink(dname, recursive = TRUE, force = TRUE)
  # Format & structure
  if (class(d) != "try-error") {
    save_data(d, tempdir(), file.type = ".rds", file.name = id) # .rds
    expect_true(file.exists(fname))                             # was created
    unlink(fname, recursive = TRUE, force = TRUE)
    save_data(d, tempdir(), file.type = ".csv", file.name = id) # .csv
    expect_true(dir.exists(dname))                              # was created
    fnames <- tools::file_path_sans_ext(list.files(dname))      # dir has tables
    expect_true(all(fnames %in% unique(criteria$table)))
    fext <- unique(tools::file_ext(list.files(dname)))          # has file extension
    expect_equal(fext, "csv")
    unlink(dname, recursive = TRUE, force = TRUE)
  }
  # Name control (default naming)
  if (class(d) != "try-error") {
    save_data(d, tempdir(), file.type = ".rds")
    expect_true(file.exists(paste0(tempdir(), "/d.rds")))                # .rds
    unlink(paste0(tempdir(), "/d.rds"), recursive = TRUE, force = TRUE)
    save_data(d, tempdir(), file.type = ".csv")
    expect_true(dir.exists(paste0(tempdir(), "/d")))                     # .csv
    unlink(paste0(tempdir(), "/d"), recursive = TRUE, force = TRUE)
  }
})

