context("plot_data()")

library(ecocomDP)

testthat::test_that("Plots are written to file", {
  id <- "edi.193.3"
  d <- try(suppressWarnings(read_data(id = id)), silent = TRUE)
  if (class(d) != "try-error") {
    # only one data object at a time
    plot_data(d, type = "community", path = )
    
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
})

