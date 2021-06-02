context("create_location()")

library(ecocomDP)

# create_location() ----------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  for (i in c("df", "tbbl")) {
    if (i == "df") { # test w/data.frame
      flat <- as.data.frame(ants_L0_flat)
    } else {      # test w/tibble
      flat <- ants_L0_flat
    }
    crit <- read_criteria()
    res <- create_location(
      L0_flat = flat, 
      location_id = "location_id", 
      location_name = c("block", "plot"), 
      latitude = "latitude", 
      longitude = "longitude", 
      elevation = "elevation")
    # Has expected classes and columns
    if (i == "df") { # test w/data.frame
      expect_true(all(class(res) %in% "data.frame"))
    } else {         # test w/tibble
      expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(res)))
    }
    crit_cols <- stats::na.omit(crit$column[crit$table == "location"])
    expect_true(all(crit_cols %in% colnames(res)))
    # Is not empty
    expect_true(nrow(res) != 0)
  }
})
