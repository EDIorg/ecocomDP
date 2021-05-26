context("flatten_data()")

library(ecocomDP)

#  ------------------------------------------------------

testthat::test_that("Round trip (from L0 flat to L1 flat)", {
  
  # Parameterize
  crit <- read_criteria()
  L0_flat <- ants_L0_flat
  L1_flat <- ecocomDP::flatten_data(ants_L1[[1]]$tables)
  
  # Compare L0 flat and L1 flat - The column names and values of the L0 flat and L1 flattened tables should match, with a few exceptions:
  # 1.) Locations - All locations above the level at which observations were made were lost during flattening. This information cannot be retrieved without implementing a convention to use during the L0 to L1 conversion process. 
  # 2.) Relatedly, the L0 column listing location names, at the observation level, is now "location_name".
  # 3.) Primary keys, row identifiers, of the ancillary tables are now present.
  
  # Adjust L0 flat to our expectations
  L0_flat <- L0_flat %>% 
    dplyr::select(-block) %>%           # A higher level location lost when flattened
    dplyr::select(-author) %>%           # Columns of NA are dropped when flattened
    dplyr::rename(location_name = plot) # Becomes "location_name" when flattened
  
  # TEST: All L0 flat columns (with above exceptions) should be in L1 flat
  cols_missing_from_L1 <- base::setdiff(colnames(L0_flat), colnames(L1_flat))
  expect_true(length(cols_missing_from_L1) == 0)
  
  # TEST: All L1 flat columns should be in L0 flat
  cols_missing_from_L0 <- base::setdiff(colnames(L1_flat), colnames(L0_flat))
  expect_true(length(cols_missing_from_L0) == 0)
  
  # TEST: Column classifications should be equivalent
  L0_classes <- unlist(lapply(L0_flat, class))
  L1_classes <- unlist(lapply(L1_flat, class))
  
  # TODO TEST: Missing non-required columns isn't an issue
  
  # TODO TEST: Observation "A" in L0 flat has the same values in observation "A" of L1 flat
  invisible(
    lapply(
      colnames(L0_flat),
      function(L0_col) {
        # same number of values
        expect_true(length(a) == length(b))
        # same values, but possibly in a different order
        expect_true(identical(sort(df$a), sort(df$b)))
      }))
  
})

