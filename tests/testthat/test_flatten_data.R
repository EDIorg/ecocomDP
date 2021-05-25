context("flatten_data()")

library(ecocomDP)

#  ------------------------------------------------------

testthat::test_that("Round trip (from L0 wide to L1 wide)", {
  
  # Parameterize
  crit <- read_criteria()
  L0_wide <- ants_L0_wide
  L1_flat <- ecocomDP::flatten_data(ants_L1[[1]]$tables)
  
  # Compare L0 wide and L1 flat - The column names and values of the L0 wide and L1 flattened tables should match, with a few exceptions:
  # 1.) Locations - All locations above the level at which observations were made were lost during flattening. This information cannot be retrieved without implementing a convention to use during the L0 to L1 conversion process. 
  # 2.) Relatedly, the L0 column listing location names, at the observation level, is now "location_name".
  # 3.) Primary keys, row identifiers, of the ancillary tables are now present.
  
  cols_L0_wide <- colnames(L0_wide)
  cols_L1_flat <- colnames(L1_flat)
  
  # Create set of L0 wide columns to test
  loc_cols <- c("block", "plot") # Location columns
  # dataset_summary_cols <- c("original_package_id",
  #                           "length_of_survey_years",
  #                           "number_of_years_sampled",
  #                           "std_dev_interval_betw_years",
  #                           "geo_extent_bounding_box_m2",
  #                           "max_num_taxa")
  # TODO: Rename plot column
  
  # Create set of L1 flat columns to test
  
  
  # Columns originally in L0 wide but now missing from L1 flat
  cols_missing_from_L1 <- base::setdiff(cols_L0_wide, cols_L1_flat)
  
  
  ancil_key_cols <- c("observation_ancillary_id", # Primary keys of ancillary tables (these were generated during L1 creation)
                      "location_ancillary_id", 
                      "taxon_ancillary_id", 
                      "variable_mapping_id")
  
  cols_not_expected_to_return_to_L1_flat <- c(location_cols, dataset_summary_cols)
  cols_missing_from_L1 <- cols_missing_from_L1[!(cols_missing_from_L1 %in% cols_not_expected_to_return_to_L1_flat)]
  
  expect_true(length(cols_missing_from_L1) == 0) # All expected columns of L1 flat are accounted present
  
  # TODO: Not only should these columns not be expected, but they should be absent (i.e. returning unnecessary information)
  # TEST: None of this set should be present
  
  # TODO: Columns in L1 flat that were never in L0 wide.
  # TEST: Anything in L1 flat that is outside the set of columns from L0 wide + ancillary_pkeys is an outlier and should be excluded
  
  
  # Column "A" in L0 wide has the same values in column "A" of L1 flat
  invisible(
    lapply(
      colnames(L0_wide),
      function(L0_col) {
        # same number of values
        expect_true(length(a) == length(b))
        # same values, but possibly in a different order
        expect_true(identical(sort(df$a), sort(df$b)))
      }))
  
})

