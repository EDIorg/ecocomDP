context("flatten_data()")

library(ecocomDP)

#  ------------------------------------------------------

testthat::test_that("Round trip (from L0 wide to L1 wide)", {
  
  # Parameterize
  L0_wide <- ants_L0_wide
  d <- ants_L1
  my_list_of_tables <- d[[1]]$tables
  L1_flat <- ecocomDP::flatten_data(my_list_of_tables)
  
  # Compare column names - Column names of the L0 wide and L1 flattened should match, with a few exceptions:
  # 1.) Locations - All locations above the level at which observations were made are lost. The L0 column listing locations is now named "location_name". Note: We could implement a recommended practice and supporting code that would enable return of all the original L0 location columns, but let's save this for the next release.
  # 2.) Dataset summary - Summary information included in the L0, to facilitate parsing to L1 tables, is not valuable enough to include in the L1 flat. This summary info can always be referenced from the L1.
  # 3.) Identifiers of the core L1 tables (primary keys) are now present. Only some of these were in wide.
  
  cols_L0_wide <- colnames(L0_wide)
  cols_L1_flat <- colnames(L1_flat)
  
  # Columns missing from L1 flat that were in the original L0 wide
  cols_missing_from_L1 <- cols_L0_wide[!(cols_L0_wide %in% cols_L1_flat)]
  
  # Remove columns that we've decided should not be present and there for shouldn't be considered in the evalutation
  location_cols <- c("block", "plot") # Location related columns (these can't be returned)
  dataset_summary_cols <- c("original_package_id", # dataset_summary cols are summaries not very valuable
                            "length_of_survey_years",
                            "number_of_years_sampled",
                            "std_dev_interval_betw_years",
                            "geo_extent_bounding_box_m2",
                            "max_num_taxa")
  cols_not_expected_to_return_to_L1_flat <- c(location_cols, dataset_summary_cols)
  cols_missing_from_L1 <- cols_missing_from_L1[!(cols_missing_from_L1 %in% cols_not_expected_to_return_to_L1_flat)]
  
  
  # TODO: Colin
  # 1.) Test: Values of matching L0 wide and L1 wide columns should be equal (Colin will write a test for this)
  if (length(cols_missing_from_L1)) {
    stop("These columns were in the L0 wide and expected in the L1 flat but are missing from L1 flat: ", 
         paste(cols_missing_from_L1, collapse = ", "), call. = FALSE)
  }
  
  # TODO: Colin
  # 2.) Columns in L1 flat that were never in L0 wide. This is just info for the flatten_data() function metadata. I.e. info the user would want to know.
  
  # TODO: Colin
  # 3.) Columns in L1 flat that shouldn't be there. Not sure how to do this.
  
})

