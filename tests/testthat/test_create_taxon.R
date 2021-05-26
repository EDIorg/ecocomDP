context("create_taxon()")

library(ecocomDP)

# create_taxon() --------------------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  flat <- ants_L0_flat
  res <- create_taxon(
    L0_flat = flat, 
    taxon_id = "taxon_id", 
    taxon_rank = "taxon_rank", 
    taxon_name = "taxon_name", 
    authority_system = "authority_system", 
    authority_taxon_id = "authority_taxon_id")
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "taxon"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
