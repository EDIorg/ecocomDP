context("create_taxon_ancillary()")

library(ecocomDP)

# create_taxon_ancillary() ----------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  wide <- ants_L0_wide
  res <- create_taxon_ancillary(
    L0_wide = wide,
    taxon_id = "taxon_id",
    variable_name = c(
      "subfamily", "hl", "rel", "rll", "colony.size", 
      "feeding.preference", "nest.substrate", "primary.habitat", 
      "secondary.habitat", "seed.disperser", "slavemaker.sp", 
      "behavior", "biogeographic.affinity", "source"),
    unit = c("unit_hl", "unit_rel", "unit_rll"))
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "taxon_ancillary"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
