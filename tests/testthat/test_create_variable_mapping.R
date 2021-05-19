context("create_variable_mapping()")

library(ecocomDP)

# create_variable_mapping() ---------------------------------------------------

testthat::test_that("Standard L1 column inputs", {
  crit <- read_criteria()
  wide <- ants_L0_wide
  
  # Create input tables
  observation <- create_observation(
    L0_wide = wide, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "abundance",
    unit = "unit_abundance")

  observation_ancillary <- create_observation_ancillary(
    L0_wide = wide,
    observation_id = "observation_id", 
    variable_name = c("trap.type", "trap.num", "moose.cage"))
  
  location_ancillary <- create_location_ancillary(
    L0_wide = wide,
    location_id = "location_id",
    variable_name = "treatment")
  
  taxon_ancillary <- create_taxon_ancillary(
    L0_wide = wide,
    taxon_id = "taxon_id",
    variable_name = c(
      "subfamily", "hl", "rel", "rll", "colony.size", 
      "feeding.preference", "nest.substrate", "primary.habitat", 
      "secondary.habitat", "seed.disperser", "slavemaker.sp", 
      "behavior", "biogeographic.affinity", "source"),
    unit = c("unit_hl", "unit_rel", "unit_rll"))
  
  # Create variable mapping
  res <- create_variable_mapping(
    observation = observation,
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary, 
    taxon_ancillary = taxon_ancillary)
  
  # Is data.frame with expected columns
  expect_s3_class(res, "data.frame")
  crit_cols <- na.omit(crit$column[crit$table == "variable_mapping"])
  expect_true(all(crit_cols %in% colnames(res)))
  # Is not empty
  expect_true(nrow(res) != 0)
})
