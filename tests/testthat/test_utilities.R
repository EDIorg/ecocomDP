context("utilities.R")

library(ecocomDP)

# detect_delimiter() - Primary method -----------------------------------------

testthat::test_that("Primary method of detect_delimiter()", {
  # The primary method of this function relies on the verbose output of data.table::fread(). Therefore any changes to the output structure will compromise the return value and should be corrected ASAP. The urgency of a fix is lessened by a secondary approach that works well.
  f <- system.file("extdata", "/ecocomDP/attributes_observation.txt", package = "ecocomDP")
  msg <- utils::capture.output(data.table::fread(f, verbose = TRUE) %>% {NULL}) # This is the method in question
  seps <- stringr::str_extract_all(msg, "(?<=(sep=')).+(?='[:blank:])")
  sep <- unique(unlist(seps))
  expect_length(sep, 1)
})

# is_edi(), is_neon() ---------------------------------------------------------

testthat::test_that("Identify source from id string", {
  expect_true(is_edi("edi.100.1"))
  expect_false(is_edi("edi.100"))
  expect_true(is_edi("knb-lter-ntl.100.1"))
  expect_false(is_edi("knb-lter-ntl.100"))
  expect_true(is_neon("neon.ecocomdp.20166.001.001"))
  expect_false(is_neon("ecocomdp.20166.001.001"))
})

# write_tables() --------------------------------------------------------------

testthat::test_that("write_tables()", {
  # Parameterize
  mypath <- paste0(tempdir(), "/data")
  dir.create(mypath)
  flat <- ants_L0_flat
  observation <- create_observation(
    L0_flat = flat, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "variable_name",
    value = "value",
    unit = "unit")
  observation_ancillary <- create_observation_ancillary(
    L0_flat = flat,
    observation_id = "observation_id", 
    variable_name = c("trap.type", "trap.num", "moose.cage"))
  # Write tables to file
  write_tables(
    path = mypath, 
    observation = observation, 
    observation_ancillary = observation_ancillary)
  # Test
  expect_true(file.exists(paste0(mypath, "/observation.csv")))
  expect_true(file.exists(paste0(mypath, "/observation_ancillary.csv")))
  # Clean up
  unlink(mypath, recursive = TRUE)
})
