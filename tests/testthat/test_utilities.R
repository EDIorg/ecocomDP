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

# parse_datetime_frmt_from_vals() ---------------------------------------------

testthat::test_that("parse_datetime_frmt_from_vals()", {
  expect_equal(
    parse_datetime_frmt_from_vals(vals = "2021-07-11 13:20:00"),
    "YYYY-MM-DD hh:mm:ss")
  expect_equal(
    parse_datetime_frmt_from_vals(vals = "2021-07-11 13:20"),
    "YYYY-MM-DD hh:mm")
  expect_equal(
    parse_datetime_frmt_from_vals(vals = "2021-07-11 13"),
    "YYYY-MM-DD hh")
  expect_equal(
    parse_datetime_frmt_from_vals(vals = "2021-07-11"),
    "YYYY-MM-DD")
  expect_equal(
    parse_datetime_frmt_from_vals(vals = "2021"),
    "YYYY")
  expect_null(
    parse_datetime_frmt_from_vals(vals = NA))
  expect_null(
    parse_datetime_frmt_from_vals(vals = NA_character_))
  suppressWarnings(
    expect_equal(
      parse_datetime_frmt_from_vals(vals = c("2021-07-11", "2021", NA_character_)),
      "YYYY-MM-DD"))
  expect_warning(
    parse_datetime_frmt_from_vals(vals = c("2021-07-11", "2021", NA_character_)),
    regexp = "The best match .+ may not describe all datetimes")
})

# parse_datetime_from_frmt() --------------------------------------------------

testthat::test_that("parse_datetime_from_frmt()", {
  mytbl <- "observation"
  expect_equal(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = "2021-07-11 13:20:00",
                             frmt = "YYYY-MM-DD hh:mm:ss"),
    lubridate::ymd_hms("2021-07-11 13:20:00"))
  expect_equal(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = "2021-07-11 13:20",
                             frmt = "YYYY-MM-DD hh:mm"),
    lubridate::ymd_hm("2021-07-11 13:20"))
  expect_equal(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = "2021-07-11 13",
                             frmt = "YYYY-MM-DD hh"),
    lubridate::ymd_h("2021-07-11 13"))
  expect_equal(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = "2021-07-11",
                             frmt = "YYYY-MM-DD"),
    lubridate::ymd("2021-07-11"))
  expect_equal(
    suppressWarnings(parse_datetime_from_frmt(tbl = mytbl,
                                              vals = "2021",
                                              frmt = "YYYY")),
    lubridate::ymd_h("2021-01-01 00"))
  expect_warning(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = "2021",
                             frmt = "YYYY"),
    regexp = "Input datetimes have format \'YYYY\' but are being returned as \'YYYY-MM-DD\'.")
  expect_warning(
    parse_datetime_from_frmt(tbl = mytbl,
                             vals = c("2021-07-11", "2021-07-11 13:20"),
                             frmt = "YYYY-MM-DD"),
    regexp = "1 observation datetime strings failed to parse")
})

# write_tables() --------------------------------------------------------------

testthat::test_that("write_tables()", {
  # Parameterize
  mypath <- paste0(tempdir(), "/data")
  unlink(mypath, recursive = TRUE)
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
