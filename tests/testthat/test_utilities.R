context("utilities.R")

library(ecocomDP)

testthat::test_that("Primary method of detect_delimiter()", {
  # The primary method of this function relies on the verbose output of data.table::fread(). Therefore any changes to the output structure will compromise the return value and should be corrected ASAP. The urgency of a fix is lessened by a secondary approach that works well.
  f <- system.file("/ecocomDP/attributes_observation.txt", 
                   package = "ecocomDP")
  msg <- capture.output(data.table::fread(f, verbose = TRUE) %>% {NULL}) # This is the method in question
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
