context("Search data")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

r <- search_data()
r_edi<- r[
  stringr::str_detect(
    r$id,
    "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)"), ]
r_neon<- r[
  stringr::str_detect(
    r$id,
    "^DP.\\.[:digit:]+\\.[:digit:]+"), ]

# search_data() NULL input ----------------------------------------------------

testthat::test_that("search_data() with NULL input", {
  
  # class - Is data frame
  
  expect_true(
    all(class(r) %in% c("data.table", "data.frame")))
  
  # column names - Are from the expected set
  
  expect_true(
    all(colnames(r) %in% 
          c("source", "id", "title", "description", "abstract", "years",
            "sampling_interval", "sites", "url")))
  
  # nrows - There are many ecocomDP datasets
  
  expect_true(nrow(r) > 70)
  
  # source - Only EDI and NEON are currently supported
  
  expect_true(
    all(unique(r$source) %in% c("EDI", "NEON")))
  
  # id - Only EDI and NEON identifiers are currently supported
  
  expect_true(
    all(
      stringr::str_detect(
        r$id,
        "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^DP.\\.[:digit:]+\\.[:digit:]+)")))
  
  # title - All datasets have a title
  
  expect_true(
    all(!is.na(r$title)))
  
  # description - EDI doesn't have a description
  
  expect_true(
    all(is.na(r_edi$description)))
  
  # abstract - All datasets have an abstract
  
  expect_true(
    all(!is.na(r$abstract)))
  
  # years - EDI has single value, NEON has range
  
  expect_true(
    all(
      !is.na(
        as.integer(r_edi$years))))
  
  expect_true(
    all(
      stringr::str_detect(
        r_neon$years,
        "min = .+, max = .+"
      )))
  
  # sampling_interval - EDI is numeric, NEON has range
  
  expect_true(
    sum(
      is.na(
        as.numeric(r_edi$sampling_interval))) < nrow(r_edi))
  
  expect_true(
    all(
      stringr::str_detect(
        r_neon$sampling_interval,
        "min = .+, max = .+"
      )))
  
  # sites - 
  
  # url - 

})
