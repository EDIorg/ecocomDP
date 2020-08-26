context("Search data")

library(ecocomDP)

# NULL input ------------------------------------------------------------------

testthat::test_that("search_data() with NULL input", {
  
  # Parameterize
  
  r <- search_data()
  r_edi<- r[
    stringr::str_detect(
      r$id,
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)"), ]
  r_neon<- r[
    stringr::str_detect(
      r$id,
      "^DP.\\.[:digit:]+\\.[:digit:]+"), ]
  
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
  
  # sites - Site search available for NEON only
  
  expect_true(
    all(
      is.na(r_edi$sites)))
  
  expect_true(
    all(
      stringr::str_detect(r_neon$sites, "(.+,)+")))
  
  # url - Present for all datasets
  
  expect_true(
    all(
      stringr::str_detect(r$url, "https:.+")))

})

# non-NULL input --------------------------------------------------------------

testthat::test_that("search_data() with non-NULL input", {
  
  # Parameterize
  
  summary_data <- c(summary_data_edi, summary_data_neon)
  
  # text - Search titles
  
  r <- search_data(
    text = "Small mammal box trapping")
  expect_equal(nrow(r), 1)
  
  # text - Search abstract
  
  r <- search_data(
    text = "South Carolina")
  expect_true(nrow(r) < nrow(search_data()))
  
  # taxa
  
  taxa_search <- "Chordata"
  r_method_1 <- search_data(taxa = taxa_search)
  r_method_2 <- lapply(
    names(summary_data),
    function(id) {
      x <- summary_data[[id]]
      if (stringr::str_detect(
        id, "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
        # EDI
        stringr::str_detect(
          x$taxa$geographic_bounds$taxa, 
          taxa_search)
      } else if (stringr::str_detect(
        id, "(^DP.\\.[:digit:]+\\.[:digit:]+)")) {
        # NEON
        taxa <- unname(
          unlist(
            lapply(
              x$taxa,
              function(k) {
                k$taxa
              })))
        taxa <- paste(taxa, collapse = ",")
        stringr::str_detect(taxa, taxa_search)
      }
    })
  na.omit(names(summary_data)[unlist(r_method_2)])
  
  expect_true(
    r_method_1$id,
    na.omit(names(summary_data)[unlist(r_method_2)])
  )
  
  # ?
  test <- summary_data[["DP1.10022.001"]]
  test$taxa$ABBY$taxa
  
  
  stringr::str_view(
    x$taxa$geographic_bounds$taxa, 
    taxa_search
  )
  
  # num.taxa
  
  # years
  
  # sd.between.surveys
  
  # geographic.area
  
  # boolean.operator
  
})
