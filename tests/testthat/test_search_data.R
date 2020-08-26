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
      taxa <- unname(
        unlist(
          lapply(
            x$taxa,
            function(k) {
              k$taxa
            })))
      taxa <- paste(taxa, collapse = ",")
      stringr::str_detect(taxa, taxa_search)
    })
  
  expect_true(
    all(
      r_method_1$id %in%
        unique(na.omit(names(summary_data)[unlist(r_method_2)]))))

  # num.taxa
  
  search_num_taxa <- c(0, 10)
  r_method_1 <- search_data(num.taxa = search_num_taxa)
  r_method_2 <- lapply(
    names(summary_data),
    function(id) {
      x <- summary_data[[id]]
      num_taxa <- unname(
        unlist(
          lapply(
            x$taxa,
            function(k) {
              k$unique_taxa
            })))
      any((num_taxa >= search_num_taxa[1]) & (num_taxa <= search_num_taxa[2]))
    })
  
  expect_true(
    all(
      unique(r_method_1$id) %in%
        unique(na.omit(names(summary_data)[unlist(r_method_2)]))))
  
  # years
  
  search_years <- c(10, 20)
  r_method_1 <- search_data(num.taxa = search_years)
  r_method_2 <- lapply(
    names(summary_data),
    function(id) {
      x <- summary_data[[id]]
      years <- unname(
        unlist(
          lapply(
            x$taxa,
            function(k) {
              k$unique_taxa
            })))
      any((years >= search_years[1]) & (years <= search_years[2]))
    })
  
  expect_true(
    all(
      unique(r_method_1$id) %in%
        unique(na.omit(names(summary_data)[unlist(r_method_2)]))))
  
  # sd.between.surveys
  
  search_sd_between_surveys <- c(.25, 1)
  r_method_1 <- search_data(sd.between.surveys = search_sd_between_surveys)
  r_method_2 <- lapply(
    names(summary_data),
    function(id) {
      x <- summary_data[[id]]
      sd_between_surveys <- unname(
        unlist(
          lapply(
            x$std_dev_interval_betw_years,
            function(k) {
              k
            })))
      any((sd_between_surveys >= search_sd_between_surveys[1]) & 
            (sd_between_surveys <= search_sd_between_surveys[2]))
    })
  
  expect_true(
    all(
      unique(r_method_1$id) %in%
        unique(na.omit(names(summary_data)[unlist(r_method_2)]))))
  
  # geographic.area
  
  # boolean.operator
  
})
