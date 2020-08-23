# Tests are organized around validation check functions (e.g. all tests listed 
# under validate_column_names() are relevant to that check).

context("validate_ecocomDP()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

# Use the example dataset for testing

test_data <- read_from_files(system.file("/data", package = "ecocomDP"))[[1]]$tables
test_data_file_names <- dir(
  system.file("/data", package = "ecocomDP"), pattern = "Ant_Assemblages_")

# Load validation criteria for tables and columns

criteria <- data.table::fread(
  system.file('validation_criteria.txt', package = 'ecocomDP'))

# validate_table_names() ------------------------------------------------------

testthat::test_that("validate_table_names()", {
  
  file.copy(
    system.file("/data", package = "ecocomDP"),
    tempdir(),
    recursive = TRUE)
  
  # Valid tables result in a corresponding list of table names
  
  r <- validate_table_names(paste0(tempdir(), "/data"))
  
  expect_equal(
    r, 
    dir(
      system.file("/data", package = "ecocomDP"), 
      pattern = "Ant_Assemblages_"))
  
  # More than one study name results in a character string
  
  file.rename(
    from = dir(
      paste0(tempdir(), "/data"), 
      pattern = "dataset_summary", 
      full.names = TRUE),
    to = paste0(tempdir(), "/data/Ant_Assemblages_dataset_summary.csv"))
  
  file.rename(
    from = dir(
      paste0(tempdir(), "/data"), 
      pattern = "observation_ancillary", 
      full.names = TRUE),
    to = paste0(tempdir(), "/data/Ant_observation_ancillary.csv"))
  
  expect_error(
    validate_table_names(paste0(tempdir(), "/data")),
    regexp = "More than one study name found. There can be only one. Unique ")
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/data"), 
    recursive = TRUE, 
    force = TRUE)
  
})

# validate_table_presence() ---------------------------------------------------

testthat::test_that("validate_table_presence()", {
  
  d <- test_data
  
  # Return message when all required tables are present.
  
  expect_null(validate_table_presence(d))
  
  # Return character string when required tables are missing.
  
  required_tables <- criteria$table[is.na(criteria$column) & criteria$required]
  for (i in required_tables) {
    d <- test_data
    d[i] <- NULL
    r <- validate_table_presence(d)
    expect_true(
      stringr::str_detect(r, "Missing required table: .+"))
  }
  
})

# validate_column_names() -----------------------------------------------------

testthat::test_that("validate_column_names()", {
  
  d <- test_data
  
  # Return message when column spelling is correct.
  
  expect_null(validate_column_names(d))
  
  # Return character string when column spelling is incorrect.
  
  for (table in names(d)) {
    d <- test_data
    invalid_names <- names(d[[table]])
    invalid_names[1:2] <- c("invalid_colname_1", "invalid_colname_2")
    names(d[[table]]) <- invalid_names
    r <- validate_column_names(d)
    expect_true(
      stringr::str_detect(
        r, 
        "The .+ table has these invalid column names: .+"))
  }
  
})

# validate_column_presence() --------------------------------------------------

testthat::test_that("validate_column_presence()", {
  
  d <- test_data
  
  # Return message when required columns are present.
  
  expect_null(validate_column_presence(d))
  
  # Return character string when required columns are missing.
  
  for (table in names(d)) {
    required_columns <- criteria$column[
      !is.na(criteria$column) & criteria$table == table & criteria$required]
    for (column in required_columns) {
      d <- test_data
      d[[table]][[column]] <- NULL
      r <- validate_column_presence(d)
      expect_true(
        stringr::str_detect(
          r, 
          "The .+ table is missing these required columns: .+"))
    }
  }
  
})

# validate_datetime() ---------------------------------------------------------

testthat::test_that("validate_datetime()", {
  
  d <- test_data
  
  # Return message when datetime formats are valid.
  
  expect_null(validate_datetime(d))
  
  # Return character string when datetime formats are invalid.
  
  for (table in names(d)) {
    d <- test_data
    datetime_columns <- criteria$column[
      !is.na(criteria$column) & 
        criteria$table == table & 
        criteria$class == "Date"]
    if (length(datetime_columns) != 0) {
      for (column in datetime_columns) {
        if (!all(is.na(d[[table]][[column]]))) {
          d[[table]][[column]][1:2] <- c("01/02/2003", "01/02/2003")
          r <- validate_datetime(d)
          expect_true(
            stringr::str_detect(
              r, 
              "The .+ table has unsupported datetime formats at rows:"))
        }
      }
    }
  }
  
})

# validate_column_classes() ---------------------------------------------------

testthat::test_that("validate_column_classes()", {
  
  d <- test_data
  
  # Return message when column classes are valid.
  
  expect_null(validate_column_classes(d))
  
  # Return character string when column classes are invalid.
  
  for (table in names(d)) {
    d <- test_data
    table_columns <- colnames(d[[table]])
    for (column in table_columns) {
      d <- test_data
      d[[table]][[column]] <- as.logical(d[[table]][[column]])
      r <- validate_column_classes(d)
      expect_true(
        stringr::str_detect(
          r, 
          "The column .+ in the table .+ has a class of .+ but a class of .+"))
    }
  }
  
})

# validate_primary_keys() -----------------------------------------------------

testthat::test_that("validate_primary_keys()", {
  
  d <- test_data
  
  # Unique primary keys result in message.
  
  expect_null(validate_primary_keys(d))
  
  # Non-unique primary keys result in character string.
  
  d$dataset_summary <- NULL
  for (table in names(d)) {
    d <- test_data
    primary_key_columns <- criteria$column[
      !is.na(criteria$column) & 
        criteria$table == table & 
        criteria$primary_key]
    if (length(primary_key_columns) != 0) {
      for (column in primary_key_columns) {
        d[[table]][[column]][2] <- d[[table]][[column]][1]
        r <- validate_primary_keys(d)
        expect_true(
          stringr::str_detect(
            r, 
            "The .+ table contains non-unique primary keys in the column "))
      }
    }
  }

})

# validate_composite_keys() ---------------------------------------------------

testthat::test_that("validate_composite_keys()", {
  
  d <- test_data
  
  # Unique composite keys result in message.
  
  expect_null(validate_composite_keys(d))
  
  # Non-unique composite keys result in character string.
  
  for (table in names(d)) {
    d <- test_data
    composite_key_columns <- criteria$column[
      !is.na(criteria$column) & 
        criteria$table == table & 
        criteria$composite_key]
    if (length(composite_key_columns) != 0) {
      for (column in composite_key_columns) {
        d[[table]][[column]][2] <- d[[table]][[column]][1]
      }
      r <- validate_composite_keys(d)
      expect_true(
        stringr::str_detect(
          r, 
          "The composite keys composed of the columns .+"))
    }
  }
  
})

# validate_referential_integrity() --------------------------------------------

testthat::test_that("validate_referential_integrity()", {
  
  d <- test_data
  
  # Valid referential integrity results in message.
  
  expect_null(validate_referential_integrity(d))
  
  # Invalid referential integrity results in character string.
  
  for (table in names(d)) {
    d <- test_data
    primary_key <- criteria$column[
      (criteria$table == table) & 
        !is.na(criteria$column) & 
        (criteria$primary_key == TRUE)]
    primary_key_data <- na.omit(d[[table]][[primary_key]])
    foreign_key_table <- criteria$table[
      !is.na(criteria$column) & 
        (criteria$column == primary_key)]
    for (fk_table in foreign_key_table) {
      if (fk_table != table) {
        d <- test_data
        d[[fk_table]][[primary_key]][1] <- "invalid_foreign_key"
        r <- validate_referential_integrity(d)
        expect_true(
          stringr::str_detect(
            r, 
            "The .+ table has these foreign keys without a primary key"))
      } else if (table != "location") {
        if (fk_table == "location") {
          d[[fk_table]][["parent_location_id"]][2] <- "invalid_foreign_key"
          expect_true(
            stringr::str_detect(
              r, 
              "The .+ table has these foreign keys without a primary key"))
        }
      }
    }
  }
  
})

# validate_ecocomDP() ---------------------------------------------------------

testthat::test_that("validate_ecocomDP", {
  
  d <- test_data
  
  # If multiple validation issues, then report all issues with a warning or 
  # error.
  
  # Create issue for validate_table_presence()
  d$dataset_summary <- NULL
  # Create issue for validate_column_names()
  names(d$taxon_ancillary) <- c(
    "taxon_ancillary_id", "taxon_id", "datetime", "variable_name", "value", 
    "invalid_col_name")
  # Create issue for validate_column_presence()
  d$taxon$taxon_name <- NULL
  # Create issue for validate_datetime()
  d$location_ancillary$datetime[1] <- "08/22/2020"
  # Create issue for validate_column_classes()
  d$location$latitude <- as.character(d$location$latitude)
  # Create issue for validate_primary_keys()
  d$taxon$taxon_id[2] <- d$taxon$taxon_id[1]
  # Create issue for validate_composite_keys()
  d$location_ancillary$location_id[3] <- d$location_ancillary$location_id[2]
  d$location_ancillary$datetime[3] <- d$location_ancillary$datetime[2]
  d$location_ancillary$variable_name[3] <- d$location_ancillary$variable_name[2]
  # Create issue for validate_referential_integrity()
  d$observation$event_id[1] <- "invalid_foreign_key"
  d$observation$package_id[1] <- "invalid_foreign_key"
  d$observation$location_id[1] <- "invalid_foreign_key"
  d$observation$taxon_id[1] <- "invalid_foreign_key"
  
  issues <- validate_ecocomDP(data.list = d)
  expect_equal(length(issues), 11)
  expect_true(is.character(issues))
  
})