# Tests are organized around validation check functions (e.g. all tests listed 
# under validate_column_names() are relevant to that check).

context("validate_ecocomDP()")

library(ecocomDP)

# Parameterize ----------------------------------------------------------------

# Use the example dataset for testing

test_data <- read_from_files(system.file("/data", package = "ecocomDP"))
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
  
  # More than one study name results in an error
  
  file.rename(
    from = dir(
      paste0(tempdir(), "/data"), 
      pattern = "dataset_summary", 
      full.names = TRUE),
    to = paste0(tempdir(), "/data/Ant_Assemblages_dataset_summary.csv"))
  
  expect_error(
    validate_table_names(paste0(tempdir(), "/data")),
    regexp = "More than one study name found")
  
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
  
  expect_message(validate_table_presence(d), regexp = "Required tables")
  
  # Error when required tables are missing.
  
  required_tables <- criteria$table[is.na(criteria$column) & criteria$required]
  d[required_tables] <- NULL
  
  expect_error(
    validate_table_presence(d),
    regexp = "These required tables are missing: .+")
  
})

# validate_column_names() -----------------------------------------------------

testthat::test_that("validate_column_names()", {
  
  d <- test_data
  
  # Return message when column spelling is correct.
  
  expect_message(
    validate_column_names(d),
    regexp = "Column names")
  
  # Return error when column spelling is incorrect.
  
  invalid_names <- names(d$location)
  invalid_names[1:2] <- c("col1", "col2")
  names(d$location) <- invalid_names
  
  expect_error(
    validate_column_names(d),
    regexp = "The .+ table has these invalid column names: .+")
  
})

# validate_column_presence() --------------------------------------------------

testthat::test_that("validate_column_presence()", {
  
  d <- test_data
  
  # Return message when required columns are present.
  
  expect_message(
    validate_column_presence(d),
    regexp = "Required columns")
  
  # Return error when required columns are missing.
  
  required_columns <- criteria$column[
    !is.na(criteria$column) & criteria$table == "observation" & criteria$required]
  for (i in 1:length(required_columns)) {
    d[["observation"]][[required_columns[i]]] <- NULL
  }
  
  expect_error(
    validate_column_presence(d),
    regexp = "The .+ table is missing these required columns")
  
})

# validate_datetime() ---------------------------------------------------------

testthat::test_that("validate_datetime()", {
  
  d <- test_data
  
  # Return message when datetime formats are valid.
  
  validate_datetime(d)
  
  # Return error when datetime formats are invalid.
  
})

# validate_column_classes() ---------------------------------------------------

testthat::test_that("validate_column_classes()", {
  
  
  
})

# validate_primary_keys() -----------------------------------------------------

testthat::test_that("validate_primary_keys()", {
  
  
  
})

# validate_composite_keys() ---------------------------------------------------

testthat::test_that("validate_composite_keys()", {
  
  
  
})

# validate_referential_integrity() --------------------------------------------

testthat::test_that("validate_referential_integrity()", {
  
  
  
})

