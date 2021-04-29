# Tests are organized around validation check functions (e.g. all tests listed 
# under validate_column_names() are relevant to that check).

library(ecocomDP)

context("validate_data()")

# Parameterize ----------------------------------------------------------------

# Use the example dataset for testing

test_data <- read_from_files(system.file("/data", package = "ecocomDP"))[[1]]$tables
test_data_file_names <- dir(
  system.file("/data", package = "ecocomDP"), pattern = "Ant_Assemblages_")

# Load validation criteria for tables and columns

criteria <- data.table::fread(
  system.file('validation_criteria.txt', package = 'ecocomDP'))

# validate_table_names() ------------------------------------------------------

# FIXME: This test fails devtools::check() but passes devtools::test()
# testthat::test_that("validate_table_names()", {
#   
#   file.copy(
#     system.file("/data", package = "ecocomDP"),
#     tempdir(),
#     recursive = TRUE)
#   
#   # Valid tables result in a corresponding list of table names
#   
#   r <- validate_table_names(paste0(tempdir(), "/data"))
#   
#   expect_equal(
#     r, 
#     dir(
#       system.file("/data", package = "ecocomDP"), 
#       pattern = "Ant_Assemblages_"))
#   
#   # More than one study name results in a character string
#   
#   file.rename(
#     from = paste0(
#       tempdir(), 
#       "/data/Ant_Assemblages_in_Hemlock_Removal_Experiment_dataset_summary.csv"),
#     to = paste0(
#       tempdir(), 
#       "/data/Ant_Assemblages_dataset_summary.csv"))
#   
#   expect_error(
#     validate_table_names(paste0(tempdir(), "/data")),
#     regexp = "File names. More than one study name found. Unique study")
#   
#   # Clean up
#   
#   unlink(
#     paste0(tempdir(), "/data"), 
#     recursive = TRUE, 
#     force = TRUE)
#   
# })

# validate_table_presence() ---------------------------------------------------

testthat::test_that("validate_table_presence()", {
  
  d <- test_data
  
  # Return message when all required tables are present.
  
  expect_message(
    validate_table_presence(d),
    regexp = "Required tables")
  
  # Return character string when required tables are missing.
  
  required_tables <- criteria$table[is.na(criteria$column) & criteria$required]
  for (i in required_tables) {
    d <- test_data
    d[i] <- NULL
    r <- validate_table_presence(d)
    expect_true(
      stringr::str_detect(r, "Required table. Missing required table: .+"))
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
        "Column names. The .+ table has these invalid column names:"))
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
          "Required columns. The .+ table is missing these required"))
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
              "Datetime format. The .+ table has unsupported"))
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
          "Column classes. The column .+ in the table .+ has a class"))
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
            "Primary keys. The .+ table contains non-unique primary keys in"))
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
          "Composite keys. The composite keys composed of the columns .+"))
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
            "Referential integrity. The .+ table has these foreign keys"))
      } else if (table != "location") {
        if (fk_table == "location") {
          d[[fk_table]][["parent_location_id"]][2] <- "invalid_foreign_key"
          expect_true(
            stringr::str_detect(
              r, 
              "Referential integrity. The .+ table has these foreign keys"))
        }
      }
    }
  }
  
})

# validate_latitude_longitude_format() ----------------------------------------

testthat::test_that("validate_latitude_longitude_format()", {
  
  d <- test_data
  
  # Valid latitude and longitude results in message.
  
  expect_null(validate_latitude_longitude_format(d))
  
  # Invalid latitude and longitude results in character string.
  
  for (i in c("latitude", "longitude")) {
    d <- test_data
    d$location[[i]][1:2] <- "invalid_coordinate"
    expect_true(
      stringr::str_detect(
        validate_latitude_longitude_format(d),
        "The .+ column of the location table contains values that"))
  }
  
})

# validate_latitude_longitude_range() -----------------------------------------

testthat::test_that("validate_latitude_longitude_range()", {
  
  d <- test_data
  
  # Valid latitude and longitude results in message.
  
  expect_null(validate_latitude_longitude_range(d))
  
  # Invalid latitude and longitude results in character string.
  
  for (i in c("latitude", "longitude")) {
    d <- test_data
    if (i == "latitude") {
      d$location[[i]][3:4] <- c(100, -100)
      expect_true(
        stringr::str_detect(
          validate_latitude_longitude_range(d),
          paste0(
            "The latitude column of the location table contains values ",
            "outside the bounds -90 to 90 in rows: 3, 4")))
    } else if (i == "longitude") {
      d$location[[i]][3:4] <- c(190, -190)
      expect_true(
        stringr::str_detect(
          validate_latitude_longitude_range(d),
          paste0(
            "The longitude column of the location table contains values ",
            "outside the bounds -180 to 180 in rows: 3, 4")))
    }
  }
  
})

# validate_elevation() -----------------------------------------

testthat::test_that("validate_elevation()", {
  
  d <- test_data
  
  # Valid elevation results in message.
  
  expect_null(validate_elevation(d))
  
  # Invalid elevation results in character string.
  
  d$location$elevation[3:4] <- c(8849, -10985)
  expect_true(
    stringr::str_detect(
      validate_elevation(d),
      paste0(
        "The elevation column of the location table contains ",
        "values that may not be in the unit of meters. ",
        "Questionable values exist in rows: 3, 4")))
  
})

# validate_data() ---------------------------------------------------------

testthat::test_that("validate_data", {
  
  d <- test_data
  
  # If multiple validation issues, then report all issues with a warning or 
  # error.
  
  # Create issue for validate_table_presence()
  d$dataset_summary <- NULL
  
  # Create issue for validate_column_names()
  # FIXME: This operation fails devtools::check() but passes devtools::test()
  # colnames(d$taxon_ancillary) <- c(
  #   "taxon_ancillary_id", "taxon_id", "datetime", "variable_name", "value",
  #   "invalid_col_name")
  
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
  # Create issue for validate_latitude_longitude_format()
  d$location$latitude[3] <- "invalid_coordinate_format"
  d$location$longitude[3] <- "invalid_coordinate_format"
  # Create issue for validate_latitude_longitude_format()
  d$location$latitude[4] <- -100
  d$location$longitude[4] <- -190
  # Create issue for validate_elevation()
  d$location$elevation[4] <- 8849
  d$location$elevation[5] <- -10985
  
  issues <- suppressWarnings(validate_data(data.list = d))
  expect_equal(length(issues), 15)
  expect_true(is.list(issues))
  expect_true(is.character(issues[[1]]))
  
})
