#' Validate tables against the model
#' 
#' @param dataset (list) A dataset of the structure returned by \code{read_data()}.
#' @param path (character) Path to a directory containing ecocomDP tables as files.
#'     
#' @note This function is used by ecocomDP creators (to ensure what has been created is valid), maintainers (to improve the quality of archived ecocomDP datasets), and users (to ensure the data being used is free of error).
#'     
#' @return (list) If any checks fail, then a list of validation issues are returned along with a warning. If no issues are found then NULL is returned.
#'          
#' @details 
#'    Validation checks:
#'    \itemize{
#'        \item File names - File names are the ecocomDP table names.
#'        \item Table presence - Required tables are present.
#'        \item Column names - Column names of all tables match the model.
#'        \item Column presence - Required columns are present.
#'        \item Column classes - Column classes match the model specification.
#'        \item Datetime format - Date and time formats follow the model specification.
#'        \item Primary keys - Primary keys of tables are unique.
#'        \item Composite keys - Composite keys (unique constraints) of each table are unique.
#'        \item Referential integrity - Foreign keys have a corresponding primary key.
#'        \item Coordinate format - Values are in decimal degree format.
#'        \item Coordinate range - Values are within -90 to 90 and -180 to 180.
#'        \item Elevation - Values are less than Mount Everest (8848 m) and greater than Mariana Trench (-10984 m).
#'        \item Variable mapping - variable_name is in table_name.
#'        \item Mapped_id - values in mapped_id are valid URIs
#'    }
#'    
#' @export
#'    
#' @examples 
#' \dontrun{
#' # Write a set of ecocomDP tables to file for validation
#' mydir <- paste0(tempdir(), "/dataset")
#' dir.create(mydir)
#' write_tables(
#'   path = mydir,
#'   observation = ants_L1$tables$observation, 
#'   observation_ancillary = ants_L1$tables$observation_ancillary,
#'   location = ants_L1$tables$location,
#'   location_ancillary = ants_L1$tables$location_ancillary,
#'   taxon = ants_L1$tables$taxon,
#'   taxon_ancillary = ants_L1$tables$taxon_ancillary,
#'   dataset_summary = ants_L1$tables$dataset_summary,
#'   variable_mapping = ants_L1$tables$variable_mapping)
#' 
#' # Validate
#' validate_data(path = mydir)
#' 
#' # Clean up
#' unlink(mydir, recursive = TRUE)
#' }
#'
validate_data <- function(
  dataset = NULL,
  path = NULL) {
  
  # Validate arguments
  
  validate_arguments(fun.name = "validate_data", fun.args = as.list(environment()))
  
  # Parameterize
  
  criteria <- read_criteria()
  
  # Read data
  if (!is.null(path)) {
    d <- read_data(from = path)
  } else {
    d <- dataset
  }
  id <- d$id
  d <- d$tables # validation runs on tables, don't need other dataset components

  # Validate data
  # Get package/product ID from the dataset_summary table since this function
  # doesn't require an ID input.
  
  if (is.null(id)) { # to catch edge cases where no id can be found
    id <- "unknown data ID"
  }
  
  # Gather all validation issues and compile into a single report as this 
  # provides the user more information for trouble shooting and assessment than
  # only seeing the first issue encountered.
  
  message("Validating ", id, ":")
  issues_table_presence <- validate_table_presence(d)
  issues_column_names <- validate_column_names(d)
  issues_column_presence <- validate_column_presence(d)
  issues_column_classes <- validate_column_classes(d)
  issues_datetime <- validate_datetime(d)
  issues_primary_keys <- validate_primary_keys(d)
  issues_composite_keys <- validate_composite_keys(d)
  issues_referential_integrity <- validate_referential_integrity(d)
  issues_validate_latitude_longitude_format <- 
    validate_latitude_longitude_format(d)
  issues_validate_latitude_longitude_range <- 
    validate_latitude_longitude_range(d)
  issues_validate_elevation <- validate_elevation(d)
  issues_validate_variable_mapping <- validate_variable_mapping(d)
  issues_validate_mapped_id <- validate_mapped_id(d)
  
  # Report validation issues
  
  # Format results into a human readable format
  
  validation_issues <- as.list(
    c(
      issues_table_presence,
      issues_column_names,
      issues_column_presence,
      issues_column_classes,
      issues_datetime,
      issues_primary_keys,
      issues_composite_keys,
      issues_referential_integrity,
      issues_validate_latitude_longitude_format,
      issues_validate_latitude_longitude_range,
      issues_validate_elevation,
      issues_validate_variable_mapping,
      issues_validate_mapped_id))
  
  if (length(validation_issues) != 0) {
    warning("  Validation issues found for ", id, call. = FALSE)
  }
  
  validation_issues
  
}









# Check for required tables
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If required tables are missing, then the missing table names
#     are returned, otherwise NULL is returned.
#         
validate_table_presence <- function(data.list) {
  
  message("  Required tables")
  
  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  expected <- criteria$table[
    (is.na(criteria$class)) & (criteria$required == TRUE)]
  required_missing <- expected[!(expected %in% names(data.list))]
  if (length(required_missing) != 0) {
    paste0("Required table. Missing required table: ", 
           paste(required_missing, collapse = ", "))
  }
  
}










# Check for invalid column names
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If column names outside of the allowed set are found, then 
#     invalid column names are returned, otherwise NULL is returned.
#
validate_column_names <- function(data.list) {

  message('  Column names')
  
  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  
  r <- lapply(
    names(data.list),
    function(x) {
      expected <- criteria$column[
        (criteria$table %in% x) & !is.na(criteria$column)]
      invalid_columns <- !(colnames(data.list[[x]]) %in% expected)
      if (any(invalid_columns)) {
        paste0(
          "Column names. The ", x, " table has these invalid column ",
          "names: ", 
          paste(colnames(data.list[[x]])[invalid_columns], collapse = ", "))
      }
    })
  
  unlist(r)
  
}













# Check for required columns
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If required column names are absent, then missing columns
#     are returned, otherwise NULL is returned.
#
validate_column_presence <- function(data.list){
  
  message('  Required columns')

  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  
  r <- lapply(
    names(data.list),
    function(x) {
      expected <- criteria$column[
        (criteria$table %in% x) & 
          !is.na(criteria$column) & 
          (criteria$required == TRUE)]
      if ((x == "observation") & ("observation_ancillary" %in% names(data.list))) {
        expected <- c(expected, "event_id")
      }
      missing_columns <- !(expected %in% colnames(data.list[[x]]))
      if (any(missing_columns)) {
        paste0(
          "Required columns. The ", x, " table is missing these ",
          "required columns: ",
          paste(expected[missing_columns], collapse = ", "))
      }
    })
  
  unlist(r)

}








# Check for valid date and time formats
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If date and time data are not formatted to specification, 
#     then rows of invalid formats are returned, otherwise NULL is returned.
#
validate_datetime <- function(data.list) {
  message('  Datetime formats')
  # Parameterize
  criteria <- read_criteria()
  # Validate
  r <- lapply(
    names(data.list),
    function(x) {
      datetime_column <- criteria$column[
        (criteria$table %in% x) &
          (criteria$class == "Date") &
          !is.na(criteria$column)]
      if (length(datetime_column) > 0) {
        v <- data.list[[x]][[datetime_column]]
        na_count_raw <- sum(is.na(v))               # count NAs in datetime field
        v <- as.character(v)                        # coerce to character
        v <- stringr::str_remove_all(v, "(Z|z).+$") # prepare datetimes for parsing
        v <- stringr::str_replace(v, "T", " ")
        # Check different date time formats to see if one matches the data
        # Difference in NA count induced by coercion indicates a non-valid format
        use_i <- suppressWarnings(
          list(
            lubridate::parse_date_time(v, "ymd HMS"),
            lubridate::parse_date_time(v, "ymd HM"),
            lubridate::parse_date_time(v, "ymd H"),
            lubridate::parse_date_time(v, "ymd")))
        # count NAs for each attempt to parse datetime
        na_count_parsed <- unlist(
          lapply(
            use_i,
            function(k) {
              sum(is.na(k))
            }))
        # return info on datetime problems
        if (min(na_count_parsed) > na_count_raw) {
          use_i <- seq(
            length(v))[
              is.na(
                use_i[[
                  (which(na_count_parsed %in% min(na_count_parsed)))[1]]])]
          paste0(
            "Datetime format. The ", x, " table has unsupported ",
            "datetime formats in rows: ", 
            paste(use_i, collapse = ' '))
        }
      }
    })
  return(unlist(r))
}








# Check for valid column classes
#
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If column classes do not follow specification, then the 
#     offending column and detected class are returned, otherwise NULL is 
#     returned.
#
validate_column_classes <- function(data.list) {
  
  message("  Column classes")
  
  # Parameterize

  criteria <- read_criteria()

  # Validate
  
  r <- invisible(
    lapply(
      names(data.list),
      function(x) {
        lapply(
          colnames(data.list[[x]]),
          function(k) {
            if (k %in% criteria$column) {
              expected <- criteria$class[
                (criteria$table %in% x) &
                  !is.na(criteria$column) &
                  (criteria$column %in% k)]
              detected <- class(data.list[[x]][[k]])
              if (expected == "numeric") {
                if ((detected != "integer") &
                    (detected != "integer64") &
                    (detected != "double") &
                    (detected != "numeric")) {
                  issues <- list(
                    column = k, expected = expected, detected = detected)
                }
              } else if (expected == "character") {
                if (detected != "character") {
                  issues <- list(
                    column = k, expected = expected, detected = detected)
                }
              }
              if (exists("issues", inherits = FALSE)) {
                paste0(
                  "Column classes. The column ", k, " in the table ", 
                  x, " has a class of ", detected, " but a class of ", 
                  expected, " is expected.")
              }
            }
          })
      }))
  
  unlist(r)

}








# Check primary keys
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If primary keys are not unique, then row numbers of 
#     duplicate keys are returned, otherwise NULL is returned.
#
validate_primary_keys <- function(data.list) {

  message("  Primary keys")
  
  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  
  r <- invisible(
    lapply(
      names(data.list),
      function(x) {
        primary_key <- criteria$column[
          (criteria$table == x) & 
            !is.na(criteria$primary_key) & 
            (criteria$primary_key == TRUE)]
        v <- data.list[[x]][[primary_key]]
        use_i <- seq(length(v))[duplicated(v)]
        if (length(use_i) > 0) {
          paste0("Primary keys. The ", x, " table contains non-unique ",
                 "primary keys in the column ", primary_key, " at rows: ", 
                 paste(use_i, collapse = " "))
        }
      }))
  
  unlist(r)

}








# Check composite column uniqueness
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If composite keys are not unique, then rows numbers of 
#     duplicate keys are returned, otherwise NULL is returned.
#
validate_composite_keys <- function(data.list) {
  
  message("  Composite keys")
  
  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  
  r <- invisible(
    lapply(
      names(data.list),
      function(x) {
        composite_columns <- criteria$column[
          (criteria$table == x) & 
            !is.na(criteria$column) & 
            (criteria$composite_key == TRUE)]
        composite_columns <- composite_columns[                   # expected columns may be missing
          composite_columns %in% colnames(data.list[[x]])]
        if (length(composite_columns) > 0) {
          d <- dplyr::select(data.list[[x]], any_of(composite_columns))
          duplicates <- seq(nrow(d))[duplicated.data.frame(d)]
          if (length(duplicates) > 0) {
            paste0(
              "Composite keys. The composite keys composed of the columns ", 
              paste(composite_columns, collapse = ", "), 
              ", in the table ", x, " contain non-unique values in rows: ", 
              paste(duplicates, collapse = " "))
          }
        }
      }))
  
  unlist(r)
  
}








# Check referential integrity
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If foreign keys do not match primary keys, then unmatched 
#     foreign keys are returned, otherwise NULL is returned.
#
validate_referential_integrity <- function(data.list) {

  message("  Referential integrity")

  # Parameterize
  
  criteria <- read_criteria()
  
  # Validate
  
  r <- invisible(
    lapply(
      names(data.list),
      function(x) {
        primary_key <- criteria$column[
          (criteria$table == x) & 
            !is.na(criteria$column) & 
            (criteria$primary_key == TRUE)]
        primary_key_data <- stats::na.omit(data.list[[x]][[primary_key]])
        foreign_key_table <- criteria$table[
          !is.na(criteria$column) & 
            (criteria$column == primary_key)]
        output <- lapply(
          foreign_key_table,
          function(k) {
            if (k == "location") {
              foreign_key_data <- stats::na.omit(data.list[[k]][["parent_location_id"]])
              use_i <- foreign_key_data %in% primary_key_data
              if (length(use_i) == 0) {
                use_i <- TRUE # This is an exception for parent_location_id
              }
            } else {
              foreign_key_data <- stats::na.omit(data.list[[k]][[primary_key]])
              use_i <- foreign_key_data %in% primary_key_data
            }
            if (!all(use_i)) {
              paste0(
                "Referential integrity. The ", k, " table has these foreign ",
                "keys without a primary key reference in the ", x, " table: ", 
                paste(unique(foreign_key_data[!use_i]), collapse = ", "))
            }
          })
        unlist(output)
      }))
  
  unlist(r)
  
}








# Check latitude and longitude format
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If latitude and longitude values in the location table are 
#     not in decimal degree format, then a message is returned.
#
validate_latitude_longitude_format <- function(data.list) {
  
  message("  Latitude and longitude format")
  
  r <- invisible(
    lapply(
      c("latitude", "longitude"),
      function(colname) {
        na_before <- sum(is.na(data.list$location[[colname]]))
        na_after <- suppressWarnings(
          sum(is.na(as.numeric(data.list$location[[colname]]))))
        if (na_before < na_after) {
          paste0(
            "The ", colname, " column of the location table contains values ",
            "that are not in decimal degree format.")
        }
      }))
  
  unlist(r)
  
}








# Check latitude and longitude range
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If latitude and longitude values in the location table are 
#     outside of the expected ranges -90 to 90 and -180 to 180 respectively,
#     then a message is returned.
#
validate_latitude_longitude_range <- function(data.list) {
  
  message("  Latitude and longitude range")
  
  r <- invisible(
    lapply(
      c("latitude", "longitude"),
      function(colname) {
        values <- suppressWarnings(
          as.numeric(data.list$location[[colname]]))
        if (colname == "latitude") {
          use_i <- (values >= -90) & (values <= 90)
          use_i <- stats::na.omit(seq(length(use_i))[!use_i])
          if (length(use_i) > 0) {
            return(
              paste0(
                "The ", colname, " column of the location table contains ",
                "values outside the bounds -90 to 90 in rows: ", 
                paste(use_i, collapse = ", ")))
          }
        } else if (colname == "longitude") {
          use_i <- (values >= -180) & (values <= 180)
          use_i <- stats::na.omit(seq(length(use_i))[!use_i])
          if (length(use_i) > 0) {
            return(
              paste0(
                "The ", colname, " column of the location table contains ",
                "values outside the bounds -180 to 180 in rows: ", 
                paste(use_i, collapse = ", ")))
          }
        }
      }))
  
  unlist(r)
  
}








# Check elevation
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If elevation values are greater than Mount Everest (8848 m) 
#     or are less than the Mariana Trench (-10984 m), then a message is returned.
#
validate_elevation <- function(data.list) {
  
  message("  Elevation")
  
  r <- invisible(
    lapply(
      c("elevation"),
      function(colname) {
        values <- suppressWarnings(
          as.numeric(data.list$location[[colname]]))
        use_i <- (values <= 8848) & (values >= -10984)
        use_i <- stats::na.omit(seq(length(use_i))[!use_i])
        if (length(use_i) > 0) {
          return(
            paste0(
              "The ", colname, " column of the location table contains ",
              "values that may not be in the unit of meters. Questionable ",
              "values exist in rows: ", 
              paste(use_i, collapse = ", ")))
        }
      }))
  
  unlist(r)
  
}








# Check variable_mapping
# 
# @param data.list
#     (list of data frames) A named list of data frames, each of which is an 
#     ecocomDP table.
#
# @return 
#     (character) If variable_name is not in table_name, then a message is returned.
#
validate_variable_mapping <- function(data.list) {
  if ("variable_mapping" %in% names(data.list)) {
    message("  variable_mapping")
    output <- lapply(
      unique(data.list$variable_mapping$table_name),
      function(tbl) {
        foreign_keys <- unique(data.list[[tbl]]$variable_name) 
        i <- data.list$variable_mapping$table_name %in% tbl
        primary_keys <- data.list$variable_mapping$variable_name[i]
        use_i <- primary_keys %in% foreign_keys
        if (!all(use_i)) {
          paste0(
            "Variable mapping. The variable_mapping table has these variable_name values ",
            "without a match in the ", tbl, " table: ", 
            paste(primary_keys[!use_i], collapse = ", "))
        }
      })
    return(unlist(output))
  }
}




#' Check mapped_id
#' 
#' @param data.list (list) A named list of data frames, each of which is an ecocomDP table.
#'
#' @return (character) If mapped_id does not resolve with a status of 200, then a message is returned.
#' 
#' @details This function is slow to execute due to API calls for each variable listed in the variable_mapping table. To shorten wait times for \code{read_data()} operations against the archive of resolvable annotations, this function only executes during:
#' \itemize{
#'   \item the validation step of the "create ecocomDP dataset" workflow (i.e. when \code{validate_data()} is called with a \code{path} argument).
#'   \item a direct call from a unit test
#' }
#'     
#' @noRd
#'
validate_mapped_id <- function(data.list) {
  callstack <- as.character(sys.calls())
  continue <- any(stringr::str_detect(callstack, "validate_data\\(path")) | # when checking files at path
    any(stringr::str_detect(callstack, "test_that\\(")) # when unit testing
  if (continue) {
    if ("variable_mapping" %in% names(data.list)) {
      message("  mapped_id")
      output <- lapply(
        unique(data.list$variable_mapping$mapped_id),
        function(m_id) {
          m_id <- trimws(m_id)
          i <- try(httr::GET(m_id)$status, silent = T)
          if (i != 200 & !is.na(m_id)) {
            paste(m_id)
          }
        })
      output <- unlist(output)
      if (!is.null(output)) {
        output <- paste0("Variable mapping. The variable_mapping table has ",
                         "these mapped_id values that don't resolve: ",
                         paste(unlist(output), collapse = ", "))
      }
      return(output)
    }
  }
}

