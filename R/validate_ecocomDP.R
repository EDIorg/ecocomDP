#' Validate ecocomDP tables
#'
#' @description  
#'     Once you've formatted a dataset to the ecocomDP (L1) you will need to 
#'     check that it complies with the data pattern standard. This process 
#'     ensures your L1 is valid and can be combined with other L1 with the 
#'     \code{aggregate_ecocomDP} function.
#'
#' @usage validate_ecocomDP(data.path, sep)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#'     
#' @param datetime.format.string
#'     A character string specifying the datetime format string used in your
#'     L1 tables.
#'
#' @return 
#'     A validation report printed in the console window of RStudio. The 
#'     validation checks run until an error is encountered. You must address 
#'     errors before continuing to the next check. Once all validation checks 
#'     have successfully completed, you will be notified with a congratulatory 
#'     message.
#'          
#' @details 
#' 
#'     Run this function after creating a L1 and before making EML for it.
#' 
#'    Validation checks performed by this function:
#'    \itemize{
#'        \item \strong{Table names} Table names must follow the ecocomDP 
#'        naming convention (i.e. \emph{studyName_ecocomDPTableName.ext}, e.g. 
#'        \emph{gleon_chloride_observation.csv}). 
#'        \item \strong{Required tables} Some L1 tables are 
#'        required, others are not.
#'        \item \strong{Column names} Column names must be those specified by
#'        the data pattern.
#'        \item \strong{Required columns} Some columns are 
#'        required, others are not.
#'        \item \strong{Column classes} Column classes must match the 
#'        ecocomDP specification.
#'        \item \strong{Datetime format string} Datetime values should match
#'        the input datetime.format.string parameter.
#'    }
#'         
#' @export
#'

validate_ecocomDP <- function(data.path, datetime.format.string) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }
  if (missing(datetime.format.string)){
    stop('Input argument "datetime.format.string" is missing! Specify the datetime format string used in your ecocomDP tables.')
  }
  
  # Validate path
  
  validate_path(data.path)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)

  
  # Load validation criteria --------------------------------------------------
  
  message("Loading validation criteria ")
  
  criteria <- read.table(paste(path.package("ecocomDP"),
                               "/validation_criteria.txt",
                               sep = ""),
                         header = T,
                         sep = "\t",
                         as.is = T,
                         na.strings = "NA")
  
  column_names <- unique(criteria$column[!is.na(criteria$column)])
  
  table_names <- unique(criteria$table)
  
  table_names_regexpr <- paste0("_",
                                table_names,
                                "\\b")
  
  table_names_required <- criteria$table[(is.na(criteria$class))
                                         & (criteria$required == "yes")]
  
  table_names_required_regexpr <- paste0("_",
                                         table_names_required,
                                         "\\b")
  
  
  # Validate file naming convention -------------------------------------------
  
  message("Checking table names")
  
  validate_table_names <- function(dir.files, name.pattern){
    msg <- list()
    tables <- dir.files[attr(regexpr(paste(name.pattern, 
                                                      collapse = "|"), 
                                                dir.files),
                                        "match.length")
                                   != -1]
    study_names <- unique(gsub(paste(name.pattern, 
                                     collapse = "|"), 
                               "", tables))
    study_names <- str_replace(study_names, "\\.[:alnum:]*$", replacement = "")
    if (length(study_names) > 1){
      stop(paste("\n",
                 "More than one study name found in your ecocomDP tables.\n",
                 "Only one study name is allowed.\n",
                 "Here are the unique study names found:\n",
                 paste(study_names, collapse = ", ")),
           call. = F)
    }
  }
  validate_table_names(dir.files = dir_files, name.pattern = table_names_regexpr)

  
  # Report required tables that are missing -----------------------------------
  
  message("Checking for required tables")
  
  report_missing_tables <- function(dir.files, name.pattern){
    missing_tables <- c()
    tables <- dir.files[attr(regexpr(paste(name.pattern, 
                                           collapse = "|"), 
                                     dir.files),
                             "match.length")
                        != -1]
    for (i in 1:length(name.pattern)){
      table_in <- grep(name.pattern[i], tables, value = T)
      if (identical(table_in, character(0))){
        missing_tables[i] <- str_sub(name.pattern[i], 2, nchar(name.pattern[i])-2)
      }
    }
    if (!identical(missing_tables, NULL)){
      missing_tables <- missing_tables[!is.na(missing_tables)]
      stop(paste("\n",
                 "One or more required tables are missing.\n",
                 "These are the missing tables:\n",
                 paste(missing_tables, collapse = ", ")))
    }
  }
  report_missing_tables(dir.files = dir_files, name.pattern = table_names_required_regexpr)
  
  
  # Validate column names -----------------------------------------------------
  
  message("Checking column names")
  
  report_invalid_column_names <- function(dir.files, expected.col.names, name.pattern){
    for (i in 1:length(name.pattern)){
      table_in <- grep(name.pattern[i], dir.files, value = T)
      if (!identical(table_in, character(0))){
        sep <- detect_delimeter(path = data.path,
                                data.files = table_in,
                                os = os)
        data <- read.table(paste0(data.path,
                                  "/",
                                  table_in),
                           header = T,
                           sep = sep,
                           as.is = T,
                           na.strings = "NA")
        colnames_in <- colnames(data)
        index <- match(colnames_in, expected.col.names)
        index_2 <- 1:length(index)
        invalid_column_names <- colnames_in[index_2[is.na(index)]]
        if (sum(is.na(index)) > 0){
          stop(paste("\n",
                     "This table:\n",
                     table_in, "\n",
                     "contains these invalid column names:\n",
                     paste(invalid_column_names, collapse = ", ")))
        }
      }
    }
  }
  report_invalid_column_names(dir.files = dir_files, 
                              expected.col.names = column_names,
                              name.pattern = table_names_regexpr)
  
  
  # Report requried columns that are missing ----------------------------------

  message("Checking for required columns")

  report_required_columns_missing <- function(dir.files, name.pattern, col.names){
    for (i in 1:length(name.pattern)){
      table_in <- grep(name.pattern[i], dir.files, value = T)
      if (!identical(table_in, character(0))){
        sep <- detect_delimeter(path = data.path,
                                data.files = table_in,
                                os = os)
        data_in <- read.table(paste0(data.path,
                                     "/",
                                     table_in),
                              header = T,
                              sep = sep,
                              as.is = T,
                              na.strings = "NA")
        use_i <- criteria$table %in% substr(name.pattern[i], 2, nchar(name.pattern[i]) - 2)
        use_i2 <- criteria$required == "yes"
        use_i3 <- (use_i == T) & (use_i2 == T) 
        required_column_names <- criteria$column[use_i3]
        required_column_names <- required_column_names[!is.na(criteria$column[use_i3])]
        found_column_names <- colnames(data_in)
        use_i <- required_column_names %in% found_column_names
        missing_column_names <- required_column_names[!use_i]
        if (sum(use_i) < length(use_i)){
          stop(paste("\n",
                     "This table:\n",
                     table_in, "\n",
                     "is missing these required columns:\n",
                     paste(missing_column_names, collapse = ", ")))
        }
      }
    }
  }
  report_required_columns_missing(dir.files = dir_files, 
                                  name.pattern = table_names_regexpr, 
                                  col.names = column_names)
  

  # Report incorrect column classes -------------------------------------------
  
  message("Checking column classes")

  validate_column_classes <- function(dir.files, name.pattern){
    for (i in 1:length(name.pattern)){
      table_in <- grep(table_names_regexpr[i], dir.files, value = T)
      if (!identical(table_in, character(0))){
        sep <- detect_delimeter(path = data.path,
                                data.files = table_in,
                                os = os)
        data_in <- read.table(paste(data.path,
                                    "/",
                                    table_in,
                                    sep = ""),
                              header = T,
                              sep = sep,
                              as.is = T,
                              na.strings = "NA")
        found_column_names <- colnames(data_in)
        found_column_classes <- unname(unlist(lapply(data_in, class)))
        found_numeric_classes <- (found_column_classes == "numeric") | (found_column_classes == "integer")
        found_column_classes[found_numeric_classes] <- "numeric"
        found_column_criteria <- data.frame(colname = found_column_names,
                                            colclass = found_column_classes,
                                            stringsAsFactors = F)
        expected_column_names <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
        expected_column_classes <- criteria$class[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
        expected_numeric_classes <- (expected_column_classes == "numeric") | (expected_column_classes == "integer")
        expected_column_criteria <- data.frame(colname = expected_column_names,
                                               colclass = expected_column_classes,
                                               stringsAsFactors = F)
        use_i <- expected_column_criteria$colclass == "Date"
        if (sum(use_i) > 0){
          expected_column_criteria$colclass[use_i] <- "character"
        }
        use_i <- expected_column_criteria$colclass == "character"
        if (sum(use_i > 0)){
          found_column_criteria$colclass[use_i] <- "character"
        }
        use_i <- match(found_column_criteria$colname,
                       expected_column_criteria$colname)
        use_i <- found_column_criteria$colclass == expected_column_criteria$colclass[use_i]
        if (sum(!use_i) > 0){
          stop(paste("\n",
                     "This table:\n",
                     table_in, "\n",
                     "contains these columns:\n",
                     paste(found_column_criteria$colname[!use_i], collapse  = ", "), "\n",
                     "with these incorrect column classes:\n",
                     paste(found_column_criteria$colclass[!use_i], collapse = ", "), "\n",
                     "The correct classes for these columns are:\n",
                     paste(expected_column_criteria$colclass[!use_i], collapse = ", ")))                                                    
        }
      }
    }
  }
  validate_column_classes(dir.files = dir_files, name.pattern = table_names_regexpr)
  
  
  # Validate table IDs --------------------------------------------------------
  
  
  # Validation complete -------------------------------------------------------
  
  message(paste('Congratulations! Your ecocomDP tables have passed validation!\n',
                'Now make metadata for it with the "make_eml" function'))
  
  
}
  