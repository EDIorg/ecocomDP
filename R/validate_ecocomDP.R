#' Validate ecocomDP tables
#'
#' @description  
#'     Once you've formatted a dataset to ecocomDP you will need to check that 
#'     it complies with the data pattern standard. This process ensures your 
#'     dataset is valid and can be combined with other datasets in the ecocomDP
#'     with the aggregate_ecocomDP function.
#'
#' @usage validate_ecocomDP(path, delimiter)
#'     Run this function after ecocomDP creation and before making EML for it.
#' 
#' @param path 
#'     A path to the dataset working directory containing \emph{only} ecocomDP 
#'     tables. This directory should not contain anything else.
#' @param delimiter
#'     The field delimiter of ecocomDP tables. Can be comma or tab delimited 
#'     (i.e. "," or "\t")
#'
#' @return 
#'     A validation report printed in the console window of RStudio. This 
#'     validator runs until it hits an error. You must address the reported
#'     issues before continuing. Once all the validation checks have been 
#'     passed, you will recieve a message "Congratulations! Your ecocomDP
#'     has passed validation!"
#'     
#'          
#' @details 
#'    Checks:
#'    \itemize{
#'        \item \strong{Table naming} Table names must follow the ecocomDP 
#'        naming convention \emph{studyName_ecocomDPTableName}. Make sure table
#'        names are consistent.
#'        \item \strong{Required tables} Some tables are required. Add any 
#'        missing tables.
#'        \item \strong{NULL tables} NULL tables take up unnecessary space.
#'        Delete these.
#'        \item \strong{NULL columns} NULL columns take up unnecessary space.
#'        Delete these.
#'        \item \strong{Column names} Column names must be valid.
#'        Adjust column names as required.
#'        \item \strong{Required columns} Some columns are required.
#'        Add missing columns.
#'        \item \strong{Column classes} Column classes must be correct.
#'        \item \strong{Datetime format} Date time format must follow ISO 8601,
#'        specifically \emph{YYYY-MM-DDThh:mm+-hh}. When reporting only use
#'        elements of this format required by the precision of the original 
#'        data.
#'    }
#'         
#' @export
#'

validate_ecocomDP <- function(path, delimiter) {
  
  # Parameters ----------------------------------------------------------------
  
  datetime_iso8601 <- "%Y-%m-%d"
  dir_files <- list.files(path, recursive = T)

  # Load validation criteria --------------------------------------------------
  
  print("Loading validation criteria ... ")
  
  criteria <- read.table(paste(path.package("ecocomDP"),
                               "/validation_criteria.txt",
                               sep = ""),
                         header = T,
                         sep = delimiter,
                         as.is = T,
                         na.strings = "NA")
  
  valid_table_names <- unique(criteria$table)
  
  required_table_names <- criteria$table[(is.na(criteria$class)) & (criteria$required == "yes")]
  
  valid_column_names <- unique(criteria$column)
  valid_column_names <- valid_column_names[!is.na(valid_column_names)]
  
  
  
  # Validate input ecocomDP to criteria listed in /inst/ ----------------------
  
  print("Validating ecocomDP ...")
  
  # Check table naming criteria
  
  print("Checking table names ...")
  
  validate_table_names <- function(dir_files, valid_table_names){
    msg <- list()
    input_table_names <- dir_files[attr(regexpr(paste(valid_table_names, 
                                                      collapse = "|"), 
                                                dir_files),
                                        "match.length")
                                   != -1]
    table_study_name <- gsub(paste(valid_table_names, collapse = "|"), "", input_table_names)
    table_study_names <- unique(table_study_name)
    if (length(table_study_names) > 1){
      msg[[1]] <- paste("Inconsistencies found in study names of ecocomDP tables.\n",
                      "Please ensure study names match for all tables.\n",
                      "Here are the unique study names found:\n")
      for (i in 1:length(table_study_names)){
        msg[[i+1]] <- paste(table_study_names[i], "\n")
      }
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
    
  }
  validate_table_names(dir_files, valid_table_names)

  
  # Report missing tables that are required
  
  print("Checking for required tables ...")
  
  required_table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "observation\\b", "dataset_summary\\b")
  report_missing_tables <- function(dir_files, required_table_names_adjusted, required_table_names){
    msg <- list()
    for (i in 1:length(required_table_names_adjusted)){
      table_in <- grep(required_table_names_adjusted[i], dir_files, value = T)
      # data_in <- read.table(paste(path,
      #                             "/",
      #                             table_in,
      #                             sep = ""),
      #                       header = T,
      #                       sep = delimiter,
      #                       as.is = T,
      #                       na.strings = "NA")
      # colnames_in <- colnames(data_in)
      # coldatetime <- grep("datetime\\b", colnames_in, value = T)
      # len_date_in <- dim(na.omit(data_in[coldatetime]))[1]
      #dates <- as.Date(data_in[[coldatetime]], format = datetime_iso8601)
      # dates <- as.Date(data_in[[coldatetime]], format = "%m-%d-%Y")
      if (identical(table_in, character(0))){
        msg[[i]] <- paste("This table is required but is missing:\n",
                          required_table_names[i], "\n")
      }
    }
    if (length(msg) > 0){
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
  }
  report_missing_tables(dir_files, required_table_names_adjusted, required_table_names)
  
  # Remove NULL tables
  
  print("Checking for NULL tables ...")
  
  report_null_tables <- function(dir_files, delimiter){
    msg <- list()
    for (i in 1:length(dir_files)){
      data_in <- read.table(paste(path,
                                  "/",
                                  dir_files[i],
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      data_clean <- data_in[rowSums(is.na(data_in)) != ncol(data_in), ]
      data_dim <- dim(data_clean)
      if (data_dim[1] == 0){
        msg[[1]] <- paste("This file does not contain any data:\n",
                      dir_files[i], "\n",
                      "Remove this table.")
        cat("\n", unlist(msg))
        stop("See above message for details", call. = F)
      }
    }
  }
  report_null_tables(dir_files, delimiter)
  
  
  # Columns ---------------------------------------------------------------------

  # Remove NULL columns

  print("Check for NULL columns ...")

  table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  table_names <- c("sampling_location", "taxon", "event", "observation", "sampling_location_ancillary", "taxon_ancillary", "dataset_summary")
  report_null_columns <- function(dir_files, delimiter, table_names_adjusted, table_names){
    msg <- list()
    for (i in 1:length(table_names_adjusted)){
      table_in <- grep(table_names_adjusted[i], dir_files, value = T)
      data_in <- read.table(paste(path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      output <-sapply(data_in, function(x)all(is.na(x)))
      data_in_names <- names(output)
      data_in_logic <- unname(unlist(output))
      null_cols <- data_in_names[data_in_logic]
      if (!identical(null_cols, character(0))){
        msg[[i]] <- paste("This table has NULL columns:\n",
                          table_in,"\n",
                          "NULL columns found:\n",
                          paste(null_cols, collapse = "\n"),"\n")
      }
    }
    if (length(msg) > 0){
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
  }
  report_null_columns(dir_files, delimiter, table_names_adjusted, table_names)
  
  
  # Validate column names
  
  print("Check column names ...")
  
  table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  table_names <- c("sampling_location", "taxon", "event", "observation", "sampling_location_ancillary", "taxon_ancillary", "dataset_summary")
  report_invalid_column_names <- function(dir_files, valid_column_names, delimiter, table_names_adjusted, table_names){
    msg <- list()
    for (i in 1:length(table_names_adjusted)){
      table_in <- grep(table_names_adjusted[i], dir_files, value = T)
      data_in <- read.table(paste(path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      colnames_in <- colnames(data_in)
      index <- match(colnames_in, valid_column_names)
      index_2 <- 1:length(index)
      invalid_column_names <- colnames_in[index_2[is.na(index)]]
      if (sum(is.na(index)) > 0){
        for (j in 1:length(invalid_column_names)){
          msg[[j]] <- paste("This file contains invalid column names:\n",
                        dir_files[i], "\n",
                        "Invalid column name:\n",
                        invalid_column_names[j], "\n")
        }
        cat("\n",unlist(msg))
        stop("See above message for details", call. = F)
      }
    }
  }
  report_invalid_column_names(dir_files, valid_column_names, delimiter)
  
  
  # Report requried columns that are missing

  print("Check for required columns that are missing ...")

  table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  table_names <- c("sampling_location", "taxon", "event", "observation", "sampling_location_ancillary", "taxon_ancillary", "dataset_summary")
  report_required_columns_missing <- function(dir_files, table_names_adjusted, table_names, delimiter){
    msg <- list()
    for (i in 1:length(table_names_adjusted)){
      table_in <- grep(table_names_adjusted[i], dir_files, value = T)
      data_in <- read.table(paste(path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      required_column_names <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i]) & (criteria$required == "yes")]
      if ((table_names[i] == "observation") & (!identical(grep(table_names_adjusted[3], dir_files, value = T), character(0)))){
        required_column_names <- c(required_column_names, "event_id")
      }
      colnames_in <- colnames(data_in)
      index <- match(required_column_names, colnames_in)
      index2 <- 1:length(index)
      missing_column_names <- required_column_names[index2[is.na(index)]]
      if (sum(is.na(index)) > 0){
        for (j in 1:length(missing_column_names)){
          msg[[i+j]] <- paste("This table is missing required columns:\n",
                        table_in, "\n",
                        "Missing column name:\n",
                        missing_column_names[j],"\n")
        }
      }
    }
    cat("\n",unlist(msg))
    stop("See above message for details", call. = F)
  }
  report_required_columns_missing(dir_files, table_names_adjusted, table_names, delimiter)


  # Validate column vector classes
  
  print("Column classes ...")
  
  table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  validate_column_classes <- function(dir_files, table_names_adjusted){
    msg <- list()
    for (i in 1:length(table_names_adjusted)){
      table_in <- grep(table_names_adjusted[i], dir_files, value = T)
      data_in <- read.table(paste(path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      colnames_in <- colnames(data_in)
      found_column_classes <- unname(unlist(lapply(data_in, class)))
      colnames_expected <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
      required_column_classes <- criteria$class[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
      index <- match(colnames_in, colnames_expected)
      required_column_classes <- required_column_classes[index]
      rcci <- required_column_classes
      fcci <- found_column_classes
      ncci <- colnames_in
      msg2 <- list()
      for (j in 1:length(ncci)){
        if ((rcci[j] == "character") & (fcci[j] == "integer")){
          
        } else if ((rcci[j] == "character") & (fcci[j] == "numeric")){
          
        } else if ((rcci[j] == "character") & (fcci[j] == "Date")){
          
        } else if ((rcci[j] == "integer") & (fcci[j] == "character")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "integer") & (fcci[j] == "numeric")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "integer") & (fcci[j] == "Date")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "numeric") & (fcci[j] == "character")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "numeric") & (fcci[j] == "Date")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "Date") & (fcci[j] == "numeric")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        } else if ((rcci[j] == "Date") & (fcci[j] == "integer")){
          msg2[[j]] <- paste("This table has an incorrect column class:\n",
                             table_in, "\n",
                             "Column in question:\n",
                             ncci[j], "\n",
                             "Column class found:\n",
                             fcci[j], "\n",
                             "Column class required:\n",
                             rcci[j], "\n")
        }
      }
      msg[[i]] <- msg2
    }
    if (length(unlist(msg)) > 0){
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
  }
  validate_column_classes(dir_files, table_names_adjusted)
  
  
  # Validate datetime format
  
  print("Check datetime format ...")
  
  table_names_adjusted <- c("observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b")
  check_datetime_format <- function(dir_files, table_names_adjusted, delimiter){
    msg <- list()
    for (i in 1:length(table_names_adjusted)){
      table_in <- grep(table_names_adjusted[i], dir_files, value = T)
      data_in <- read.table(paste(path,
                                  "/",
                                  table_in,
                                  sep = ""),
                            header = T,
                            sep = delimiter,
                            as.is = T,
                            na.strings = "NA")
      colnames_in <- colnames(data_in)
      coldatetime <- grep("datetime\\b", colnames_in, value = T)
      len_date_in <- dim(na.omit(data_in[coldatetime]))[1]
      dates <- as.Date(data_in[[coldatetime]], format = datetime_iso8601)
      if (sum(is.na(dates)) > 0){
        msg[[i]] <- paste("This table contains a datetime not in the required format:\n",
                      table_in, "\n",
                      "Remember the required format is:\n",
                      "YYYY-MM-DD","\n")
      }
    }
    if (length(msg) > 0){
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
  }
  check_datetime_format(dir_files, table_names_adjusted, delimiter)
  
  
  # Validation complete
  print("Congratulations! Your ecocomDP has passed validation!")
  
  
}
  