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
                     "This table:.\n",
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
    # msg <- list()
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
        required_column_names <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i]) & (criteria$required == "yes")]
        if ((table_names[i] == "observation") & (!identical(grep("observation_ancillary\\b", dir_files, value = T), character(0)))){
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
    }
    if (length(unlist(msg)) > 0){
      cat("\n",unlist(msg))
      stop("See above message for details", call. = F)
    }
  }
  report_required_columns_missing(dir_files, name.pattern, table_names)


  # Validate column vector classes
  
  # print("Column classes ...")
  # 
  # table_names_regexpr <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  # validate_column_classes <- function(dir_files, table_names_regexpr){
  #   msg <- list()
  #   for (i in 1:length(table_names_regexpr)){
  #     table_in <- grep(table_names_regexpr[i], dir_files, value = T)
  #     if (!identical(table_in, character(0))){
  #     
  #       data_in <- read.table(paste(data.path,
  #                                   "/",
  #                                   table_in,
  #                                   sep = ""),
  #                             header = T,
  #                             sep = sep,
  #                             as.is = T,
  #                             na.strings = "NA")
  #       colnames_in <- colnames(data_in)
  #       found_column_classes <- unname(unlist(lapply(data_in, class)))
  #       colnames_expected <- criteria$column[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
  #       required_column_classes <- criteria$class[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
  #       index <- match(colnames_in, colnames_expected)
  #       required_column_classes <- required_column_classes[index]
  #       rcci <- required_column_classes
  #       fcci <- found_column_classes
  #       ncci <- colnames_in
  #       msg2 <- list()
  #       for (j in 1:length(ncci)){
  #         if ((rcci[j] == "character") & (fcci[j] == "integer")){
  #           
  #         } else if ((rcci[j] == "character") & (fcci[j] == "numeric")){
  #           
  #         } else if ((rcci[j] == "character") & (fcci[j] == "Date")){
  #           
  #         } else if ((rcci[j] == "integer") & (fcci[j] == "character")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "integer") & (fcci[j] == "numeric")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "integer") & (fcci[j] == "Date")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "numeric") & (fcci[j] == "character")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "numeric") & (fcci[j] == "Date")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "Date") & (fcci[j] == "numeric")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         } else if ((rcci[j] == "Date") & (fcci[j] == "integer")){
  #           msg2[[j]] <- paste("This table has an incorrect column class:\n",
  #                              table_in, "\n",
  #                              "Column in question:\n",
  #                              ncci[j], "\n",
  #                              "Column class found:\n",
  #                              fcci[j], "\n",
  #                              "Column class required:\n",
  #                              rcci[j], "\n")
  #         }
  #       }
  #     }
  #     msg[[i]] <- msg2
  #   }
  #   if (length(unlist(msg)) > 0){
  #     cat("\n",unlist(msg))
  #     stop("See above message for details", call. = F)
  #   }
  # }
  # validate_column_classes(dir_files, table_names_regexpr)
  
  
  # # Validate datetime format
  # 
  # print("Check datetime format ...")
  # 
  # table_names_regexpr <- c("observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b")
  # check_datetime_format <- function(dir_files, table_names_regexpr, sep){
  #   msg <- list()
  #   for (i in 1:length(table_names_regexpr)){
  #     table_in <- grep(table_names_regexpr[i], dir_files, value = T)
  #     if (!identical(table_in, character(0))){
  #       data_in <- read.table(paste(data.path,
  #                                   "/",
  #                                   table_in,
  #                                   sep = ""),
  #                             header = T,
  #                             sep = sep,
  #                             as.is = T,
  #                             na.strings = "NA")
  #       colnames_in <- colnames(data_in)
  #       coldatetime <- grep("datetime\\b", colnames_in, value = T)
  #       len_date_in <- dim(na.omit(data_in[coldatetime]))[1]
  #       dates <- as.Date(data_in[[coldatetime]], format = datetime_iso8601)
  #       if (sum(is.na(dates)) > 0){
  #         msg[[i]] <- paste("This table contains a datetime not in the required format:\n",
  #                           table_in, "\n",
  #                           "Remember the required format is:\n",
  #                           "YYYY-MM-DD","\n")
  #       }
  #     }
  #   }
  #   if (length(msg) > 0){
  #     cat("\n",unlist(msg))
  #     stop("See above message for details", call. = F)
  #   }
  # }
  # check_datetime_format(dir_files, table_names_regexpr, sep)
  
  
  # Validation complete
  print("Congratulations! Your ecocomDP has passed validation!")
  
  
}
  