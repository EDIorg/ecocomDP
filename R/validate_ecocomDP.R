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
#'     A validation report printed in the console window of RStudio.
#'          
#' @details 
#'    Checks:
#'    \itemize{
#'        \item \strong{Table names} Table names follow the ecocomDP 
#'        convention \emph{studyName_ecocomDPTableName}.
#'        \item \strong{Missing tables} Some tables are required. 
#'        Missing tables are reported.
#'        \item \strong{NULL columns} NULL columns take up space
#'        and are not necessary for ecocomDP aggregation.
#'        \item \strong{Column names} Column names must be correct to 
#'        facilitate aggregation.
#'        \item \strong{Missing columns} Some columns are required.
#'        \item \strong{Column classes} Column classes must be correct.
#'        \item \strong{datetime format} Date time format must follow ISO 8601,
#'        specifically \emph{YYYY-MM-DDThh:mm+-hh}. Datetime reporting only use
#'        elements of this format required by the precision of the original 
#'        data.
#'    }
#'         
#' @export
#'

validate_ecocomDP <- function(path, delimiter) {
  
  # Arguments to function -----------------------------------------------------
  
  #delimiter <- "\t" # Can be "," or "\t"
  
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
  
  validate_table_names <- function(valid_table_names){
    input_table_names <- dir_files[attr(regexpr(paste(valid_table_names, 
                                                      collapse = "|"), 
                                                dir_files),
                                        "match.length")
                                   != -1]
    table_study_name <- gsub(paste(valid_table_names, collapse = "|"), "", input_table_names)
    table_study_names <- unique(table_study_name)
    if (length(table_study_names) > 1){
      warning(paste("Inconsistencies found in study names of ecocomDP tables.\n",
                    "Please ensure study names match for all tables.\n",
                    "Here are the unique study names found:\n"))
      for (i in 1:length(table_study_names)){
        warning(table_study_names[i])
      }
    }
  }

  
  # Report missing tables that are required
  
  print("Checking for required tables ...")
  
  required_table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "observation", "dataset_summary")
  report_missing_tables <- function(required_table_names_adjusted){
    required_tables_found <- dir_files[attr(regexpr(paste(required_table_names_adjusted, 
                                 collapse = "|"), 
                           dir_files),
                   "match.length")
              != -1]
    if (length(required_tables_found) < 4){
      warning(paste("One or more required tables are missing.\n",
                    "Required tables are:\n",
                    "sampling_location\n",
                    "taxon\n",
                    "observation\n",
                    "dataset_summary\n"))
    }
  }
  
  # Remove NULL tables
  
  print("Checking for NULL tables ...")
  
  report_null_tables <- function(dir_files){
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
        warning(paste("This file does not contain any data:\n",
                      dir_files[i], "\n",
                      "Consider removing it."))
      }
    }
  }
  
  
  # Columns ---------------------------------------------------------------------

  # # Remove NULL columns
  # 
  # print("NULL columns ...")
  # 
  # report_null_columns <- function(dir_files){
  #   for (i in 1:length(dir_files)){
  #     data_in <- read.table(paste(path,
  #                                 "/",
  #                                 dir_files[i],
  #                                 sep = ""),
  #                           header = T,
  #                           sep = delimiter,
  #                           as.is = T,
  #                           na.strings = "NA")
  #     data_clean <- data_in[rowSums(is.na(data_in)) != ncol(data_in), ]
  #     data_dim <- dim(data_clean)
  #     if (data_dim[1] == 0){
  #       warning(paste("This file does not contain any data:\n",
  #                     dir_files[i], "\n",
  #                     "Consider removing it."))
  #     }
  #   }
  # }
  
  
  # Validate column names
  
  print("Check column names ...")
  
  report_invalid_column_names <- function(dir_files, valid_column_names){
    for (i in 1:length(dir_files)){
      data_in <- read.table(paste(path,
                                  "/",
                                  dir_files[i],
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
          warning(paste("This file contains invalid column names:\n",
                        dir_files[i], "\n",
                        "Invalid column name:\n",
                        invalid_column_names[j]),"\n")
        }
      }
    }
  }
  
  
  # Report requried columns that are missing 
  
  print("Check for required columns that are missing ...")

  table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  table_names <- c("sampling_location", "taxon", "event", "observation", "sampling_location_ancillary", "taxon_ancillary", "dataset_summary")
  report_required_columns_missing <- function(dir_files, table_names_adjusted, table_names){
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
      colnames_in <- colnames(data_in)
      index <- match(required_column_names, colnames_in)
      index2 <- 1:length(index)
      missing_column_names <- required_column_names[index2[is.na(index)]]
      if (sum(is.na(index)) > 0){
        for (j in 1:length(missing_column_names)){
          warning(paste("This table is missing required columns:\n",
                        table_in, "\n",
                        "Missing column name:\n",
                        missing_column_names[j]),"\n")
        }
      }
    }
  }


  # Validate column vector classes
  
  # print("Column classes ...")
  # 
  # table_names_adjusted <- c("sampling_location\\b", "taxon\\b", "event\\b", "observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b")
  # report_incorrect_column_classes <- function(dir_files, table_names_adjusted){
  #   for (i in 1:length(table_names_adjusted)){
  #     table_in <- grep(table_names_adjusted[i], dir_files, value = T)
  #     data_in <- read.table(paste(path,
  #                                 "/",
  #                                 table_in,
  #                                 sep = ""),
  #                           header = T,
  #                           sep = delimiter,
  #                           as.is = T,
  #                           na.strings = "NA")
  #     colnames_in <- colnames(data_in)
  #     required_column_classes <- criteria$class[(!is.na(criteria$class)) & (criteria$table == table_names[i])]
  #     found_column_classes <- unname(unlist(lapply(data_in, class)))
  #     index <- match(found_column_classes, required_column_classes)
  #     index2 <- 1:length(index)
  #     rcci <- required_column_classes[index2[is.na(index)]]
  #     fcci <- found_column_classes[index2[is.na(index)]]
  #     ncci <- colnames_in[index2[is.na(index)]]
  #     for (j in 1:length(ncci)){
  #       if ((rcci[j] == "character") & (fcci[j] == "integer")){
  #         
  #       } else if ((rcci[j] == "character") & (fcci[j] == "numeric")){
  #         
  #       } else if ((rcci[j] == "character") & (fcci[j] == "Date")){
  #         
  #       } else if ((rcci[j] == "integer") & (fcci[j] == "character")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "integer") & (fcci[j] == "numeric")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "integer") & (fcci[j] == "Date")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "numeric") & (fcci[j] == "character")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "numeric") & (fcci[j] == "Date")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "Date") & (fcci[j] == "numeric")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       } else if ((rcci[j] == "Date") & (fcci[j] == "integer")){
  #         warning(paste("This table has an incorrect column class:\n",
  #                       table_in, "\n",
  #                       "Column in question:\n",
  #                       ncci[j], "\n",
  #                       "Column class found:\n",
  #                       fcci[j], "\n",
  #                       "Column class required:\n",
  #                       rcci[j]))
  #       }
  #     }
  # 
  #     
  #     
  #     index <- match(required_column_names, colnames_in)
  #     index2 <- 1:length(index)
  #     missing_column_names <- required_column_names[index2[is.na(index)]]
  #     if (sum(is.na(index)) > 0){
  #       for (j in 1:length(missing_column_names)){
  #         warning(paste("This table is missing required columns:\n",
  #                       table_in, "\n",
  #                       "Missing column name:\n",
  #                       missing_column_names[j]),"\n")
  #       }
  #     }
  #   }
  # }
  
  
  # Validate datetime format
  
  print("Check datetime format ...")
  
  table_names_adjusted <- c("observation\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b")
  check_datetime_format <- function(dir_files, table_names_adjusted){
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
      #dates <- as.Date(data_in[[coldatetime]], format = datetime_iso8601)
      dates <- as.Date(data_in[[coldatetime]], format = "%m-%d-%Y")
      if (sum(is.na(dates)) > 0){
        warning(paste("This table contains a datetime not in the required format:\n",
                      table_in, "\n",
                      "Remember the required format is:\n",
                      "YYYY-MM-DD"))
      }
    }
  }
      
}
  