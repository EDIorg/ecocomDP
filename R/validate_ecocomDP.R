#' Validate ecocomDP tables
#'
#' @description  
#'     Once you've formatted a dataset to ecocomDP you will need to check that 
#'     it complies with the data pattern standard. This process ensures your 
#'     dataset is valid and can be combined with other datasets in the ecocomDP
#'     with the aggregate_ecocomDP function.
#'
#' @usage validate_ecocomDP(path)
#'     Run this function after ecocomDP creation and before making EML for it.
#' 
#' @param path 
#'     A path to the dataset working directory containing ecocomDP tables. This
#'     directory should not contain an ecocomDP of another dataset.
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

validate_ecocomDP <- function(path) {
  
  # Parameters ------------------------------------------------------------------
  
  datetime_iso8601 <- "%Y-%m-%d"
  dir_files <- list.files(path, recursive = T)

  # Load validation criteria --------------------------------------------------
  
  print("Loading validation criteria ... ")
  
  criteria <- read.table(paste(path.package("ecocomDP"),
                               "/validation_criteria.txt",
                               sep = ""),
                         header = T,
                         sep = "\t",
                         as.is = T,
                         na.strings = "NA")
  
  valid_table_names <- unique(criteria$table)
  
  valid_column_names <- unique(criteria$column)
  valid_column_names <- valid_column_names[!is.na(valid_column_names)]
  
  
  
  # Validate input ecocomDP to criteria ---------------------------------------
  
  print("Validating input ecocomDP:")
  
  # Validate table names
  
  print("Table names ...")
  
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
                    "Please ensure study names match."))
    }
  }

  # Report missing tables
  
  # Columns ---------------------------------------------------------------------
  
  # Remove columns NA columns
  
  # Validate column names
  
  # Report requried columns that are missing 
  
  # Validate column vector classes
  
  # Validate datetime format
  
  # Validation report -----------------------------------------------------------
  
  
}
  