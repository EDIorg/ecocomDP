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
  

  # Tables ----------------------------------------------------------------------
  
  # Validate table names
  
  # Report missing tables
  
  # Columns ---------------------------------------------------------------------
  
  # Remove columns NA columns
  
  # Validate column names
  
  # Report requried columns that are missing 
  
  # Validate column vector classes
  
  # Validate datetime format
  
  # Validation report -----------------------------------------------------------
  
  
}
  