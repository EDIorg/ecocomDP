#' Validate ecocomDP tables
#'
#' @description  
#'     Once you've created an ecocomDP (L1) you will need to check that it 
#'     is accurately formatted. This validation process ensures your L1 is 
#'     correct and can be combined with other L1.
#'
#' @usage validate_ecocomDP(data.path)
#' 
#' @param data.path 
#'     A character string specifying the path to the directory containing L1
#'     tables.
#'
#' @return 
#'     A validation report to the RStudio console window. When an issue is 
#'     encountered a message will be displayed with recommendations for fixing
#'     it. Each issue must be resolved before the next check will be implemented.
#'     Once all validation checks have been passed, you will be notified with 
#'     a congratulatory message and you can move on to creating EML metadata with
#'     \code{make_eml}.
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
#'        \item \strong{Relational IDs} Relational IDs should be unique.
#'    }
#'         
#' @export
#'

validate_ecocomDP <- function(data.path) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }

  # Validate path
  
  validate_path(data.path)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)

  
  # Load validation criteria --------------------------------------------------
  
  message("Loading validation criteria")
  
  criteria <- read.table(paste(path.package("ecocomDP"),
                               "/validation_criteria.txt",
                               sep = ""),
                         header = T,
                         sep = "\t",
                         as.is = T,
                         na.strings = "NA")
  
  message("Validating:")
  
  # Validate table names ------------------------------------------------------
  
  validate_table_names(data.path = data.path,
                       criteria = criteria)
  
  # Report required tables that are missing -----------------------------------

  validate_table_presence(data.path = data.path,
                          criteria = criteria)
  
  # Validate column names -----------------------------------------------------
  
  validate_column_names(data.path = data.path,
                        criteria = criteria)

  # Validate column presence --------------------------------------------------

  validate_column_presence(data.path = data.path,
                        criteria = criteria)

  # Validate column classes ---------------------------------------------------
  
  # validate_column_classes()
  
  # # Validate table IDs --------------------------------------------------------
  # 
  # 
  
  # Validation complete -------------------------------------------------------

  message(paste('Congratulations! Your ecocomDP tables have passed validation!\n',
                'Now make metadata for it with the "make_eml" function.'))

  
}
  