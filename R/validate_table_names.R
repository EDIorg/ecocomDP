#' validate_table_names
#'
#' @description  
#'     This function ensures that your ecocomDP (L1) tables follow the
#'     table naming convention (i.e. \emph{studyName_ecocomDPTableName.ext}, 
#'     e.g. \emph{gleon_chloride_observation.csv}).
#'
#' @usage validate_table_names(data.path, criteria)
#' 
#' @param data.path 
#'     (character) Path to the directory containing the L1 tables.
#'     tables.
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If table names are valid, then the corresponding table names are 
#'     returned. If table names are invalid, then an error message is returned.
#'         
#' @export
#'

validate_table_names <- function(data.path, criteria) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(data.path)){
    stop('Input argument "data.path" is missing! Specify path to your ecocomDP tables.')
  }
  if (missing(criteria)){
    stop('Input argument "criteria" is missing! Specify the validation criteria for the ecocomDP tables.')
  }
  if (!is.data.frame(criteria)){
    stop('Input argument "criteria" is not a data frame!')
  }
  
  # Validate path
  
  validate_path(data.path)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Misc.
  
  dir_files <- list.files(data.path, 
                          recursive = F)
  
  
  # Load validation criteria --------------------------------------------------
  
  table_names <- unique(criteria$table)

  table_names_regexpr <- paste0("_",
                                table_names,
                                "\\b")
  
  # Validate file naming convention -------------------------------------------
  
  message("Checking table names ...")
  
  msg <- list()
  tables <- dir_files[attr(regexpr(paste(table_names_regexpr, 
                                         collapse = "|"), 
                                   dir_files),
                           "match.length")
                      != -1]
  study_names <- unique(gsub(paste(table_names_regexpr, 
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
  } else {
    message('... table names are valid')
    tables
  }

}
