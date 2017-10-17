#' Define ecocomDP variables
#'
#' @description  
#'     This function identifies unique variables listed in the ecocomDP tables
#'     and writes them to tables for you to provide definitions and units for.
#'
#' @usage define_variables(data.path, sep)
#'
#' @param data.path 
#'     A path to the dataset working directory containing ecocomDP tables.
#' @param sep
#'     The field separator string. Values within each row of ecocomDP tables
#'     are separated by this string. Valid options are "," or "\t".
#'
#' @return 
#'     A tab delimited UTF-8 file in the dataset working directory titled 
#'     \emph{ecocomDPtablename_variables.txt} with fields:
#'     \itemize{
#'         \item \strong{attributeName} Name of the variable field from the
#'         an ecocomDP table.
#'         \item \strong{code} Unique variable names detected in an ecocomDP 
#'         table.
#'         \item \strong{definition} A field for users to supply definitions 
#'         for variables listed in the code field.
#'         \item \strong{units} A field for users to supply the units for each 
#'         defined variable.
#'     }
#'     
#' @details 
#'     Run this function after you have created and validated ecocomDP tables but
#'     prior to create EML with \code{make_eml}. 
#' 
#'     Variables of ecocomDP tables are stored in long format. Use 
#'     \code{define_variables} to identify unique variables and assign 
#'     definitions and associated units.
#'     
#'     Refer to the standard unit dictionary when selecting units. Open the 
#'     standard unit dictionary by calling \code{view_unit_dictionary}. Search 
#'     for a unit and enter the value listed in the \emph{name} field of the 
#'     dictionary into the \emph{units} field of 
#'     \emph{ecocomDPtablename_variables.txt}. NOTE these values are case 
#'     sensitive. If you cannot find the unit you are looking for, enter the 
#'     unit in the tab delimited UTF-8 formatted file \emph{custom_units.txt} 
#'     that is imported into your dataset working directory when calling 
#'     \code{import_templates}. Valid custom units must be convertible to SI 
#'     Units (i.e. International System of Units). If it cannot be converted to
#'     SI then list it in the units field as "dimensionless". To create a 
#'     custom unit define the:
#'     \itemize{
#'         \item \strong{id} This is equivalent to the unit name. Reference 
#'         the standard unit dictionary formatting
#'         \item \strong{unitType} The type of unit being defined.
#'         Reference the dictionary for examples.
#'         \item \strong{parentSI} The SI equivalent of the id you have entered.
#'         \item \strong{multiplierToSI} This is the multiplier to convert 
#'         from your custom unit to the SI unit equivalent.
#'         \item \strong{description} A description of the custom unit. 
#'         Reference the dictionary for examples.
#'         }
#'
#' @seealso \code{\link{import_templates}} to import ecocomDP templates.
#' @seealso \code{\link{make_eml}} to make EML for an ecocomDP.
#'
#' @export
#'


define_variables <- function(data.path, sep) {
  
  # Check arguments
  
  if (missing(data.path)){
    stop("Specify path to dataset working directory.")
  }
  
  # Parameters ----------------------------------------------------------------
  
  table_patterns <- c("observation\\b", "event\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "summary\\b", "sampling_location\\b", "taxon\\b")
  table_names <- c("observation", "event", "sampling_location_ancillary", "taxon_ancillary", "summary", "sampling_location", "taxon")
  dir_files <- list.files(data.path)
  table_names_found <- list()
  tables_found <- list()
  for (i in 1:length(table_patterns)){
    tables_found[[i]] <- dir_files[grep(paste("^(?=.*", table_patterns[i], ")(?!.*variables)", sep = ""), dir_files, perl=TRUE)]
    if (!identical(tables_found[[i]], character(0))){
      table_names_found[[i]] <- table_names[i]
    }
  }
  tables_found <- unlist(tables_found)
  table_names <- unlist(table_names_found)
  
  # Begin function ------------------------------------------------------------

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new variable tables? This will overwrite your previous work! (y or n):  ")
  
  if (answer == "y"){
    
    print("Custom units template copied to working directory.")
    
    write_catvars <- function(tables_found, sep, table_names){
      tables_out <- list()
      for (i in 1:length(tables_found)){
        if ((table_names[i] == "observation")|(table_names[i] == "event")|(table_names[i] == "sampling_location_ancillary")|(table_names[i] == "taxon_ancillary")){
          print(paste("Reading", tables_found[i]))
          data_in <- read.table(paste(data.path,
                                      "/",
                                      tables_found[i],
                                      sep = ""),
                                header = T,
                                sep = sep,
                                as.is = T,
                                na.strings = "NA")
          # Build the catvars table
          print("Identifying categorical variables ...")
          univars <- unique(data_in[["variable_name"]])
          catvars <- data.frame(attributeName = character(length(univars)),
                                code = character(length(univars)),
                                definition = character(length(univars)),
                                units = character(length(univars)),
                                stringsAsFactors = F)
          catvars[["attributeName"]] <- rep("variable_name", length(univars))
          catvars[["code"]] <- univars
          # Write catvars table
          print(paste("Writing ", "variables_", table_names[i], ".txt", sep = ""))
          write.table(catvars,
                      paste(data.path,
                            "/",
                            table_names[i],
                            "_variables",
                            ".txt",
                            sep = ""),
                      sep = "\t",
                      row.names = F,
                      quote = F,
                      fileEncoding = "UTF-8") 
          tables_out[[i]] <- table_names[i]
        }
      }
    }
    write_catvars(tables_found, sep, table_names)
  }
  # Prompt the user to manually edit the catvars file and custom unit files.
  tables_out <- unlist(tables_out)
  print("Open:")
  for (i in 1:length(tables_out)){
    print(paste(tables_out[i], "_variables", ".txt", sep = ""))
  }
  print(paste("add definitions and units then save, and close.",sep = ""))
  view_unit_dictionary()
}

