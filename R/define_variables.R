#' Define variables for ecocomDP tables
#'
#' @description  
#'     Variables of ecocomDP are stored as categorical variables in long format.
#'     Use this function to identify unique variables and assign definitions 
#'     and any associated units.
#'
#' @usage define_variables(path, delimiter)
#' 
#'     Run this function to define variables prior to creating EML with 
#'     \code{make_eml}.
#'
#' @param path 
#'     A path to the dataset working directory containing ecocomDP tables.
#'     
#' @param delimiter
#'     The field delimiter of ecocomDP tables. Can be comma or tab delimited 
#'     (i.e. "," or "\t")
#'
#' @return 
#'     A tab delimited UTF-8 file in the ecocomDP working directory titled 
#'     \emph{tablename_variables.txt} containing unique values 
#'     of attributes of class "categorical" which is translated and written 
#'     to EML with \code{make_eml}.
#'     
#' @details 
#'     This function overwrites any 
#'     \emph{tablename_variables.txt} files you have created in the ecocomDP 
#'     working directory. To prevent overwriting of these files, temporarily 
#'     move them out of the working directory.
#'     
#'     This function identifies "categorical" class attributes from the file 
#'     \emph{attributes_tablename.txt} stored in the \emph{/inst/} directory of
#'     the code package and extracts unique values of these attributes to the 
#'     table for you to define. Do not define categorical variables with empty
#'     field contents. Delete these rows from this file.
#'     
#'     When defining categorical variables with unit values, refer to the 
#'     standard unit dictionary "name" column. The unit dictionary will be 
#'     automatically opened when running this function. Enter the unit name in 
#'     the unit column of the categorical variables table. Note these values 
#'     are case sensitive. If you cannot find a unit in the dictionary, enter 
#'     the unit in the tab delimited UTF-8 formatted file 
#'     \emph{custom_units.txt} that has been automatically imported into your
#'     working directory. Valid custom units must be convertible to SI Units 
#'     (i.e. International System of Units). If it cannot be converted to SI 
#'     then list it units column as "dimensionless". To create a custom unit 
#'     define the:
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
#' @export
#'
#' @seealso 
#'     \code{\link{make_eml}} to make an EML file and write to the working
#'     directory.


define_variables <- function(path, delimiter) {
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  
  # Parameters ----------------------------------------------------------------
  
  table_patterns <- c("observation\\b", "event\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b")
  table_names <- c("observation", "event", "sampling_location_ancillary", "taxon_ancillary")
  dir_files <- list.files(path)
  table_names_found <- list()
  tables_found <- list()
  for (i in 1:length(table_patterns)){
    tables_found[[i]] <- grep(table_patterns[i], dir_files, value = T)
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
    
    file.copy(from = paste(path.package("ecocomDP"),
                           "/custom_units.txt",
                           sep = ""),
              to = path)
    
    write_catvars <- function(tables_found, delimiter, table_names){
      
      for (i in 1:length(tables_found)){
        print(paste("Reading", tables_found[i]))
        data_in <- read.table(paste(path,
                                    "/",
                                    tables_found[i],
                                    sep = ""),
                              header = T,
                              sep = delimiter,
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
        print(paste("Writing ", table_names[i], "_variables.txt", sep = ""))
        write.table(catvars,
                    paste(path,
                          "/",
                          table_names[i],
                          "_variables",
                          ".txt",
                          sep = ""),
                    sep = "\t",
                    row.names = F,
                    quote = F,
                    fileEncoding = "UTF-8")
      }
    }
    write_catvars(tables_found, delimiter, table_names)
  }
  # Prompt the user to manually edit the catvars file and custom unit files.
  print("Open:")
  for (i in 1:length(tables_found)){
    print(paste("variables_", table_names[i], ".txt", sep = ""))
  }
  print(paste("add definitions and units then save, and close.",sep = ""))
  view_unit_dictionary()
}

