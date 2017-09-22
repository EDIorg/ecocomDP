#' Define categorical variables
#'
#' @description  
#'     Identify and define categorical variables of ecocomDP tables.
#'
#' @usage define_catvars(path)
#' 
#'     Run this function to set variable definitions prior to running make_eml.
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
#'     \emph{tablename_catvars.txt} containing unique values 
#'     of attributes of class "categorical" which is translated and written 
#'     to EML with \code{make_eml}.
#'     
#' @details 
#'     This function overwrites any 
#'     \emph{tablename_catvars.txt} files you have created in 
#'     the ecocomDP working directory. To prevent overwriting of these files, 
#'     temporarily move them out of the working directory.
#'     
#'     This function identifies "categorical" class attributes from the file 
#'     \emph{attributes_tablename.txt} and extracts unique 
#'     values of these attributes to the table for you to define. Do not define 
#'     categorical variables with empty field contents. Delete these rows from 
#'     this file.
#'     
#'     When defining categorical variables with unit values, refer to the 
#'     standard unit dictionary "name" column. Enter the unit name in the 
#'     definition column of the categorical variables table. Note these values 
#'     are case sensitive.
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
  

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new categorical variable tables? This will overwrite your previous work! (y or n):  ")
  
  if (answer == "y"){
    
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
                              stringsAsFactors = F)
        catvars[["attributeName"]] <- rep("variable_name", length(univars))
        catvars[["code"]] <- univars
        # Write catvars table
        print(paste("Writing ", "catvars_", table_names[i], sep = ""))
        write.table(catvars,
                    paste(path,
                          "/",
                          "catvars_",
                          table_names[i],
                          sep = ""),
                    sep = "\t",
                    row.names = F,
                    quote = F,
                    fileEncoding = "UTF-8")
        # Prompt the user to manually edit the catvars file and custom unit files.
        view_unit_dictionary()
        readline(
          prompt = paste("Open ", "catvars_", table_names[i], " define factor codes, then save, close, and press <enter>.",
                         sep = ""))

      }
    }
  }
}

