#' Import ecocomDP template files
#'
#' @description  
#'     Imports template files utilized in the ecocomDP creation process.
#'
#' @usage 
#'     import_templates(data.path = "")
#'
#' @param data.path
#'     A path to the dataset working directory entered as a character string.
#'
#' @return 
#'     \emph{additional_contact.txt} A tab delimited UTF-8 text file in which 
#'     to provide contact information of the person that created the ecocomDP
#'     from a parent data package. This additional contact is added to the EML
#'     of the ecocomDP along with the contacts of the parent data package.
#'     
#'     \emph{additional_provenance.txt} A UTF-8 text file containing fields to 
#'     fill out to provide provenance of a parent data package that does not 
#'     exist in the EDI data repository. All columns of this file must be 
#'     completed.
#'     
#'     \emph{cusom_units.txt} A UTF-8 text file containing fields to fill out
#'     for each custom unit that is found within the dataset.
#'     
#' @details 
#'     This function should be run at the beginning of the ecocomDP creation 
#'     process. Make a working directory for your dataset then run this function 
#'     to import template files. If template files already exist in the working 
#'     directory, new templates will not be imported.
#'     
#' @export     
#'     


import_templates <- function(data.path){
  
  # Check arguments
  
  if (missing(data.path)){
    stop("Specify path to dataset working directory.")
  }
  
  # Import files that are not already in the directory
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/additional_contact.txt",
                                  sep = ""),
                     to = data.path)
  if (isTRUE(value)){
    print("Importing additional_contact.txt ... ")
  } else {
    print("additional_contact.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/additional_provenance.txt",
                                  sep = ""),
                     to = data.path)
  if (isTRUE(value)){
    print("Importing additional_provenance.txt ... ")
  } else {
    print("additional_provenance.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/custom_units.txt",
                                  sep = ""),
                     to = data.path)
  if (isTRUE(value)){
    print("Importing custom_units.txt ... ")
  } else {
    print("custom_units.txt already exists ... ")
  }

  
}
