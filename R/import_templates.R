#' Import templates for ecocomDP
#'
#' @description  
#'     Create a working directory for your dataset then run this function to 
#'     import templates. Use these templates to map level-0 data (raw) to 
#'     level-1 (ecocomDP), as well as provide metadata for your dataset.
#'
#' @usage 
#'     import_templates(path)
#'
#' @param path 
#'     A path to the dataset working directory.
#'
#' @return 
#'     \emph{additional_contact.txt} A tab delimited UTF-8 text file in which 
#'     to provide contact information of the person that created the ecocomDP
#'     from a parent data package. This additional contact is added to the EML
#'     of the ecocomDP along with the contacts of the parent data package.
#'     
#'     \emph{cusom_units.txt} A UTF-8 text file containing fields to fill out
#'     for each custom unit that is found within the dataset.
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be imported.
#'     
#' @export     
#'     


import_templates <- function(path){
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  
  # Import files that are not already in the directory
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/additional_contact.txt",
                                  sep = ""),
                     to = path)
  if (isTRUE(value)){
    print("Importing additional_contact.txt ... ")
  } else {
    print("additional_contact.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/custom_units.txt",
                                  sep = ""),
                     to = path)
  if (isTRUE(value)){
    print("Importing custom_units.txt ... ")
  } else {
    print("custom_units.txt already exists ... ")
  }

  
}
