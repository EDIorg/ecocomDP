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
#'     \emph{map.txt} A tab delimited UTF-8 text file to map level-0 to level-1
#'     data. Open this file with a spreadsheet editor. and reference 
#'     instructions for help.
#'     
#'     \emph{map_instructions.txt} A UTF-8 text file containing instructions on
#'     how to complete mapping of level-0 to level-1.
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
  
  # Begin function
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/map.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing map.txt ... ")
  } else {
    print("map.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("ecocomDP"),
                                  "/map_instructions.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing map_instructions.txt ... ")
  } else {
    print("map_instructions.txt already exists ... ")
  }
  
}
