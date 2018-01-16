#' Validate path
#'
#' @description  
#'     Use \code{dir.exists} to determine whether the input path is valid and 
#'     returns an error message if not.
#'
#' @usage validate_path(path = "")
#' 
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' 
#' @return 
#'     A warning message if the path leads to a non-existant directory.
#'
#' @export
#'

validate_path <- function(path){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  
  # Validate path -------------------------------------------------------------
  
  if (!dir.exists(path)){
    stop('The directory specified by the argument "path" does not exist! Please enter the correct path for your dataset working directory.')
  }
  
}
