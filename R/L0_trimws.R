#' Trim white space from L0 character columns
#'
#' @description  
#'     A function to remove white space from L0 character class columns. The 
#'     presence of white space affects alignment with L0 EML and downstream usage.
#'
#' @usage 
#'     L0_trimws(x)
#'
#' @param x
#'     (data frame) L0 table to trim white space from.
#'
#' @return 
#'     (data frame) L0 table with white space removed from character class 
#'     columns.
#'     
#' @export
#'

L0_trimws <- function(x){
  
  # Trim white space from both ends of character type data
  
  for (i in 1:ncol(x)){
    if (is.character(x[ ,i])){
      x[ ,i] <- trimws(x[ ,i], 'both')
    }
  }
  
  # Return
  
  x
  
}