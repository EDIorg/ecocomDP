#' validate_primary_keys
#'
#' @description  
#'     Checks that primary keys of a table are unique and issues an error if
#'     otherwise.
#'
#' @usage validate_primary_keys(x, col)
#' 
#' @param x
#'     (data frame) The table to be checked.
#' @param col
#'     (character) Name of the column containing primary keys.
#'
#' @return 
#'     An error is issued if primary keys are not unique.
#'         
#' @export
#'

validate_primary_keys <- function(x, col) {
  
  # Check inputs --------------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  if (!is.data.frame(x)){
    stop('Input argument "x" is not a data frame!')
  }
  if (missing(col)){
    stop('Input argument "col" is missing!')
  }
  if (length(col) > 1){
    stop('Only one input is allowed for argument "col"!')
  }
  
  # Are primary keys unique? --------------------------------------------------
  
  if (!isTRUE(length(unique(x[ , col])) == nrow(x))){
    
    use_i <- seq(nrow(x))[duplicated(x[ , 'observation_id'])]
    
    stop(paste0("\n",
                "Primary keys are not unique!\n",
                "Duplicates found in rows:\n",
                paste(use_i, collapse = " ")),
         call. = F
         )
  }
  
}
