#' Validate arguments of ecocomDP functions
#'
#' @description
#'     Validate input arguments to ecocomDP functions.
#'
#' @param fun.name
#'     (character) Name of function from which \code{validate_arguments()} is
#'     called.
#' @param fun.args
#'     (named list) Arguments passed to calling function and formatted as 
#'     \code{as.list(environment())}.
#'     
#' @details
#'     Validation checks are function specific.    
#'

validate_arguments <- function(fun.name, fun.args){
  
  # Parameterize --------------------------------------------------------------
  
  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)
  
  # search_data() -------------------------------------------------------------
  
  if (fun.name == "search_data") {
    
    # text
    
    if (!is.null(fun.args$text) & !is.character(fun.args$text)) {
      stop("Input 'text' must be of class 'character'.", call. = F)
    }
    
    # taxa
    
    if (!is.null(fun.args$taxa) & !is.character(fun.args$taxa)) {
      stop("Input 'taxa' must be of class 'character'.", call. = F)
    }
    
    # num.taxa
    
    if (!is.null(fun.args$num.taxa)) {
      if (!is.numeric(fun.args$num.taxa)) {
        stop("Input 'num.taxa' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$num.taxa) != 2) {
        stop(
          "Input 'num.taxa' must have a minimum and maximum value.", 
          call. = F)
      }
    }

    # years
    
    if (!is.null(fun.args$years)) {
      if (!is.numeric(fun.args$years)) {
        stop("Input 'years' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$years) != 2) {
        stop(
          "Input 'years' must have a minimum and maximum value.", 
          call. = F)
      }
    }
    
    # sd.between.surveys
    
    if (!is.null(fun.args$sd.between.surveys)) {
      if (!is.numeric(fun.args$sd.between.surveys)) {
        stop(
          "Input 'sd.between.surveys' must be of class 'numeric'.", 
          call. = F)
      }
      if (length(fun.args$sd.between.surveys) != 2) {
        stop(
          "Input 'sd.between.surveys' must have a minimum and maximum value.", 
          call. = F)
      }
    }
    
    # geographic.area
    
    if (!is.null(fun.args$geographic.area)) {
      if (!is.numeric(fun.args$geographic.area)) {
        stop(
          "Input 'geographic.area' must be of class 'numeric'.", 
          call. = F)
      }
      if (length(fun.args$geographic.area) != 4) {
        stop(
          paste0(
            "Input 'geographic.area' must have North, East, South, and West ",
            "coordinates." ), 
          call. = F)
      }
    }
    
    # boolean.operator
    
    if (!is.null(fun.args$boolean.operator)) {
      if (!(tolower(fun.args$boolean.operator) %in% c("and", "or"))) {
        stop(
          "Valid inputs to 'boolean.operator' are: 'AND', 'OR'", 
          call. = F)
      }
    }
    
  }
  
  
}
