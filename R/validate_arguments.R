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
validate_arguments <- function(fun.name, fun.args) {
  
  # Parameterize --------------------------------------------------------------
  
  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)
  
  criteria <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
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
  
  # validate_ecocomDP() -------------------------------------------------------
  
  if (fun.name == "validate_ecocomDP") {
    
    # data.path - Is a valid path
    
    if (!is.null(fun.args$data.path)) {
      if (!dir.exists(fun.args$data.path)) {
        stop("Input 'data.path' doesn't exits.", call. = F)
      }
    }

    # data.list - Is a named list containing only ecocomDP tables
    
    if (!is.null(fun.args$data.list)) {
      if (!is.list(fun.args$data.list)) {
        stop("Input 'data.list' is not a list.", call. = F)
      }
      use_i <- names(fun.args$data.list) %in% unique(criteria$table)
      if (any(!use_i)) {
        stop(
          "Input 'data.list' has unsupported tables: ", 
          paste(names(fun.args$data.list)[!use_i], collapse = ", "), call. = F)
      }
    }
    
  }
  
  # read_data() ---------------------------------------------------------------
  
  if (fun.name == "read_data") {
    
    # search_index <- search_data()
    
    
    # id - Exists in the search_data() default output, otherwise drops the 
    # invalid input and issues a warning. If a newer revision exists, a warning
    # is returned.
    #
    # if (!is.null(fun.args$id)) {
    #   validate_id <- function(x) {
    #     search_index <- suppressMessages(search_data())
    #     
    #     # Is it in the search index?
    #     index_i <- fun.args$id %in% search_index$id
    #     if (!all(index_i)) {
    #       # If not indexed, is it an older revision? Currently, only EDI 
    #       # supports revisions
    #       not_indexed <- fun.args$id[!index_i]
    #       possible_revision <- not_indexed[
    #         stringr::str_detect(
    #           not_indexed, 
    #           "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")]
    #       if (length(possible_revision) > 0) {
    #         browser()
    #         # FIXME: Only search indexed EDI packages
    #         not_revision <- stringr::str_remove_all(possible_revision, "\\.[:digit:]*$") %in% 
    #           stringr::str_remove_all(search_index$id, "\\.[:digit:]*$")
    #       }
    #       # warning(
    #       #   "Invalid input 'id' ", paste(fun.args$id[!use_i], collapse = ", "), 
    #       #   " cannot be read.", call. = FALSE)
    #       # fun.args$id <- fun.args$id[use_i]
    #     }
    #     
    #     # Check for newer revisions - If newer is found the original input is
    #     # removed so the next step (id validation) won't fail
    #     use_i <- (fun.args$id %in% search_index$id) & 
    #       (stringr::str_detect(
    #         fun.args$id, 
    #         "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)"))
    #     
    #     if (!all(use_i)) {
    #       browser()
    #       warning(
    #         "Invalid input 'id' ", paste(fun.args$id[!use_i], collapse = ", "), 
    #         " cannot be read.", call. = FALSE)
    #       fun.args$id[use_i]
    #     }
    #     
    #     fun.args$id
    #   }
    #   fun.args$id <- validate_id()
    # }
    
    
    # path - Is valid
    
    # file.type - Is a supported type
    
    # site - Exists in the search_data() default output
    
    # startdate - Character of YYYY-MM format
    
    # enddate - Character of YYYY-MM format
    
    # check.size - Is logical
    
    # nCores - Is iteger
    
    # forceParallel - Is logical
    
    # Return modified inputs
    
    return(fun.args)
    
  }
  
}



