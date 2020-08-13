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
    
    # Because inputs to read_data() can vary (vector of id, list of id with 
    # associated arguments), they need to be converted to a consistent format.
    
    if (!is.list(fun.args$id)) {
      fun.args <- lapply(
        fun.args$id, 
        function(x) {
          fun.args$id <- x
          fun.args
        })
    } else {
      fun.args <- fun.args$id
      fun.args <- lapply(
        names(fun.args), 
        function(x) {
          if (is.null(fun.args[[x]])) {
            output <- formals(read_data)
            output$id <- x
          } else {
            output <- c(
              formals(read_data)[
                !(names(formals(read_data)) %in% names(fun.args[[x]]))],
              fun.args[[x]])
            output$id <- x
          }
          output
        })
    }
    
    # Validate each input
    
    fun.args <- lapply(
      names(fun.args$id),
      function(x) {
        browser()
        # id - Exists in the search_data() default output, otherwise the invalid
        # input is dropped and a warning issued. If a newer revision exists, a 
        # warning is returned.
        if (!is.null(x)) {
          fun.args$id <- unlist(
            lapply(fun.args$id, validate_id))
        }
        
        
      }
    )
    
    # path - Is valid
    
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {
        stop("Input 'path' doesn't exist.", call. = FALSE)
      }
    }
    
    # file.type - Is a supported type
    
    if (!is.null(fun.args$file.type)) {
      if (!(fun.args$file.type %in% c(".rda", ".csv"))) {
        stop("Unsupported 'file.type'. One of '.rda', '.csv' is expected.", 
             call. = FALSE)
      }
    }
    
    # TODO: site - Exists in the search_data() default output
    
    if (!is.null(fun.args$id) & !is.null(fun.args$site)) {
      browser()
      fun.args$site <- unlist(
        lapply(fun.args$site, validate_site))
    }
    
    # startdate - Character of YYYY-MM format, and MM is 1-12
    
    if (!is.null(fun.args$startdate)) {
      if (!stringr::str_detect(fun.args$startdate, "^[:digit:]{4}-[:digit:]{2}$")) {
        stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
             call. = FALSE)
      }
      month <- as.integer(
        stringr::str_extract(fun.args$startdate, "(?<=-)[:digit:]{2}$"))
      if (!((month > 0) & (month <= 12))) {
        stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
             call. = FALSE)
      }
    }
    
    # enddate - Character of YYYY-MM format, and MM is 1-12
    
    if (!is.null(fun.args$enddate)) {
      if (!stringr::str_detect(fun.args$enddate, "^[:digit:]{4}-[:digit:]{2}$")) {
        stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
             call. = FALSE)
      }
      month <- as.integer(
        stringr::str_extract(fun.args$enddate, "(?<=-)[:digit:]{2}$"))
      if (!((month > 0) & (month <= 12))) {
        stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
             call. = FALSE)
      }
    }
    
    # check.size - Is logical
    
    if (!is.null(fun.args$check.size)) {
      if (!is.logical(fun.args$check.size)) {
        stop("Unsupported 'check.size' input. Expected is TRUE or FALSE.", 
             call. = FALSE)
      }
    }
    
    # nCores - Is iteger
    
    if (!is.null(fun.args$nCores)) {
      if (!(fun.args$nCores %% 1 == 0)) {
        stop("Unsupported 'nCores' input. Expected is an integer value.", 
             call. = FALSE)
      }
    }
    
    # forceParallel - Is logical
    
    if (!is.null(fun.args$forceParallel)) {
      if (!is.logical(fun.args$forceParallel)) {
        stop("Unsupported 'forceParallel' input. Expected is TRUE or FALSE.", 
             call. = FALSE)
      }
    }
    
    # Return modified inputs
    
    return(fun.args)
    
  }
  
}






#' Validate data package/product identifier
#'
#' @param id
#'     (character) A data package/product identifier for an ecocomDP dataset.
#'     
#' @return 
#'     \item{id}{If valid, then \code{id} is returned along with a warning
#'     if a newer revision exists.}
#'     \item{NULL}{If invalid (i.e. not listed in the return of 
#'     \code{search_data()}) along with a warning.}
#'     
#' @details 
#'     If the exact \code{id} is not indexed, but it is an EDI data package, 
#'     then a set of logic determines if a newer version is indexed and 
#'     available.
#' 
validate_id <- function(id) {
  search_index <- suppressMessages(search_data())
  if (!(id %in% search_index$id)) {
    possible_revision <- stringr::str_detect(
      id,
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")
    if (possible_revision) {
      indexed_identifiers <- stringr::str_extract(
        search_index$id, ".+(?=\\.[:digit:]$)")
      id_identifier <- stringr::str_extract(id, ".+(?=\\.[:digit:]$)")
      if (id_identifier %in% indexed_identifiers) {
        id_version <- stringr::str_extract(id, "[:digit:]$")
        indexed_version <- stringr::str_extract(
          search_index$id[which(id_identifier == indexed_identifiers)],
          "[:digit:]$")
        if (as.numeric(indexed_version) > as.numeric(id_version)) {
          warning("A newer version of '", id, "' is available.", call. = FALSE)
        }
      }
    } else {
      id <- NULL
      warning("Invalid identifier '", id, "' cannot be read.", call. = FALSE)
    }
  }
  id
}








#' Validate site name (for NEON data products only)
#'
#' @param id
#'     (character) A data package/product identifier.
#' @param site 
#'     (character; NEON data only) A character vector of site codes to filter 
#'     data on. Sites are listed in the "sites" column of the 
#'     \code{search_data()} output.
#'     
#' @return 
#'     \item{site}{If valid, then \code{site} is returned.}
#'     \item{NULL}{If invalid, then the errant \code{site} is dropped and a
#'     warning is returned.}
#'     
#' 
validate_site <- function(site) {
  search_index <- suppressMessages(search_data())
  browser()
  
  if (!(id %in% search_index$id)) {
    possible_revision <- stringr::str_detect(
      id,
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")
    if (possible_revision) {
      indexed_identifiers <- stringr::str_extract(
        search_index$id, ".+(?=\\.[:digit:]$)")
      id_identifier <- stringr::str_extract(id, ".+(?=\\.[:digit:]$)")
      if (id_identifier %in% indexed_identifiers) {
        id_version <- stringr::str_extract(id, "[:digit:]$")
        indexed_version <- stringr::str_extract(
          search_index$id[which(id_identifier == indexed_identifiers)],
          "[:digit:]$")
        if (as.numeric(indexed_version) > as.numeric(id_version)) {
          warning("A newer version of '", id, "' is available.", call. = FALSE)
        }
      }
    } else {
      id <- NULL
      warning("Invalid identifier '", id, "' cannot be read.", call. = FALSE)
    }
  }
  id
}