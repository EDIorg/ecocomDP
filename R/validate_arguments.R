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
    
    # id - Is required
    
    if (is.null(fun.args$id)) {
      stop("Input 'id' is required.", call. = FALSE)
    }
    
    # Because inputs to read_data() can vary (i.e. can be a vector of id, list 
    # of id with associated arguments), they need to be converted to a 
    # consistent format for processing.

    if (!is.list(fun.args$id)) {
      empty_list <- vector(mode = "list", length(fun.args$id))
      names(empty_list) <- unlist(fun.args$id)
      fun.args$id <- empty_list
      # List default NEON arguments directly under id if missing
      for (i in 1:length(fun.args$id)) {
        if (stringr::str_detect(
          names(fun.args$id)[i], "^DP.\\.[:digit:]+\\.[:digit:]+")) {
          if (is.null(fun.args$id[[i]]$site)) {
            fun.args$id[[i]]$site <- fun.args$site
          }
          if (is.null(fun.args$id[[i]]$startdate)) {
            fun.args$id[[i]]$startdate <- fun.args$startdate
          }
          if (is.null(fun.args$id[[i]]$enddate)) {
            fun.args$id[[i]]$enddate <- fun.args$enddate
          }
        }
      }
      # Remove NEON defaults from top level
      fun.args$site <- NULL
      fun.args$startdate <- NULL
      fun.args$enddate <- NULL
    }
    
    # Validate general argument values (i.e. not associated with a specific 
    # id).
    
    # path - Is valid
    
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {
        stop("Input 'path' (", fun.args$path, ") doesn't exist.", call. = FALSE)
      }
    }
    
    # file.type - Is a supported type
    
    if (!is.null(fun.args$file.type)) {
      if (!(fun.args$file.type %in% c(".rda", ".csv"))) {
        stop("Unsupported 'file.type'. One of '.rda', '.csv' is expected.", 
             call. = FALSE)
      }
    }
    
    # check.size - Is logical
    
    if (!is.null(fun.args$check.size) & !is.logical(fun.args$check.size)) {
      stop("Unsupported 'check.size' input. Expected is TRUE or FALSE.", 
           call. = FALSE)
    }
    
    # nCores - Is iteger
    
    if (!is.null(fun.args$check.size)) {
      if (!(fun.args$nCores %% 1 == 0)) {
        stop("Unsupported 'nCores' input. Expected is an integer value.", 
             call. = FALSE)
      }
    }
    
    # forceParallel - Is logical
    
    if (!is.null(fun.args$check.size) & !is.logical(fun.args$forceParallel)) {
      stop("Unsupported 'forceParallel' input. Expected is TRUE or FALSE.", 
           call. = FALSE)
    }
    
    # Validate each id and corresponding set of argument values.
    
    invisible(
      lapply(
        seq_along(fun.args$id),
        function(x) {
          id <- names(fun.args$id)[x]
          
          # id - Exists in the search_data() default output. If a newer revision 
          # exists, a warning is returned.
          
          validate_id(id)
          
          # For a NEON id, validate associated argument values
          
          if (stringr::str_detect(id, "^DP.\\.[:digit:]+\\.[:digit:]+")) {

            # site - Listed sites exist for a specified id
            
            if (!is.null(fun.args$id[[x]]$site)) {
              if (all(fun.args$id[[x]]$site != "all")) {
                validate_site(fun.args$id[[x]]$site, id)
              }
            }
            
            # startdate - Character of YYYY-MM format, and MM is 1-12
            
            if (!is.null(fun.args$id[[x]]$startdate)) {
              if (!is.na(fun.args$id[[x]]$startdate)) {
                if (!stringr::str_detect(
                  fun.args$id[[x]]$startdate, "^[:digit:]{4}-[:digit:]{2}$")) {
                  stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
                       call. = FALSE)
                }
                month <- as.integer(
                  stringr::str_extract(
                    fun.args$id[[x]]$startdate, "(?<=-)[:digit:]{2}$"))
                if (!((month > 0) & (month <= 12))) {
                  stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
                       call. = FALSE)
                }
              }
            }
            
            # enddate - Character of YYYY-MM format, and MM is 1-12
            
            if (!is.null(fun.args$id[[x]]$enddate)) {
              if (!is.na(fun.args$id[[x]]$enddate)) {
                if (!stringr::str_detect(
                  fun.args$id[[x]]$enddate, "^[:digit:]{4}-[:digit:]{2}$")) {
                  stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
                       call. = FALSE)
                }
                month <- as.integer(
                  stringr::str_extract(
                    fun.args$id[[x]]$enddate, "(?<=-)[:digit:]{2}$"))
                if (!((month > 0) & (month <= 12))) {
                  stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
                       call. = FALSE)
                }
              }
            }
            
          }
          
        }))
    
    # Return modified arguments
    
    return(fun.args)
    
  }
  
}






#' Validate data package/product identifier
#'
#' @param id
#'     (character) A data package/product identifier for an ecocomDP dataset.
#'     
#' @details 
#'     If invalid (i.e. not listed in the return of \code{search_data()}), then
#'     an error is returned.
#' 
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
      stop("Invalid identifier '", id, "' cannot be read.", call. = FALSE)
    }
  }
}








#' Validate site name (for NEON data products only)
#'
#' @param site 
#'     (character; NEON data only) A character vector of site codes to filter 
#'     data on. Sites are listed in the "sites" column of the 
#'     \code{search_data()} output.
#' @param id
#'     (character) A data package/product identifier.
#'     
#' @details 
#'     If invalid (i.e. not listed in the return of \code{search_data()}), then
#'     an error is returned.
#' 
validate_site <- function(site, id) {
  search_index <- suppressMessages(search_data())
  available_sites <- unlist(
    stringr::str_split(
      search_index$sites[search_index$id == id], 
      ","))
  site_exists <- site %in% available_sites
  if (!all(site_exists)) {
    stop("Sites not available in ", id, ": ", paste(site[!site_exists], collapse = ", "), 
         call. = FALSE)
  }
}