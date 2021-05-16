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
  
  # plot_*() ------------------------------------------------------------------
  
  if (fun.name == "plot") {
    # dataset
    if (!is.null(fun.args$dataset)) {
      validate_dataset_structure(fun.args$dataset)
    }
    # alpha
    if (!is.null(fun.args$alpha)) {
      if (!(fun.args$alpha <= 1 & fun.args$alpha > 0)) {
        stop("Intput 'alpha' should be 0-1.", call. = FALSE)
      }
    }
  }
  
  # search() -------------------------------------------------------------
  
  if (fun.name == "search") {
    
    # text
    
    if (!is.null(fun.args$text) & !is.character(fun.args$text)) {
      stop("Input 'text' must be of class 'character'.", call. = F)
    }
    
    # taxa
    
    if (!is.null(fun.args$taxa) & !is.character(fun.args$taxa)) {
      stop("Input 'taxa' must be of class 'character'.", call. = F)
    }
    
    # num_taxa
    
    if (!is.null(fun.args$num_taxa)) {
      if (!is.numeric(fun.args$num_taxa)) {
        stop("Input 'num_taxa' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$num_taxa) != 2) {
        stop(
          "Input 'num_taxa' must have a minimum and maximum value.", 
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
    
    # sd_years
    
    if (!is.null(fun.args$sd_years)) {
      if (!is.numeric(fun.args$sd_years)) {
        stop(
          "Input 'sd_years' must be of class 'numeric'.", 
          call. = F)
      }
      if (length(fun.args$sd_years) != 2) {
        stop(
          "Input 'sd_years' must have a minimum and maximum value.", 
          call. = F)
      }
    }
    
    # area
    
    if (!is.null(fun.args$area)) {
      if (!is.numeric(fun.args$area)) {
        stop(
          "Input 'area' must be of class 'numeric'.", 
          call. = F)
      }
      if (length(fun.args$area) != 4) {
        stop(
          paste0(
            "Input 'area' must have North, East, South, and West ",
            "coordinates." ), 
          call. = F)
      }
    }
    
    # boolean
    
    if (!is.null(fun.args$boolean)) {
      if (!(tolower(fun.args$boolean) %in% c("and", "or"))) {
        stop(
          "Valid inputs to 'boolean' are: 'AND', 'OR'", 
          call. = F)
      }
    }
    
  }
  
  # validate() -------------------------------------------------------
  
  if (fun.name == "validate") {
    # path
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {                                     # exists
        stop("Input 'path' doesn't exits.", call. = F)
      }
    }
    # dataset
    if (!is.null(fun.args$dataset)) {
      validate_dataset_structure(fun.args$dataset)                               # has expected structure
    }
    
  }
  
  # read() ---------------------------------------------------------------
  
  if (fun.name == "read") {
    if (is.null(fun.args$from)) {                                            # EDI is accessible (required by many functions)
      ping_edi()
    }
    # id
    if (!is.null(fun.args$id)) {
      if (!is.character(fun.args$id)) {                                           # is character
        stop("Input 'id' should be character.", call. = FALSE)
      }
      validate_id(fun.args$id)                                                    # exists and warns if newer
    }
    # path
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {                                           # exists
        stop("Input 'path' (", fun.args$path, ") doesn't exist.", call. = FALSE)
      }
    }
    # parse_datetime
    if (!is.null(fun.args$parse_datetime)) {
      if (!is.logical(fun.args$parse_datetime)) {                                 # is logical
        stop("Input 'parse_datetime' should be logical.", call. = FALSE)
      }
    }
    # unique_keys
    if (!is.null(fun.args$unique_keys)) {
      if (!is.logical(fun.args$unique_keys)) {                           # is logical
        stop("Input 'unique_keys' should be logical.", call. = FALSE)
      }
    }
    # site
    if (!is.null(fun.args$site) & !is.null(fun.args$id)) {
      if (all(fun.args$site != "all")) {
        if (is_neon(fun.args$id)) {
          validate_site(fun.args$site, fun.args$id)                                # listed sites exist for a specified id
        }
      }
    }
    # startdate
    if (!is.null(fun.args$startdate)) {
      if (!is.na(fun.args$startdate)) {
        if (!stringr::str_detect(                                                  # has YYYY-MM format
          fun.args$startdate, "^[:digit:]{4}-[:digit:]{2}$")) {
          stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
               call. = FALSE)
        }
        month <- as.integer(                                                       # MM is 1-12
          stringr::str_extract(
            fun.args$startdate, "(?<=-)[:digit:]{2}$"))
        if (!((month > 0) & (month <= 12))) {
          stop("Unsupported 'startdate'. Expected format is YYYY-MM.", 
               call. = FALSE)
        }
      }
    }
    # enddate
    if (!is.null(fun.args$enddate)) {
      if (!is.na(fun.args$enddate)) {
        if (!stringr::str_detect(                                                   # has YYYY-MM format
          fun.args$enddate, "^[:digit:]{4}-[:digit:]{2}$")) {
          stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
               call. = FALSE)
        }
        month <- as.integer(                                                        # MM is 1-12
          stringr::str_extract(
            fun.args$enddate, "(?<=-)[:digit:]{2}$"))
        if (!((month > 0) & (month <= 12))) {
          stop("Unsupported 'enddate'. Expected format is YYYY-MM.", 
               call. = FALSE)
        }
      }
    }
    # package
    if (!is.null(fun.args$package)) {
      if (!(fun.args$package %in% c("basic", "expanded"))) {                        # has expected value
        stop("Input 'package' should be 'basic' or 'expanded'.", call. = FALSE)
      }
    }
    # check.size
    if (!is.null(fun.args$check.size)) {                                            # is logical
      if (!is.logical(fun.args$check.size)) {
        stop("Input 'check.size' should be logical.", call. = FALSE)
      }
    }
    # nCores
    if (!is.null(fun.args$nCores)) {                                                # is integer
      if (!(fun.args$nCores %% 1 == 0)) {
        stop("Input 'nCores' should be integer.", call. = FALSE)
      }
    }
    # forceParallel
    if (!is.null(fun.args$forceParallel) & !is.logical(fun.args$forceParallel)) {
      stop("Input 'forceParallel' should be logical.", call. = FALSE)
    }
    # neon.data.save.dir
    if (!is.null(fun.args$neon.data.save.dir)) {
      if (!dir.exists(fun.args$neon.data.save.dir)) {                               # exists
        stop("Input 'neon.data.save.dir' (", fun.args$neon.data.save.dir, ") doesn't exist.", call. = FALSE)
      }
    }
    # from
    if (!is.null(fun.args$from)) {
      if (!any(file.exists(fun.args$from) | dir.exists(fun.args$from))) { # file or dir exists
        stop("Input 'from' is a non-existant file or directory.", call. = FALSE)
      }
    }
  }
  
  # save() ---------------------------------------------------------------
  
  if (fun.name == "save") {
    # path
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {                                           # exists
        stop("Input 'path' (", fun.args$path, ") doesn't exist.", call. = FALSE)
      }
    }
    # type
    if (!is.null(fun.args$type)) {
      if (!(fun.args$type %in% c(".rds", ".csv"))) {                        # has expected value
        stop("Input 'type' should be '.rds' or '.csv'.", call. = FALSE)
      }
    }
  }
  
}






#' Validate data package/product identifier
#'
#' @param id
#'     (character) A data package/product identifier for an ecocomDP dataset.
#'     
#' @details 
#'     If invalid (i.e. not listed in the return of \code{search()}), then
#'     an error is returned.
#' 
#'     If the exact \code{id} is not indexed, but it is an EDI data package, 
#'     then a set of logic determines if a newer version is indexed and 
#'     available.
#' 
validate_id <- function(id) {
  search_index <- suppressMessages(search())
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
#'     \code{search()} output.
#' @param id
#'     (character) A data package/product identifier.
#'     
#' @details 
#'     If invalid (i.e. not listed in the return of \code{search()}), then
#'     an error is returned.
#' 
validate_site <- function(site, id) {
  search_index <- suppressMessages(search())
  available_sites <- unlist(
    stringr::str_split(
      search_index$sites[search_index$id == id], 
      ","))
  site_exists <- site %in% available_sites
  if (!all(site_exists)) {
    warning("Sites not available in ", id, ": ", paste(site[!site_exists], collapse = ", "))
  }
}








#' Validate dataset structure
#'
#' @param dataset (list) Data object returned by \code{read()} (? list of named datapackage$tables)
#'
#' @details Returns an error if \code{dataset} is malformed
#'
validate_dataset_structure <- function(dataset) {
  criteria <- read_criteria()
  res <- c()
  res <- c(res, tryCatch(is.list(dataset), error = function(cond) {FALSE})) # is a list
  res <- c(res, tryCatch(is.character(names(dataset)), error = function(cond) {FALSE})) # 1st level name is id
  res <- c(res, tryCatch("tables" %in% names(dataset[[1]]), error = function(cond) {FALSE})) # 2nd level has tables
  res <- c(res, tryCatch(all(names(dataset[[1]]$tables) %in% unique(criteria$table)), error = function(cond) {FALSE})) # table names are valid
  res <- c(res, tryCatch(all(unlist(lapply(dataset[[1]]$tables, is.data.frame))), error = function(cond) {FALSE})) # tables are data.frames
  if (!all(res)) {
    stop("Input 'dataset' has invalid structure. See return from read() ",
         "for more details.", call. = F)
  }
}
