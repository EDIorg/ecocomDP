# Validate arguments of ecocomDP functions
#
# @description
#     Validate input arguments to ecocomDP functions.
#
# @param fun.name
#     (character) Name of function from which \code{validate_arguments()} is
#     called.
# @param fun.args
#     (named list) Arguments passed to calling function and formatted as 
#     \code{as.list(environment())}.
#     
# @details
#     Validation checks are function specific.    
#
validate_arguments <- function(fun.name, fun.args) {
  
  # Parameterize --------------------------------------------------------------
  
  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)
  
  criteria <- read_criteria()
  
  # convert_to_dwca() ---------------------------------------------------------
  
  if (fun.name == "convert_to_dwca") {
    if (fun.args$core_name != "event") {
      stop("Invalid input to 'core_name'. Only 'event' is currently supported.", call. = FALSE)
    }
  }
  
  # create_eml() --------------------------------------------------------------
  
  if (fun.name == "create_eml") {
    
    # Basis of record
    if (!is.null(fun.args$basis_of_record)) {
      if (length(fun.args$basis_of_record) > 1) {
        stop("Only one basis_of_record is allowed.", call. = FALSE)
      }
      if ((fun.args$basis_of_record != "HumanObservation") &
          (fun.args$basis_of_record != "MachineObservation")) {
        stop("Invalid input to 'basis_of_record'. Must be 'HumanObservation' or 'MachineObservation'.", call. = FALSE)
      }
    }
    
    # create_ecocomDP.R - A standard script format facilitates maintenance
    if (is.null(fun.args$script)) {
      # Warning Input argument 'script' is missing
      stop("Input 'script' is missing.", call. = FALSE)
    } else {
      if (is.null(fun.args$script_description)) {
        stop("Input 'script_description' is missing.", call. = FALSE)
      }
    }
    if (!is.null(fun.args$script)) {
      # Only one script is allowed
      if (length(fun.args$script) != 1) {
        stop("Only one 'script' is allowed.", call. = FALSE)
      }
      # Can be found in path
      if (!(fun.args$script %in% dir(fun.args$path))) {
        stop(paste0("The 'script' ", fun.args$script, " cannot be found ",
                    "at 'path'."),
             call. = FALSE)
      }
      # Has name 'create_ecocomDP.R'
      if (fun.args$script != "create_ecocomDP.R") {
        stop("The 'script' must have the name: 'create_ecocomDP.R'.", call. = FALSE)
      }
      # Has func 'create_ecocomDP()' including recommended arguments
      check_script(path = paste0(fun.args$path, "/create_ecocomDP.R"))
    }
    
  }
  
  # create_tables() -----------------------------------------------------------
  
  if (stringr::str_detect(fun.name, "^create_")) {
    
    flat <- fun.args$L0_flat
    cols <- fun.args[names(fun.args) %in% stats::na.omit(criteria$column)] # Col args can be assigned any value, so we need a map
    
    # Table specific checks
    if (fun.name == "create_observation") {
      var_lengths <- c(length(fun.args$variable_name), length(fun.args$value), length(fun.args$unit))
      if (any(var_lengths != 1)) {
        stop("Only one input is allowed to 'variable_name', 'value', and 'unit'. Gather your primary observation variables into long format before calling this function.", call. = FALSE)
      }
    }
    
    # Check input cols exist in L0_flat
    cols_expctd <- unlist(cols, use.names = FALSE)
    cols_in_flat <- cols_expctd %in% colnames(flat)
    if (!all(cols_in_flat)) {
      stop("Columns not in 'L0_flat': ", paste(cols_expctd[!cols_in_flat], collapse = ", "), call. = FALSE)
    }
    
    # Rename input cols according to L1 specifications so the resulting L1 table is valid
    cols2rename <- base::setdiff(names(cols), c("variable_name", "unit", "location_name"))
    cols2rename <- unlist(cols[cols2rename])
    cols_flat <- colnames(flat)
    cols_flat[match(cols2rename, cols_flat)] <- names(cols2rename)
    colnames(flat) <- cols_flat
    cols[names(cols2rename)] <- names(cols[names(cols2rename)])

    # Check unit cols follow required format and have matching variable_name, otherwise downstream processes will error
    if (fun.name != "create_observation") { # Exception: Unit input to create_observation() is already gathered
      if ("unit" %in% names(cols)) {
        hasfrmt <- stringr::str_detect(cols$unit, "^unit_")
        if (!all(hasfrmt)) {
          stop("Unexpected 'unit' formats: ", 
               paste(cols$unit[!hasfrmt], collapse = ", "), call. = FALSE)
        }
        if (!is.null(cols$variable_name)) {
          hasmatch <- stringr::str_remove_all(cols$unit, "^unit_") %in% cols$variable_name
          if (!all(hasmatch)) {
            stop("Input 'unit' without 'variable_name' match: ", 
                 paste(cols$unit[!hasmatch], collapse = ", "), call. = FALSE)
          }
        }
      }
    }
    
    # Return modified arguments to the calling function
    fun.args$L0_flat <- flat
    fun.args[names(cols)] <- cols
    list2env(fun.args, envir = parent.frame(1))

  }
  
  # flatten_data() ------------------------------------------------------------
  
  if (fun.name == "flatten_data") {
    crit <- read_criteria()
    # list
    if (all(class(fun.args$tables) != "list")) {
      stop("Input 'tables' should be a list.", call. = FALSE)
    }
    # names
    nms <- names(fun.args$tables)
    expctd <- nms %in% unique(crit$table)
    if (any(!expctd)) {
      stop("Unrecognized tables: ", paste(nms[!expctd], collapse = ", "), call. = FALSE)
    }
  }
  
  # plot_*() ------------------------------------------------------------------
  
  if (fun.name == "plot") {
    # alpha
    if (!is.null(fun.args$alpha)) {
      if (!(fun.args$alpha <= 1 & fun.args$alpha > 0)) {
        stop("Intput 'alpha' should be 0-1.", call. = FALSE)
      }
    }
    # time_window_size
    if (!is.null(fun.args$time_window_size)) {
      if (!fun.args$time_window_size %in% c("day", "month", "year")) {
        stop('Intput "time_window_size" must be "day", "month",or "year"', call. = FALSE)
      }
    }
    # facet_scales
    if (!is.null(fun.args$facet_scales)) {
      if (!fun.args$facet_scales %in% c("free", "fixed", "free_x", "free_y")) {
        stop('Intput "facet_scales" must be "free", "fixed", "free_x", or ',
             '"free_y"', call. = FALSE)
      }
    }
  }
  
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

    # num_years
    
    if (!is.null(fun.args$num_years)) {
      if (!is.numeric(fun.args$num_years)) {
        stop("Input 'years' must be of class 'numeric'.", call. = F)
      }
      if (length(fun.args$num_years) != 2) {
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
  
  # validate_data() -------------------------------------------------------
  
  if (fun.name == "validate_data") {
    # path
    if (!is.null(fun.args$path)) {
      if (!dir.exists(fun.args$path)) {                                     # exists
        stop("Input 'path' doesn't exits.", call. = F)
      }
    }
    # dataset
    if (!is.null(fun.args$dataset)) {
      if (detect_data_type(fun.args$dataset) == "dataset") {
        validate_dataset_structure(fun.args$dataset)
      } else if (detect_data_type(fun.args$dataset) == "dataset_old") {
        validate_dataset_structure_old(fun.args$dataset)
      }
    }
  }
  
  # read_data() ---------------------------------------------------------------
  
  if (fun.name == "read_data") {
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
  
  # save_data() ---------------------------------------------------------------
  
  if (fun.name == "save_data") {
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
    # dataset
    if (!is.null(fun.args$dataset)) {
      frmt <- suppressWarnings(detect_data_type(fun.args$dataset))
      if (frmt %in% c("dataset_old", "list_of_dataset_old")) {
        stop('Input "dataset" format is deprecated. Use the new format instead.',
             call. = FALSE)
      }
    }
  }
  
}






# Validate data package/product identifier
#
# @param id
#     (character) A data package/product identifier for an ecocomDP dataset.
#     
# @details 
#     If invalid (i.e. not listed in the return of \code{search_data()}), then
#     an error is returned.
# 
#     If the exact \code{id} is not indexed, but it is an EDI data package, 
#     then a set of logic determines if a newer version is indexed and 
#     available.
# 
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








# Validate site name (for NEON data products only)
#
# @param site 
#     (character; NEON data only) A character vector of site codes to filter 
#     data on. Sites are listed in the "sites" column of the 
#     \code{search_data()} output.
# @param id
#     (character) A data package/product identifier.
#     
# @details 
#     If invalid (i.e. not listed in the return of \code{search_data()}), then
#     an error is returned.
# 
validate_site <- function(site, id) {
  search_index <- suppressMessages(search_data())
  available_sites <- unlist(
    stringr::str_split(
      search_index$sites[search_index$id == id], 
      ","))
  site_exists <- site %in% available_sites
  if (!all(site_exists)) {
    warning("Sites not available in ", id, ": ", paste(site[!site_exists], collapse = ", "))
  }
}








# Validate dataset structure
#
# @param dataset (list) Data object returned by \code{read_data()} (? list of named datapackage$tables)
#
# @details Returns an error if \code{dataset} is malformed
#
validate_dataset_structure <- function(dataset) {
  criteria <- read_criteria()
  res <- c()
  res <- c(res, tryCatch(is.list(dataset), error = function(cond) {FALSE})) # is a list
  res <- c(res, tryCatch(is.character(dataset$id), error = function(cond) {FALSE})) # has id
  res <- c(res, tryCatch("tables" %in% names(dataset), error = function(cond) {FALSE})) # has tables
  res <- c(res, tryCatch(all(names(dataset$tables) %in% unique(criteria$table)), error = function(cond) {FALSE})) # table names are valid
  res <- c(res, tryCatch(all(unlist(lapply(dataset$tables, is.data.frame))), error = function(cond) {FALSE})) # tables are data.frames
  if (!all(res)) {
    stop("Input 'dataset' has invalid structure. See return from read_data() ",
         "for more details.", call. = F)
  }
}







# Validate dataset structure old
#
# @param dataset (list) Data object returned by \code{read_data()} (? list of named datapackage$tables)
#
# @details Returns an error if \code{dataset} is malformed
#
validate_dataset_structure_old <- function(dataset) {
  # TODO Remove this function after 2022-10-18
  criteria <- read_criteria()
  res <- c()
  res <- c(res, tryCatch(is.list(dataset), error = function(cond) {FALSE})) # is a list
  res <- c(res, tryCatch(is.character(names(dataset)), error = function(cond) {FALSE})) # has id
  res <- c(res, tryCatch("tables" %in% names(dataset[[1]]), error = function(cond) {FALSE})) # has tables
  res <- c(res, tryCatch(all(names(dataset[[1]]$tables) %in% unique(criteria$table)), error = function(cond) {FALSE})) # table names are valid
  res <- c(res, tryCatch(all(unlist(lapply(dataset[[1]]$tables, is.data.frame))), error = function(cond) {FALSE})) # tables are data.frames
  if (!all(res)) {
    stop("Input 'dataset' has invalid structure. See return from read_data() ",
         "for more details.", call. = F)
  }
}








# Check for expected func names and arguments in create_ecocomDP.R
#
# @param path (character) Full path, including file extension, to create_ecocomDP.R
#
# @return Errors if expected content is missing
# 
check_script <- function(path) {
  # Get script header
  funclines <- paste(readLines(path, warn = FALSE), 
                     collapse = " ")
  func_header <- stringr::str_extract(
    funclines, 
    "create_ecocomDP[:space:]*<-[:space:]*function\\({1}([^\\)]+)(?=\\))")
  # Test: Has expected func name
  if (!stringr::str_detect(func_header, "create_ecocomDP")) {
    stop("The script 'create_ecocomDP.R' does not contain the function ",
         "create_ecocomDP().", call. = FALSE)
  }
  # Test: Has expected arguments
  expected_args <- c("path", "source_id", "derived_id", "url")
  found_args <- unlist(stringr::str_extract_all(func_header, "path|source_id|derived_id|url"))
  missing_args <- !(expected_args %in% found_args)
  if (any(missing_args)) {
    stop("The function create_ecocomDP() in the 'script' create_ecocomDP.R", 
         " is missing these arguments: ",
         paste(expected_args[missing_args], collapse = ", "), 
         call. = FALSE)
  }
}
