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
  
  # create_*() ----------------------------------------------------------------
  
  if (stringr::str_detect(fun.name, "^create_")) {
    
    # These "create" funcs share features which can be validated together
    wide <- fun.args$L0_wide
    colmap <- fun.args[names(fun.args) %in% na.omit(criteria$column)] # Col args can be assigned any value, so we need a map
    
    # Required inputs are the intersection of func args and required table cols
    tblname <- stringr::str_remove(fun.name, "create_")
    tbl_rqrd <- criteria$column[(criteria$table %in% tblname) &        # cols required by table
                                  (criteria$required == TRUE)]
    fun_rqrd <- names(colmap)                                          # cols required by func
    cols_expctd <- unlist(colmap[base::intersect(tbl_rqrd, fun_rqrd)]) # cols expected in wide
    msng <- !(cols_expctd %in% colnames(wide))
    if (any(msng)) {
      stop("Columns not in 'L0_wide': ", paste(cols_expctd[msng], collapse = ", "), call. = FALSE)
    }
    
    # Catch datetime edge case here or will break downstream
    if (!is.null(fun.args$datetime)) {
      msng <- !(fun.args$datetime %in% colnames(wide))
      if (any(msng)) {
        stop("Columns not in 'L0_wide': ", fun.args$datetime, call. = FALSE)
      }
    }
    
    # Rename wide cols to L1 cols so output matches L1 specifications
    cols2rename <- base::setdiff(names(colmap), c("variable_name", "unit", "nesting"))
    cols_old <- unname(unlist(colmap[cols2rename]))
    cols_new <- names(unlist(colmap[cols2rename]))
    cols <- colnames(wide)
    i <- match(cols_old, cols)
    cols[i] <- cols_new
    colnames(wide) <- cols
    # update inputs
    i <- match(cols_old, colmap)
    colmap[i] <- cols_new[i]
    
    # Differentiate between defaults and user inputs. Set any not in wide to NULL. No defaults for variable_name, unit, and nesting.
    cols_expctd <- base::setdiff(names(colmap), c("variable_name", "unit", "nesting"))
    msng <- !(cols_expctd %in% colnames(wide))
    colmap[cols_expctd[msng]] <- list(NULL)

    # Unit is not required in any table so checks are here
    if ("unit" %in% names(colmap)) {
      if (!is.null(colmap$unit)) {
        msng <- !(unlist(colmap$unit) %in% colnames(wide)) # exists in wide
        if (any(msng)) {
          stop("Columns not in 'L0_wide': ", paste(unlist(colmap$unit)[msng], collapse = ", "), call. = FALSE)
        }
        hasfrmt <- stringr::str_detect(colmap$unit, "^unit_") # has the correct form
        if (!all(hasfrmt)) {
          stop("Inputs to 'unit' should have the form 'unit_<variable_name>'. These have unexpected form: ", paste(colmap$unit[!hasfrmt], collapse = ", "), call. = FALSE)
        }
        if (!is.null(colmap$variable_name)) {
          hasmatch <- stringr::str_remove_all(colmap$unit, "^unit_") %in% colmap$variable_name # has matching variable_name
          if (!all(hasmatch)) {
            stop("Inputs to 'unit' without a 'variable_name' match: ", paste(colmap$unit[!hasmatch], collapse = ", "), call. = FALSE)
          }
        }
      }
    }
    
    # Nesting is an edge case
    
    if ("nesting" %in% names(fun.args)) {
      if (is.null(fun.args$nesting)) {
        stop("Input 'nesting' is required.", call. = FALSE)
      }
      msng <- !(unlist(fun.args$nesting) %in% colnames(wide)) # exists in wide
      if (any(msng)) {
        stop("Columns not in 'L0_wide': ", paste(unlist(fun.args$nesting)[msng], collapse = ", "), call. = FALSE)
      }
    }
    
    # Return modifications to arguments of the calling function for use
    fun.args$L0_wide <- wide
    fun.args[names(colmap)] <- colmap
    list2env(fun.args, envir = parent.frame(1))

  }
  
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
      validate_dataset_structure(fun.args$dataset)                               # has expected structure
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
    warning("Sites not available in ", id, ": ", paste(site[!site_exists], collapse = ", "))
  }
}








#' Validate dataset structure
#'
#' @param dataset (list) Data object returned by \code{read_data()} (? list of named datapackage$tables)
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
    stop("Input 'dataset' has invalid structure. See return from read_data() ",
         "for more details.", call. = F)
  }
}
