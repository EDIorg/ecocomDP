#' Read published data
#'
#' @param id (character) Identifier of dataset to read. Identifiers are listed in the "id" column of the \code{search_data()} output. Older versions of datasets can be read, but a warning is issued.
#' @param parse_datetime (logical) Parse datetime values if TRUE, otherwise return as character strings.
#' @param unique_keys (logical) Whether to create globally unique primary keys (and associated foreign keys). Useful in maintaining referential integrity when working with multiple datasets. If TRUE, \code{id} is appended to each table's primary key and associated foreign key. Default is FALSE.
#' @param site (character) For NEON data, a character vector of site codes to filter data on. Sites are listed in the "sites" column of the \code{search_data()} output. Defaults to "all", meaning all sites.
#' @param startdate (character) For NEON data, the start date to filter on in the form YYYY-MM. Defaults to NA, meaning all available dates.
#' @param enddate (character) For NEON data, the end date to filter on in the form YYYY-MM. Defaults to NA, meaning all available dates.
#' @param package (character) For NEON data, either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param check.size (logical) For NEON data, should the user approve the total file size before downloading? Defaults to FALSE.
#' @param nCores (integer) For NEON data, the number of cores to parallelize the stacking procedure. Defaults to 1.
#' @param forceParallel (logical) For NEON data, if the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Defaults to FALSE.
#' @param token (character) For NEON data, a user specific API token (generated within neon.datascience user accounts).
#' @param neon.data.save.dir (character) For NEON data, an optional and experimental argument (i.e. may not be supported in future releases), indicating the directory where NEON source data should be saved upon download from the NEON API. Data are downloaded using \code{neonUtilities::loadByProduct()} and saved in this directory as an .rds file. The filename will follow the format <NEON data product ID>_<timestamp>.rds
#' @param neon.data.read.path (character) For NEON data, an optional and experimental argument (i.e. may not be supported in future releases), defining a path to read in an .rds file of 'stacked NEON data' from \code{neonUtilities::loadByProduct()}. See details below for more information.
#' @param ... For NEON data, other arguments to \code{neonUtilities::loadByProduct()}
#' @param from (character) Full path of file to be read (if .rds), or path to directory containing saved datasets (if .csv).
#' @param format (character) Format of returned object, which can be: "new" (the new implementation) or "old" (the original implementation; deprecated). In the new format, the top most level of nesting containing the "id" field has been moved to the same level as the "tables", "metadata", and "validation_issues" fields.
#'     
#' @return (list) A dataset with the structure:
#' \itemize{
#'   \item id - Dataset identifier
#'   \item metadata - List of info about the dataset. NOTE: This object is underdevelopment and content may change in future releases.
#'   \item tables - List of dataset tables as data.frames.
#'   \item validation_issues - List of validation issues. If the dataset fails any validation checks, then descriptions of each issue are listed here.
#' }
#' 
#' @note This function may not work between 01:00 - 03:00 UTC on Wednesdays due to regular maintenance of the EDI Data Repository.
#' 
#' @details 
#'     Validation checks are applied to each dataset ensuring it complies with the ecocomDP model. A warning is issued when any validation checks fail. All datasets are returned, even if they fail validation.
#'     
#'     Column classes are coerced to those defined in the ecocomDP specification.
#'     
#'     Validation happens each time files are read, from source APIs or local environments.
#'     
#'     Details for \code{read_data()} function regarding NEON data: Using this function to read data with an \code{id} that begins with "neon.ecocomdp" will result in a query to download NEON data from the NEON Data Portal API using \code{neonUtilities::loadByProduct()}. If a query includes provisional data (or if you are not sure if the query includes provisional data), we recommend saving a copy of the data in the original format provided by NEON in addition to the derived ecocomDP data package. To do this, provide a directory path using the \code{neon.data.read.path} argument. For example, the query \code{my_ecocomdp_data <- read_data(id = "neon.ecocomdp.10022.001.001", neon.data.save.dir = "my_neon_data")} will download the data for NEON Data Product ID DP1.10022.001 (ground beetles in pitfall traps) and convert it to the ecocomDP data model. In doing so, a copy of the original NEON download will be saved in the directory "my_ neon_data with the filename 
#'     "DP1.10022.001_<timestamp>.RDS" and the derived data package in the ecocomDP format will be stored in your R environment in an object named "my_ecocomdp_data". Further, if you wish to reload a previously downloaded NEON dataset into the ecocomDP format, you can do so using \code{my_ecocomdp_data <- read_data(id = "neon.ecocomdp.10022.001.001", neon.data.read.path = 
#'     "my_neon_data/DP1.10022.001_<timestamp>.RDS")}
#'     
#'     Provisional NEON data. Despite NEON's controlled data entry, at times, errors are found in published data; for example, an analytical lab may adjust its calibration curve and re-calculate past analyses, or field scientists may discover a past misidentification. In these cases, Level 0 data are edited and the data are re-processed to Level 1 and re-published. Published data files include a time stamp in the file name; a new time stamp indicates data have been re-published and may contain differences from previously published data. Data are subject to re-processing at any time during an initial provisional period; data releases are never re-processed. All records downloaded from the NEON API will have a "release" field. For any provisional record, the value of this field will be "PROVISIONAL", otherwise, this field will have a value indicating the version of the release to which the record belongs. More details can be found at https://www.neonscience.org/data-samples/data-management/data-revisions-releases.
#'     
#' @export
#' 
#' @examples
#' \dontrun{
#' # Read from EDI
#' dataset <- read_data("edi.193.5")
#' str(dataset, max.level = 2)
#' 
#' # Read from NEON (full dataset)
#' dataset <- read_data("neon.ecocomdp.20120.001.001")
#' 
#' # Read from NEON with filters (partial dataset)
#' dataset <- read_data(
#'  id = "neon.ecocomdp.20120.001.001", 
#'  site = c("COMO", "LECO", "SUGG"),
#'  startdate = "2017-06", 
#'  enddate = "2019-09",
#'  check.size = FALSE)
#' 
#' # Read with datetimes as character
#' dataset <- read_data("edi.193.5", parse_datetime = FALSE)
#' is.character(dataset$tables$observation$datetime)
#' 
#' # Read from saved .rds
#' save_data(dataset, tempdir())
#' dataset <- read_data(from = paste0(tempdir(), "/dataset.rds"))
#' 
#' # Read from saved .csv
#' save_data(dataset, tempdir(), type = ".csv")# Save as .csv
#' dataset <- read_data(from = tempdir())
#' }
#' 
read_data <- function(id = NULL, parse_datetime = TRUE, 
                      unique_keys = FALSE, site = "all", 
                      startdate = NA, enddate = NA, package = "basic", 
                      check.size = FALSE, nCores = 1, forceParallel = FALSE, 
                      token = NA, neon.data.save.dir = NULL, 
                      neon.data.read.path = NULL, ..., from = NULL,
                      format = "new") {
  
  # Validate input arguments --------------------------------------------------

  validate_arguments(fun.name = "read_data", fun.args = as.list(environment()))

  # Parameterize --------------------------------------------------------------

  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]

  # Read ----------------------------------------------------------------------
  
  if (is.null(from)) { # From API
    if (stringr::str_detect(            # EDI
      id, 
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)") && 
      !grepl("^neon\\.", id)) {
      d <- read_data_edi(id, parse_datetime)
    } else if (grepl("^neon\\.", id)) { # NEON
      d <- map_neon_data_to_ecocomDP(
        id = id, 
        site = site, 
        startdate = startdate,
        enddate = enddate, 
        check.size = check.size, 
        nCores = nCores, 
        forceParallel = forceParallel,
        token = token, 
        neon.data.save.dir = neon.data.save.dir,
        neon.data.read.path = neon.data.read.path,
        ...)
    }
    d <- list(d = d)
    names(d) <- id
  } else {                  # From file
    d <- read_from_files(from)
  }
  
  # Modify --------------------------------------------------------------------
  
  # Add missing columns
  
  for (x in names(d)) {
    for (y in names(d[[x]]$tables)) {
      nms <- attr_tbl$column[attr_tbl$table == y]
      tblnms <- names(d[[x]]$tables[[y]])
      if ("observation_datetime" %in% tblnms) { # accommodate legacy data by converting observation_datetime to datetime
        tblnms[tblnms == "observation_datetime"] <- "datetime"
        names(d[[x]]$tables[[y]]) <- tblnms
      }
      if ((y == "observation_ancillary") & ("event_id" %in% tblnms)) { # Warn if legacy data linking observation_ancillary through the event_id
        warning("This dataset conforms to an older version of the ecocomDP model in which the observation_ancillary table is linked through the event_id. No validation checks will be applied to the observation_ancillary table.", call. = FALSE)
        event_id <- d[[x]]$tables[[y]]$event_id
      }
      use_i <- setdiff(nms, tblnms)
      if (length(use_i) > 0) {
        d[[x]]$tables[[y]][use_i] <- NA
        # There seems to be an incompatibility in the handling of 
        # ..nms between Mac and Windows Os
        msg <- try(
          d[[x]]$tables[[y]] <- d[[x]]$tables[[y]][ , ..nms], 
          silent = TRUE)
        if (attr(msg, "class") == "try-error") {
          d[[x]]$tables[[y]] <- d[[x]]$tables[[y]][ , nms]
        }
      }
    }
  }

  # Coerce column classes to ecocomDP specifications. NOTE: This same 
  # process is applied to read_from_files(). Any update here should be
  # duplicated there. A function is not used in order to minimize data object
  # copies.
  
  for (x in names(d)) {
    for (y in names(d[[x]]$tables)) {
      for (z in names(d[[x]]$tables[[y]])) {
        detected <- class(d[[x]]$tables[[y]][[z]])
        expected <- attr_tbl$class[(attr_tbl$table == y) & (attr_tbl$column == z)]
        if (any(detected %in% c("POSIXct", "POSIXt", "Date", "IDate"))) {
          detected <- "Date" # so downstream logic doesn't throw length() > 1 warnings
        }
        if (isTRUE(parse_datetime) & (expected == "Date") & (detected == "character" | detected == "logical")) { # NAs should be datetime for consistency
          d[[x]]$tables[[y]][[z]] <- lubridate::as_date(d[[x]]$tables[[y]][[z]])
        }
        if (detected != expected) {
          if (expected == 'character'){
            d[[x]]$tables[[y]][[z]] <- as.character(d[[x]]$tables[[y]][[z]])
          } else if (expected == 'numeric'){
            d[[x]]$tables[[y]][[z]] <- as.numeric(d[[x]]$tables[[y]][[z]])
          }
        }
      }
    }
  }
  
  # Append package_id to primary keys to ensure referential integrity (except
  # package_id, appending package_id to package_id changes the field definition
  # and shouldn't be necessary as the package_id is very unlikely to be 
  # duplicated).
  
  if (isTRUE(unique_keys)) {
    for (x in names(d)) {
      for (y in names(d[[x]]$tables)) {
        for (z in names(d[[x]]$tables[[y]])) {
          if (stringr::str_detect(z, "_id")) {
            if (!(z %in% c("package_id", "original_package_id", 
                           "mapped_id", "authority_taxon_id", 
                           "parent_location_id"))) {
              d[[x]]$tables[[y]][[z]] <- paste0(
                d[[x]]$tables[[y]][[z]], "_", x)
            } else if (z == "parent_location_id") {
              use_i <- is.na(d[[x]]$tables[[y]][[z]])
              d[[x]]$tables[[y]][[z]] <- paste0(
                d[[x]]$tables[[y]][[z]], "_", x)
              d[[x]]$tables[[y]][[z]][use_i] <- NA_character_
            }
          }
        }
      }
    }
  }
  
  # Return datetimes as character
  if (!isTRUE(parse_datetime)) {
    for (id in names(d)) {
      for (tbl in names(d[[id]]$tables)) {
        dtcols <- stringr::str_detect(colnames(d[[id]]$tables[[tbl]]), "datetime")
        if (any(dtcols)) {
          colname <- colnames(d[[id]]$tables[[tbl]])[dtcols]
          vals <- as.character(d[[id]]$tables[[tbl]][[colname]])
          d[[id]]$tables[[tbl]][[colname]] <- vals
        }
      }
    }
  }

  # Control returned structure ------------------------------------------------
  
  if (format == "new") {
    if (suppressWarnings(detect_data_type(d)) == "dataset_old")  {
      d <- c(id = names(d), d[[1]])
    } else if (detect_data_type(d) == "list_of_datasets") {
      for (i in 1:length(d)) {
        d[[i]] <- c(id = names(d[i]), d[[i]])
      }
      d <- unname(d)
    }
  } else if (format == "old") {
    warning('The old format is deprecated. Please use the new format instead.',
            call. = FALSE)
  }
  
  # Validate ------------------------------------------------------------------

  callstack <- as.character(sys.calls())
  if (!any(stringr::str_detect(callstack, "validate_data\\("))) { # don't validate if read_data() is called from validate()
    if (format == "new") {
      if (detect_data_type(d) == "dataset")  {
        d$validation_issues <- validate_data(dataset = d)
      } else if (detect_data_type(d) == "list_of_datasets") {
        for (i in 1:length(d)) {
          d[[i]]$validation_issues <- validate_data(dataset = d[[i]])
        }
      }
    } else if (format == "old") {
      # Validation only runs on the new format, so fake it and assign issues to the return object
      # TODO: Remove this 2022-10-18
      if (detect_data_type(d) == "dataset_old")  {
        mock_new <- d
        mock_new[[1]]$id <- names(d)
        mock_new <- mock_new[[1]]
        d[[1]]$validation_issues <- validate_data(dataset = mock_new)
      } else if (detect_data_type(d) == "list_of_datasets_old") {
        for (i in 1:length(d)) {
          d[[i]]$validation_issues <- validate_data(dataset = d[[i]])
        }
      }
    }
  }
  
  # Return --------------------------------------------------------------------

  return(d)
  
}








# Read an ecocomDP dataset from EDI
#
# @param id
#     (character) Data package identifier with revision number
# @param parse_datetime
#     (logical) Attempt to parse datetime character strings through an algorithm that looks at the EML formatString value and 
#     calls \code{lubridate::parse_date_time()} with the appropriate \code{orders}. Failed attempts will return a warning.
#
# @return
#     (list) Named list of data tables
#     
read_data_edi <- function(id, parse_datetime = TRUE) {
  
  message("Reading ", id)
  
  # Parameterize
  if (exists("config.environment", envir = .GlobalEnv)) {
    config.environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    config.environment <- "production"
  }
  
  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- read_criteria()
  
  # Get table metadata for reading
  
  tbl_attrs <- lapply(
    seq_along(vector('list', length(unique(attr_tbl$table)))), 
    function(x){
      list(name = NULL, url = NULL, delimiter = NULL, nrecord = NULL)
    })
  names(tbl_attrs) <- unique(attr_tbl$table)
  
  xpath <- c(
    name = './/physical/objectName',
    url = './/physical/distribution/online/url',
    delimiter = './/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter',
    nrecord = './/numberOfRecords',
    formatString = './/dateTime/formatString')
  
  eml <- suppressMessages(
    api_read_metadata(id, environment = config.environment))
  
  for (x in names(tbl_attrs)) {
    tblnames <- xml2::xml_text(xml2::xml_find_all(eml, './/dataset/dataTable/physical/objectName'))
    use_i <- stringr::str_detect(tblnames, paste0(x, '\\.[:alnum:]*$'))
    if (any(use_i)) {
      for (k in names(xpath)) {
        nodeset <- xml2::xml_find_all(eml, ".//dataset/dataTable")[use_i]
        val <- xml2::xml_text(xml2::xml_find_all(nodeset, xpath[[k]]))
        if (length(val) == 0) {
          val <- NA
        }
        tbl_attrs[[x]][[k]] <- val
      }
    } else {
      tbl_attrs[[x]] <- NULL
    }
  }
  
  # Read tables
  output <- lapply(
    names(tbl_attrs),
    function(x) {
      res <- data.table::fread(
        tbl_attrs[[x]]$url)
      res <- as.data.frame(res)
      return(res)
    })
  names(output) <- names(tbl_attrs)
  
  # Parse datetime
  for (tbl in names(output)) {
    frmtstr <- tbl_attrs[[tbl]]$formatString
    if (!is.na(frmtstr)) {
      dtcol <- stringr::str_subset(colnames(output[[tbl]]), "datetime")
      if (isTRUE(parse_datetime)) {
        parsed <- parse_datetime_from_frmt(tbl = tbl, vals = output[[tbl]][[dtcol]], frmt = frmtstr)
        output[[tbl]][[dtcol]] <- parsed
      }
    }
  }
  
  # Parse metadata
  meta = list(url = paste0("https://portal.edirepository.org/nis/mapbrowse?packageid=", id))
  
  # Return
  res <- list(metadata = meta, tables = output)
  return(res)
}








# Read ecocomDP from files TO BE USED IN CREATION PROCESS PRIOR TO ARCHIVING
#
# @param data.path 
#     (character) The path to the directory containing ecocomDP tables. 
#     Duplicate file names are not allowed.
#
# @return
#     (list) A named list of \code{id}, each including: 
#     \item{metadata}{A list of information about the data.}
#     \item{tables}{A list of tbl_df, tbl, data.frames following the ecocomDP format 
#     (\url{https://github.com/EDIorg/ecocomDP/blob/master/documentation/model/table_visualization.md})}
# 
read_from_files <- function(data.path) {
  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  fileext <- tools::file_ext(data.path)
  if (fileext == "rds") {        # rds
    d <- readRDS(data.path)
  } else if (fileext != "rds") { # dir ... note NEON ids cause file_ext() to produce misleading results
    dirs <- list.dirs(data.path)
    parent_dir_has_tables <- any( # does the parent dir have any L1 tables?
      unlist(
        lapply(
          unique(attr_tbl$table),
          function(x) {
            ecocomDP_table <- stringr::str_detect(
              tools::file_path_sans_ext(list.files(dirs[1])), 
              paste0("^", x, "$"))
            return(any(ecocomDP_table))
          })))
    if (parent_dir_has_tables) {                              # Don't look in subdirs if parent has tables
      dirs <- dirs[1]
      d <- read_dir(dirs)
    } else if (!parent_dir_has_tables & (length(dirs) > 1)) { # Identify subdirs with tables
      dirs <- dirs[-1]
      i <- unlist(
        lapply(
          dirs,
          function(dirc) {
            r <- any(
              unlist(
                lapply(
                  unique(attr_tbl$table),
                  function(x) {
                    ecocomDP_table <- stringr::str_detect(
                      tools::file_path_sans_ext(list.files(dirc)), 
                      paste0("^", x, "$"))
                    return(any(ecocomDP_table))
                  })))
            return(r)
          }))
      dirs_w_tables <- dirs[i]
      d <- read_dir(dirs_w_tables)
      return(d)
    } else {
      stop("No identifiable L1 tables at ", data.path, call. = FALSE)
      d <- NULL
    }
  }
  
  # Downstream code of read_data() use the "old" format, so need to convert
  # back to it here
  # TODO: Remove this block of code on 2022-10-18
  if (suppressWarnings(detect_data_type(d)) == "dataset") {
    id <- d$id
    d$id <- NULL
    d <- list(d)
    names(d) <- id
  } else if (suppressWarnings(detect_data_type(d)) == "list_of_datasets") {
    ids <- c()
    for (i in 1:length(d)) {
      ids <- c(ids, d[[i]]$id)
      d[[i]]$id <- NULL
    }
    names(d) <- ids
  }
  
  return(d)
}








# Read L1 tables in path
#
# @param paths (character) One or more paths to a directories
#
# @return (list) The L1 dataset object
# 
read_dir <- function(paths) {
  attr_tbl <- read_criteria()
  d <- lapply(
    paths,
    function(path) {
      res <- lapply(
        unique(attr_tbl$table),
        function(x) {
          ecocomDP_table <- stringr::str_detect(
            list.files(path), 
            paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
          if (any(ecocomDP_table)) {
            res <- data.table::fread(
              paste0(path, "/", list.files(path)[ecocomDP_table]))
            # parse datetime
            if ("datetime" %in% colnames(res)) {
              frmt <- parse_datetime_frmt_from_vals(res$datetime)
              if (!is.null(frmt)) {
                res$datetime <- parse_datetime_from_frmt(tbl = x, 
                                                         vals = res$datetime, 
                                                         frmt = frmt)
              }
            }
            res <- as.data.frame(res)
            return(res)
          }
        })
      names(res) <- unique(attr_tbl$table)
      res[sapply(res, is.null)] <- NULL
      res <- list(
        list(metadata = NULL, tables = res))
      return(res)
    })
  d <- unlist(d, recursive = FALSE)
  if (length(paths) != 0){ # Get id from dir if nested (use case of reading from save_data(..., type = .csv))
    names(d) <- basename(paths)
  } else {
    package_id <- d[[1]]$tables$dataset_summary$package_id
    if (is.na(package_id) | (package_id == "")) {
      names(d) <- "unknown"
    } else {
      names(d) <- package_id
    }
  }
  return(d)
}