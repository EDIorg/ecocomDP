#' Read ecocomDP data
#'
#' @param id
#'     (character) Identifier of dataset to read. Identifiers are listed in the "id" column of the \code{search_data()} output. Older versions of ids listed in the "id" column of the \code{search_data()} output are supported, but a warning is issued.
#' @param path
#'     (character) Path to the directory in which the data will be read.
#' @param parse.datetime
#'     (logical) Attempt to parse datetime character strings through an algorithm. For EDI, the algorithm looks at the EML formatString value and calls \code{lubridate::parse_date_time()} with the appropriate \code{orders}. For NEON, the algorithm iterates through permutations of \code{ymd HMS} orders. Failed attempts will return a warning. Default is \code{TRUE}. No attempt is made at using time zones if included (these are dropped before parsing).
#' @param globally.unique.keys
#'     (logical) Whether to create globally unique primary keys (and associated
#'     foreign keys). Reading multiple datasets raises the issue of referential 
#'     integrity across datasets. If TRUE, \code{id} is appended to each 
#'     table's primary key and associated foreign key. Defaults to FALSE.
#' @param site 
#'     (character; NEON data only) A character vector of site codes to filter 
#'     data on. Sites are listed in the "sites" column of the 
#'     \code{search_data()} output. Defaults to "all", meaning all sites.
#' @param startdate
#'     (character; NEON data only) Start date to filter on in the form YYYY-MM. 
#'     Defaults to NA, meaning all available dates.
#' @param enddate 
#'     (character; NEON data only) End date to filter on in the form YYYY-MM. 
#'     Defaults to NA, meaning all available dates.
#' @param package
#'     (character; NEON data only) Either 'basic' or 'expanded', indicating 
#'     which data package to download. Defaults to basic.
#' @param check.size 
#'     (logical; NEON data only) Should the user approve the total file size 
#'     before downloading? Defaults to FALSE.
#' @param nCores
#'     (integer; NEON data only) The number of cores to parallelize the 
#'     stacking procedure. Defaults to 1.
#' @param forceParallel 
#'     (logical; NEON data only) If the data volume to be processed does not 
#'     meet minimum requirements to run in parallel, this overrides. Defaults 
#'     to FALSE.
#' @param token 
#'     (character; NEON data only) User specific API token (generated within 
#'     neon.datascience user accounts)
#' @param ...
#'     (NEON data only) Other arguments to \code{neonUtilities::loadByProduct()}
#' @param from.file
#'     (character) Full path to file to be read. Supported options are returned by \code{save_data()}
#'     
#'     
#' @return
#'     (list) with the structure: 
#'     \itemize{
#'       \item{id - Dataset id}
#'         \itemize{
#'           \item metadata - Info about the dataset. NOTE: This object is underdevelopment and content may change in future releases
#'           \item tables - Dataset tables
#'           \item validation_issues - If the dataset fails any validation checks, 
#'     then they are listed here as a vector of character strings describing 
#'     each issue.
#'       }
#'     }
#'     
#' @details 
#'     Validation checks are applied to each dataset ensuring they comply with 
#'     the ecocomDP. A warning is issued when any validation check fails 
#'     (please report these to \url{https://github.com/EDIorg/ecocomDP/issues}). 
#'     All datasets are returned, even if they fail validation.
#'     
#'     Column classes are coerced to those defined in the ecocomDP 
#'     specification.
#'     
#' @export
#' 
#' @examples
#' \dontrun{
#' # Read a dataset
#' d <- read_data("edi.193.3")
#' 
#' # REad a NEON dataset without filters
#' d <- read_data("neon.ecocomdp.20166.001.001")
#' 
#' # Read a NEON dataset with filters
#' d <- read_data(
#'   id = "neon.ecocomdp.20166.001.001", 
#'   site = c("MAYF", "PRIN"), 
#'   startdate = "2016-01", 
#'   enddate = "2018-11")
#' }
#' 
read_data <- function(id = NULL, path = NULL, parse.datetime = TRUE, 
                      globally.unique.keys = FALSE, site = "all", 
                      startdate = NA, enddate = NA, package = "basic", 
                      check.size = FALSE, nCores = 1, forceParallel = FALSE, 
                      token = NA, ..., from.file = NULL) {
  
  # Validate input arguments --------------------------------------------------

  # fun.args <- validate_arguments("read_data", as.list(environment()))
  # id <- fun.args$id
  # path <- fun.args$path
  # file.type <- fun.args$file.type
  # site <- fun.args$site
  # startdate <- fun.args$startdate
  # enddate <- fun.args$enddate
  # check.size <- fun.args$check.size
  # nCores <- fun.args$nCores
  # token <- fun.args$token
  # forceParallel <- fun.args$forceParallel

  # Parameterize --------------------------------------------------------------

  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]

  # Read ----------------------------------------------------------------------
  
  if (is.null(from.file)) {      # From API
    if (stringr::str_detect(
      id, 
      "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)") && 
      !grepl("^neon\\.", id)) {
      d <- read_data_edi(id, parse.datetime)
      
    } else if (grepl("^neon\\.", id)) {
      d <- map_neon_data_to_ecocomDP(
        id = id, 
        site = site, 
        startdate = startdate,
        enddate = enddate, 
        check.size = check.size, 
        nCores = nCores, 
        forceParallel = forceParallel,
        token = token, 
        ...)
    }
    d <- list(d = d)
    names(d) <- id
  } else {                       # From file
    d <- read_from_files(from.file, parse.datetime)
  }
  
  # Modify --------------------------------------------------------------------
  
  # Add missing columns

  invisible(
    lapply(
      names(d),
      function(x) {
        lapply(
          names(d[[x]]$tables),
          function(y) {
            nms <- attr_tbl$column[attr_tbl$table == y]
            use_i <- setdiff(nms, names(d[[x]]$tables[[y]]))
            if (length(use_i) > 0) {
              d[[x]]$tables[[y]][use_i] <<- NA
              # There seems to be an incompatibility in the handling of 
              # ..nms between Mac and Windows Os
              msg <- try(
                d[[x]]$tables[[y]] <<- d[[x]]$tables[[y]][ , ..nms], 
                silent = TRUE)
              if (attr(msg, "class") == "try-error") {
                d[[x]]$tables[[y]] <<- d[[x]]$tables[[y]][ , nms]
              }
            }
          })
      }))

  # Coerce column classes to ecocomDP specifications. NOTE: This same 
  # process is applied to read_from_files(). Any update here should be
  # duplicated there. A function is not used in order to minimize data object
  # copies.

  invisible(
    lapply(
      names(d),
      function(x){
        lapply(
          names(d[[x]]$tables),
          function(y) {
            lapply(
              names(d[[x]]$tables[[y]]),
              function(z) {
                detected <- class(d[[x]]$tables[[y]][[z]])
                expected <- attr_tbl$class[(attr_tbl$table == y) & (attr_tbl$column == z)]
                if (isTRUE(parse.datetime) & (expected == "Date") & is.character(detected)) { # NAs should be datetime for consistency
                  d[[x]]$tables[[y]][[z]] <<- lubridate::as_date(d[[x]]$tables[[y]][[z]])
                }
                if (any(detected %in% c("POSIXct", "POSIXt", "Date", "IDate"))) {
                  detected <- "Date" # so downstream logic doesn't throw length() > 1 warnings
                }
                if (detected != expected) {
                  if (expected == 'character'){
                    d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                  } else if (expected == 'numeric'){
                    d[[x]]$tables[[y]][[z]] <<- as.numeric(d[[x]]$tables[[y]][[z]])
                  }
                }
              })
          })
      }))
  
  # Append package_id to primary keys to ensure referential integrity (except
  # package_id, appending package_id to package_id changes the field definition
  # and shouldn't be neccessary as the package_id is very unlikely to be 
  # duplicated).
  
  if (isTRUE(globally.unique.keys)) {
    
    invisible(
      lapply(
        names(d),
        function(x){
          lapply(
            names(d[[x]]$tables),
            function(y) {
              lapply(
                names(d[[x]]$tables[[y]]),
                function(z) {
                  if (stringr::str_detect(z, "_id")) {
                    if (!(z %in% c("package_id", "original_package_id", 
                                   "mapped_id", "authority_taxon_id", 
                                   "parent_location_id"))) {
                      d[[x]]$tables[[y]][[z]] <<- paste0(
                        d[[x]]$tables[[y]][[z]], "_", x)
                    } else if (z == "parent_location_id") {
                      use_i <- is.na(d[[x]]$tables[[y]][[z]])
                      d[[x]]$tables[[y]][[z]] <<- paste0(
                        d[[x]]$tables[[y]][[z]], "_", x)
                      d[[x]]$tables[[y]][[z]][use_i] <<- NA_character_
                    }
                  }
                })
            })
        }))
    
  }
  
  # Return datetimes as character
  if (!isTRUE(parse.datetime)) {
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
 
  # Validate ------------------------------------------------------------------

  for (i in 1:length(d)) {
    d[[i]]$validation_issues <- validate_ecocomDP(data.list = d[[i]]$tables)
  }
  
  # Return --------------------------------------------------------------------

  return(d)
  
}








#' Read an ecocomDP dataset from EDI
#'
#' @param id
#'     (character) Data package identifier with revision number
#' @param parse.datetime
#'     (logical) Attempt to parse datetime character strings through an algorithm that looks at the EML formatString value and calls \code{lubridate::parse_date_time()} with the appropriate \code{orders}. Failed attempts will return a warning.
#'
#' @return
#'     (list) Named list of data tables
#'     
read_data_edi <- function(id, parse.datetime = TRUE) {
  
  message("Reading ", id)
  
  # Parameterize
  if (exists("config.environment", envir = .GlobalEnv)) {
    config.environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    config.environment <- "production"
  }
  
  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
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
  
  invisible(
    lapply(
      names(tbl_attrs),
      function(x) {
        tblnames <- xml2::xml_text(xml2::xml_find_all(eml, './/dataset/dataTable/physical/objectName'))
        use_i <- stringr::str_detect(tblnames, paste0(x, '\\.[:alnum:]*$'))
        if (any(use_i)) {
          lapply(
            names(xpath),
            function(k){
              nodeset <- xml2::xml_find_all(eml, ".//dataset/dataTable")[use_i]
              val <- xml2::xml_text(xml2::xml_find_all(nodeset, xpath[[k]]))
              if (length(val) == 0) {
                val <- NA
              }
              tbl_attrs[[x]][[k]] <<- val
            })
        } else {
          tbl_attrs[[x]] <<- NULL
        }
      }))
  
  # Read tables
  output <- lapply(
    names(tbl_attrs),
    function(x) {
      data.table::fread(
        tbl_attrs[[x]]$url)
    })
  names(output) <- names(tbl_attrs)
  
  # Parse datetime
  for (tbl in names(output)) {
    frmtstr <- tbl_attrs[[tbl]]$formatString
    if (!is.na(frmtstr)) {
      dtcol <- stringr::str_subset(colnames(output[[tbl]]), "datetime")
      if (isTRUE(parse.datetime)) {
        parsed <- parse_datetime(tbl = tbl, vals = output[[tbl]][[dtcol]], frmt = frmtstr)
        output[[tbl]][[dtcol]] <- parsed
      }
    }
  }
  
  # Parse metadata
  search_index <- suppressMessages(search_data())
  i <- search_index$id == id
  meta = list(url = search_index$url[i])
  
  # Return
  res <- list(metadata = meta, tables = output)
  return(res)
}








#' Read ecocomDP from files TO BE USED IN CREATION PROCESS PRIOR TO ARCHIVING
#'
#' @param data.path 
#'     (character) The path to the directory containing ecocomDP tables. 
#'     Duplicate file names are not allowed.
#' @param parse.datetime
#'     (logical) Attempt to parse datetime character strings through an algorithm. For EDI, the algorithm looks at the EML formatString value and calls \code{lubridate::parse_date_time()} with the appropriate \code{orders}. For NEON, the algorithm iterates through permutations of \code{ymd HMS} orders. Failed attempts will return a warning. Default is \code{TRUE}. No attempt is made at using time zones if included (these are dropped before parsing).
#'
#' @return
#'     (list) A named list of \code{id}, each including: 
#'     \item{metadata}{A list of information about the data.}
#'     \item{tables}{A list of data.frames following the ecocomDP format 
#'     (\url{https://github.com/EDIorg/ecocomDP/blob/master/documentation/model/table_visualization.md})}
#'
#' @examples
#' d <- read_from_files(system.file("/data", package = "ecocomDP"))
#' 
read_from_files <- function(data.path, parse.datetime) {
  # TODO: validate_arguments("read_from_files", as.list(environment()))
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  fileext <- tools::file_ext(data.path)
  if (fileext == "rds") {     # rds
    d <- readRDS(data.path)
  } else if (fileext == "") { # dir
    d <- lapply(
      unique(attr_tbl$table),
      function(x) {
        ecocomDP_table <- stringr::str_detect(
          list.files(data.path), 
          paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
        if (any(ecocomDP_table)) {
          res <- data.table::fread(
            paste0(data.path, "/", list.files(data.path)[ecocomDP_table]))
          return(res)
        }
      })
    names(d) <- unique(attr_tbl$table)
    d[sapply(d, is.null)] <- NULL
    d <- list(
      list(metadata = NULL, tables = d))
    names(d) <- basename(data.path)
  }
  return(d)
}
