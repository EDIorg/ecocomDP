#' Read ecocomDP data
#'
#' @param id
#'     (character) Identifier of dataset to read. Identifiers are listed in the 
#'     "id" column of the \code{search_data()} output.  If reading multiple 
#'     datasets, then \code{id} should be a character vector of identifiers or 
#'     a named list containing additional arguments to filter on (for NEON data 
#'     only; see below for arguments and examples).
#' @param path
#'     (character) Path to the directory in which the data will be written.
#' @param file.type
#'     (character) Type of file to save the data to. Options are: ".rda", 
#'     ".csv".
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
#' @param globally.unique.keys
#'     (logical) Whether to create globally unique primary keys (and associated
#'     foreign keys). Reading multiple datasets raises the issue of referential 
#'     integrity across datasets. If TRUE, \code{id} is appended to each 
#'     table's primary key and associated foreign key. Defaults to FALSE.
#'     
#' @return
#'     (list) A named list of \code{id}, each including: 
#'     \item{metadata}{A list of information about the data.}
#'     \item{tables}{A list of data.frames following the ecocomDP format 
#'     (\url{https://github.com/EDIorg/ecocomDP/blob/master/documentation/model/table_visualization.md})}
#'     \item{validation_issues}{If the dataset fails any validation checks, 
#'     then they are listed here as a vector of character strings describing 
#'     each issue.}
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
#' # Read a dataset
#' d <- read_data("edi.193.3")
#' d <- read_data("DP1.20166.001")
#' 
#' # Read a NEON dataset with filters
#' d <- read_data(
#'   id = "DP1.20166.001", 
#'   site = c("MAYF", "PRIN"), 
#'   startdate = "2016-1", 
#'   enddate = "2018-11")
#' 
#' # Read multiple datasets from different sources
#' d <- read_data(c("edi.193.3", "DP1.20166.001"))
#' 
#' # Read multiple datasets from EDI and NEON with NEON filters
#' d <- read_data(
#'   id = list(
#'     edi.193.3 = NULL,
#'     DP1.20166.001 = list(
#'       site = c("MAYF", "PRIN"),
#'       startdate = "2016-1",
#'       enddate = "2018-11")),
#'   check.size = TRUE,
#'   nCores = 2,
#'   forceParallel = FALSE)
#'   
read_data <- function(
  id = NULL, path = NULL, file.type = ".rda", site = "all", startdate = NA, 
  enddate = NA, check.size = FALSE, nCores = 1, forceParallel = FALSE,
  token = NA,
  globally.unique.keys = FALSE) {

  # Validate input arguments --------------------------------------------------
  
  fun.args <- validate_arguments("read_data", as.list(environment()))
  id <- fun.args$id
  path <- fun.args$path
  file.type <- fun.args$file.type
  site <- fun.args$site
  startdate <- fun.args$startdate
  enddate <- fun.args$enddate
  check.size <- fun.args$check.size
  nCores <- fun.args$nCores
  token <- fun.args$token
  forceParallel <- fun.args$forceParallel

  # Parameterize --------------------------------------------------------------

  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]

  # Read ----------------------------------------------------------------------
  
  d <- lapply(
    names(id),
    function(x) {
      if (stringr::str_detect(
        x, 
        "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
        read_data_edi(x)
      } else if (stringr::str_detect(x, "^DP.\\.[:digit:]+\\.[:digit:]+")) {
        map_neon_data_to_ecocomDP(
          neon.data.product.id = x,
          site = id[[x]]$site,
          startdate = id[[x]]$startdate,
          enddate = id[[x]]$enddate,
          check.size = check.size,
          nCores = nCores,
          token = token,
          forceParallel = forceParallel)
      }
    })
  
  names(d) <- names(id)
  
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
                if (any(detected %in% c("POSIXct", "POSIXt", "Date", "IDate"))) {
                  detected <- "Date"
                  d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                }
                if (detected != expected) {
                  if (expected == 'character'){
                    d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                  } else if (expected == 'numeric'){
                    d[[x]]$tables[[y]][[z]] <<- as.numeric(d[[x]]$tables[[y]][[z]])
                  } else if (expected == 'Date'){
                    d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
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

  
  
  # Validate ------------------------------------------------------------------
  

  for (i in 1:length(d)) {
    d[[i]]$validation_issues <- validate_ecocomDP(data.list = d[[i]]$tables)
  }
  
  
  
  # Return --------------------------------------------------------------------
  
  if (!is.null(path)) {
    save_data(d, path, file.type)
  }
  
  d
  
} #END read_data()






# Helper functions ------------------------------------------------------------

#' Read an ecocomDP dataset from EDI
#'
#' @param id
#'     (character) Data package identifier with revision number
#'
#' @return
#'     (list) Named list of data tables
#'     
read_data_edi <- function(id) {
  
  message("Reading ", id)
  
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
    name = './/dataset/dataTable/physical/objectName',
    url = './/dataset/dataTable/physical/distribution/online/url',
    delimiter = './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter',
    nrecord = './/dataset/dataTable/numberOfRecords')
  
  eml <- suppressMessages(EDIutils::api_read_metadata(id))
  invisible(
    lapply(
      names(tbl_attrs),
      function(x) {
        use_i <- stringr::str_detect(
          xml2::xml_text(xml2::xml_find_all(eml, xpath[['name']])), 
          paste0(x, '\\.[:alnum:]*$'))
        if (any(use_i)) {
          lapply(
            names(xpath),
            function(k){
              tbl_attrs[[x]][[k]] <<- xml2::xml_text(
                xml2::xml_find_all(eml, xpath[[k]]))[use_i]
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
        tbl_attrs[[x]]$url, 
        sep = tbl_attrs[[x]]$delimiter)
    })
  names(output) <- names(tbl_attrs)
  list(
    metadata = NULL,
    tables = output)
  
}








#' Read ecocomDP from files
#'
#' @param data.path 
#'     (character) The path to the directory containing ecocomDP tables. 
#'     Duplicate file names are not allowed.
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
#' @export
#' 
read_from_files <- function(data.path) {
  
  validate_arguments("read_from_files", as.list(environment()))
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  
  d <- lapply(
    unique(attr_tbl$table),
    function(x) {
      ecocomDP_table <- stringr::str_detect(
        list.files(data.path), 
        paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
      if (any(ecocomDP_table)) {
        data.table::fread(
          paste0(data.path, "/", list.files(data.path)[ecocomDP_table]))
      }
    })
  names(d) <- unique(attr_tbl$table)
  d[sapply(d, is.null)] <- NULL
  
  # TODO: Read metadata from EML if exists in data.path
  d <- list(
    list(metadata = NULL, tables = d))
  names(d) <- d[[1]]$tables$dataset_summary$package_id
  
  # Coerce column classes to ecocomDP specifications. NOTE: This same 
  # process is applied to read_data(). Any update here should be
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
                if (any(detected %in% c("POSIXct", "POSIXt", "Date", "IDate"))) {
                  detected <- "Date"
                  d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                }
                if (detected != expected) {
                  if (expected == 'character'){
                    d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                  } else if (expected == 'numeric'){
                    d[[x]]$tables[[y]][[z]] <<- as.numeric(d[[x]]$tables[[y]][[z]])
                  } else if (expected == 'Date'){
                    d[[x]]$tables[[y]][[z]] <<- as.character(d[[x]]$tables[[y]][[z]])
                  }
                }
              })
          })
      }))
  
  # Return
  
  d
  
}







#' Save ecocomDP data
#'
#' @param data 
#'     (list) Data as a list object created by \code{read_data()}.
#' @param path
#'     (character) Path to the directory in which the data will be written.
#' @param file.type
#'     (character) Type of file to save the data to. Options are: ".rda", 
#'     ".csv"
#'
#' @return
#'     \item{.rda}{If \code{file.type} = ".rda", then an .rda representation 
#'     of \code{data} is returned.}
#'     \item{.csv}{If \code{file.type} = ".csv", then an set of .csv files are
#'     written to a sub-directory of \code{path} named after the data 
#'     package/product ID.}
#'     
#' @export
#'
#' @examples
#' d <- read_data("edi.193.3")
#' #' save_data(d, tempdir(), ".rda")
#' save_data(d, tempdir(), ".csv")
#' 
save_data <- function(data, path, file.type) {
  message("Saving data")
  tstamp <- Sys.time()
  tstamp <- stringr::str_remove_all(tstamp, "-")
  tstamp <- stringr::str_replace_all(tstamp, " ", "_")
  tstamp <- stringr::str_remove_all(tstamp, ":")
  if (file.type == ".rda") {
    fname <- paste0("ecocomDP_data_", tstamp, ".rda")
    message("Writing ", fname)
    saveRDS(data, file = paste0(path, "/", fname))
  } else if (file.type == ".csv") {
    for (i in 1:length(data)) {
      message("Writing ", names(data)[[i]])
      dirname <- paste0(path, "/", names(data)[[i]])
      dir.create(dirname)
      for (j in 1:length(data[[i]]$tables)) {
        fname <- paste0(names(data[[i]]$tables)[j], ".csv")
        message("  ", fname)
        data.table::fwrite(
          data[[i]]$tables[[j]], file = paste0(dirname, "/", fname))
      }
    }
  }
  
}