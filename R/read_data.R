#' Read ecocomDP data
#'
#' @param id
#'     (character) Identifier of dataset to read. Identifiers are listed in the 
#'     "id" column of the \code{search_data()} output.  If reading multiple 
#'     datasets, then \code{id} should be a character vector of identifiers or 
#'     a named list containing additional arguments to filter on (for NEON data 
#'     only; see below for arguments and examples).
#' @param path
#'     (character) Path to the directory in which the data will be written as 
#'     an .rda object.
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
#'     
#' @return
#'     (list) A named list, including: 
#'     \item{metadata}{A list of information about the data.}
#'     \item{tables}{A list of data.frames following the ecocomDP format 
#'     (\url{https://github.com/EDIorg/ecocomDP/blob/master/documentation/model/table_visualization.md})}
#'     
#' @details 
#'     Validation checks are applied to each dataset ensuring they comply with 
#'     the ecocomDP. A warning is issued when validation fails (please report 
#'     these to \url{https://github.com/EDIorg/ecocomDP/issues}). Datasets that
#'     pass validation are returned.
#'     
#'     Column classes are coerced to those defined in the ecocomDP 
#'     specification.
#'     
#'     Reading multiple datasets raises the issue of referential integrity. 
#'     This is addressed by adding the \code{id} to each table's primary key
#'     (e.g. observation_id, event_id, etc.).
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
#'       enddate = "2018-11",
#'       check.size = FALSE)))
#' 
read_data <- function(
  id, path, site = "all", startdate = NA, enddate = NA, check.size = FALSE, 
  nCores = 1, forceParallel = FALSE) {
  
  # Parameterize --------------------------------------------------------------
  
  # id can be a list so, if not already, wrap it in list()

  if (!is.list(id)) {
    empty_list <- vector(mode = "list", length(id))
    names(empty_list) <- unlist(id)
    id <- empty_list
  }
  
  # Get ecocomDP attributes for validation and coercion
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  
  # Validate input arguments --------------------------------------------------
  
  # TODO: Validate input arguments

  # Read ----------------------------------------------------------------------
  
  d <- lapply(
    names(id),
    function(x) {
      if (stringr::str_detect(
        x, 
        "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
        read_data_edi(x)
      } else if (stringr::str_detect(x, "^DP1")) {
        if (is.null(id[[x]])) {
          map_neon_data_to_ecocomDP(
            neon.data.product.id = x,
            site = site,
            startdate = startdate,
            enddate = enddate,
            check.size = check.size,
            nCores = nCores,
            forceParallel = FALSE)
        } else {
          do.call(map_neon_data_to_ecocomDP, c(neon.data.product.id = x, id[[x]]))
        }
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
              d[[x]]$tables[[y]][[use_i]] <<- NA
              d[[x]]$tables[[y]] <<- d[[x]]$tables[[y]][, ..nms]
            }
          })
      }))
  
  # Coerce column classes to ecocomDP specifications.
  # FIXME Harmonize date formats of different temporal resolutions
  
  col2class <- function(column, class){
    if (class == 'character'){
      column <- as.character(column)
    } else if (class == 'numeric'){
      column <- as.numeric(column)
    } else if (class == 'Date'){
      column <- lubridate::ymd(column)
    }
    column
  }
  
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
                if (all(detected %in% c("POSIXct", "POSIXt"))) {
                  detected <- "Date"
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
  
  
  # Ensure referential integrity
  
  # if (length(id) > 1) {
  #   # Append package_id to primary keys to ensure referential integrity
  #   
  # }
  
  # Validate ------------------------------------------------------------------
  
  for (i in 1:length(d)) {
    message("Validating ", names(d)[i])
    validate_ecocomDP(data.list = d[[i]]$tables)
  }
  
  # Return --------------------------------------------------------------------
  
  if (!missing(path)){
    tstamp <- Sys.time()
    tstamp <- stringr::str_remove_all(tstamp, "-")
    tstamp <- stringr::str_replace_all(tstamp, " ", "_")
    tstamp <- stringr::str_remove_all(tstamp, ":")
    fname <- paste0("ecocomDP_data_", tstamp, ".rda")
    message("Writing ", fname)
    saveRDS(d, file = paste0(path, "/", fname))
  }
  
  d
  
}




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
  # Create globally uniqe identifiers by appending package ID to primary and 
  # foreign keys.
  
  output <- lapply(
    names(tbl_attrs),
    function(x) {
      d <- data.table::fread(
        tbl_attrs[[x]]$url, 
        sep = tbl_attrs[[x]]$delimiter)
      # TODO: Add missing columns
      
      # # TODO: Move to join_data()
      # d <- dplyr::mutate_at(
      #   d, 
      #   .vars = na.omit(attr_tbl$column[
      #     (attr_tbl$table == x) & (attr_tbl$globally_unique == 'yes')]),
      #   .funs = function(k){
      #     paste0(k, '_', id)
      #   })
      
      # Clean up byproducts created with globally unique identifiers
      if (x == 'location') {
        d$parent_location_id[
          stringr::str_detect(d$parent_location_id, 'NA')] <- NA_character_
      }
      d
    })
  names(output) <- names(tbl_attrs)
  list(
    metadata = NULL,
    tables = output)
  
}