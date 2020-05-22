#' Read ecocomDP data
#'
#' @param id
#'     (character) Identifier of dataset to read. Identifiers are listed in the 
#'     "id" column of the \code{search_data()} output.  If reading multiple 
#'     datasets, then \code{id} should be a character vector of identifiers or 
#'     a named list containing additional arguments to filter on (for NEON data 
#'     only; see below for arguments and examples).
#' @param path
#'     (character) Path to the directory in which the data will be written file 
#'     (one file per ecocomDP table).
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
#'     before downloading? Defaults to TRUE.
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
#' # Read multiple datasets and concatenate together
#' d <- read_data(c("edi.193.3", "DP1.20166.001"))
#' 
#' # Read multiple datasets from EDI and NEON with NEON filters.
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
  id, path, site = "all", startdate = NA, enddate = NA, check.size = TRUE, 
  nCores = 1, forceParallel = FALSE) {
  
  message("Reading ", id)
  
  # Parameterize --------------------------------------------------------------
  
  # Get ecocomDP attributes for validation and coercion
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  
  # Identify data sources
  edi_i <- stringr::str_detect(
    id, 
    "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")
  neon_i <- stringr::str_detect(id, "^DP1")

  # Read from EDI -------------------------------------------------------------
  # Read from the Environmental Data Initiative Data Repository
  
  if (any(edi_i)) {
    edi_d <- lapply(id[edi_i], read_data_edi)
    names(edi_d) <- id[edi_i]
  }

  # Read from NEON ------------------------------------------------------------
  # Read from the National Ecological Observatory Network
  
  if (any(neon_i)) {
    neon_d <- ecocomDP::map_neon_data_to_ecocomDP(
      neon.data.product.id = id,
      site = site,
      startdate = startdate,
      enddate = enddate,
      check.size = check.size,
      nCores = nCores,
      forceParallel = forceParallel)
    browser()
  }
  
  # Apply validation checks ---------------------------------------------------
  
  # Coerce column classes -----------------------------------------------------
  # # Coerce column classes to ecocomDP specifications.
  # # FIXME Harmonize date formats of different temporal resolutions
  # 
  # invisible(
  #   lapply(
  #     seq_along(out),
  #     function(i){
  #       use_tbl <- names(out[i])
  #       if (!is.null(out[[use_tbl]])){
  #         lapply(
  #           seq_along(colnames(out[[use_tbl]])),
  #           function(j){
  #             use_col <- colnames(out[[use_tbl]])[[j]]
  #             use_class <- attr_tbl$class[
  #               ((attr_tbl$table == use_tbl) & 
  #                  (!is.na(attr_tbl$column)) & 
  #                  (attr_tbl$column == use_col))]
  #             out[[use_tbl]][[use_col]] <<- col2class(
  #               column = out[[use_tbl]][[use_col]],
  #               class = use_class)
  #           }
  #         )
  #       }
  #     }
  #   )
  # )
  
  # Combine sources -----------------------------------------------------------
  # Combine results from different sources
  
  if (length(id) > 1) {
    # Append package_id to primary keys to ensure referential integrity
    
  }
  
  # Coerce column classes -----------------------------------------------------
  # Coerce column classes to ecocomDP specifications.
  # FIXME Harmonize date formats of different temporal resolutions
  
  invisible(
    lapply(
      seq_along(out),
      function(i){
        use_tbl <- names(out[i])
        if (!is.null(out[[use_tbl]])){
          lapply(
            seq_along(colnames(out[[use_tbl]])),
            function(j){
              use_col <- colnames(out[[use_tbl]])[[j]]
              use_class <- attr_tbl$class[
                ((attr_tbl$table == use_tbl) & 
                   (!is.na(attr_tbl$column)) & 
                   (attr_tbl$column == use_col))]
              out[[use_tbl]][[use_col]] <<- col2class(
                column = out[[use_tbl]][[use_col]],
                class = use_class)
            }
          )
        }
      }
    )
  )
  
  # Return --------------------------------------------------------------------
  
  if (!missing(path)){
    message('Writing tables to file')
    EDIutils::validate_path(path)
    lapply(
      seq_along(out),
      function(x){
        readr::write_csv(
          out[[tbls[x]]], 
          paste0(path, '/ecocomDP_export_', tbls[x], '.csv'))
      }
    )
  }
  
  list(
    metadata = NULL,
    tables = out)
  
}




# Helper functions ------------------------------------------------------------

assign_field_types <- function(table.list){
  
  criteria <- read.table(
    system.file('validation_criteria.txt', package = 'ecocomDP'),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA")
  
  for (i in 1:length(table.list)){
    use_tbl <- names(table.list[i])
    if (!is.null(table.list[[use_tbl]])){
      for (j in 1:length(colnames(table.list[[use_tbl]]))){
        use_col <- colnames(table.list[[use_tbl]])[[j]]
        use_class <- criteria$class[
          ((criteria$table == use_tbl) & (!is.na(criteria$column)) & (criteria$column == use_col))
          ]
        table.list[[use_tbl]][[use_col]] <- col2class(
          column = table.list[[use_tbl]][[use_col]],
          class = use_class
        )
      }
    }
  }
  
  # Return
  table.list
}




col2class <- function(column, class){
  if (class == 'character'){
    
    column <- as.character(column)
  } else if (class == 'numeric'){
    column <- as.numeric(column)
  } else if (class == 'Date'){
    column <- as.character(column)
  }
  column
}





#' Read an ecocomDP dataset from EDI
#'
#' @param id
#'     (character) Data package identifier with revision number
#'
#' @return
#'     (list) Named list of data tables
#'     
read_data_edi <- function(id) {
 
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
      d <- suppressMessages(
        data.table::fread(
          tbl_attrs[[x]]$url, 
          sep = tbl_attrs[[x]]$delimiter))
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