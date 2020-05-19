#' Read ecocomDP data
#'
#' @param id
#'     (character) Identifier (one or more) of ecocomDP datasets to read. IDs 
#'     are listed in the "id" column of search results output from 
#'     \code{search_data()}.
#' @param site 
#'     (character) For NEON data. Either the string "all", meaning 
#'     all available sites, or a character vector of codes listed in the 
#'     "sites" column of results output from \code{search_data()}. Defaults to 
#'     "all".
#' @param startdate
#'     (character) For NEON data. Either NA, meaning all available dates, or a 
#'     character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate 
#'     (character) For NEON data. Either NA, meaning all available dates, or a 
#'     character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param check.size 
#'     (logical) For NEON data. T or F, should the user approve the total file 
#'     size before downloading? Defaults to T. When working in batch mode, or 
#'     other non-interactive workflow, use check.size = F.
#' @param nCores
#'     (integer) For NEON data. The number of cores to parallelize the 
#'     stacking procedure. By default it is set to a single core.
#' @param forceParallel 
#'     (logical) For NEON data. If the data volume to be processed does not 
#'     meet minimum requirements to run in parallel, this overrides. Set to 
#'     FALSE as default.
#' @param path
#'     (character; optional) Path to the directory in which the aggregated 
#'     ecocomDP will be written.
#' @return
#'     (list) A named list, including: 
#' \item{metadata}{A list of information about the data.}
#' \item{tables}{A list of data.frames following the ecocomDP format.}
#' @details 
#'     Each data source has a corresponding reader. Determine the source from 
#'     the id and call the correct reader.
#' @export
#' @examples
#' d <- read_data("edi.193.3")
#' d <- read_data("DP1.20166.001")
read_data <- function(id, site = "all", startdate = NA, enddate = NA, 
                      check.size = TRUE, nCores = 1, forceParallel = FALSE, 
                      path) {
  # TODO: Validate inputs
  # TODO: Wrap map_neon_data_to_ecocomDP()
  # TODO: Refactor inputs for multiple NEON datasets (supply as list?)
  
  message("Reading ", id)
  
  # EDI -----------------------------------------------------------------------
  
  if (stringr::str_detect(
    id, 
    "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
    
    # Get attributes of input tables ------------------------------------------
    # Get the URL, file name, number of records, and field delimiter of each 
    # table to be aggregated so the output tables may be initialized for better
    # performance.
    
    # Get ecocomDP attributes
    
    attrs_ecocomDP <- suppressMessages(
      readr::read_tsv(
        system.file('validation_criteria.txt', package = 'ecocomDP')))
    
    tbls <- unique(attrs_ecocomDP$table)
    
    # Initialize attributes list
    
    tbl_attrs <- lapply(
      seq_along(id),
      function(x){
        out <- lapply(
          seq_along(vector('list', length(tbls))), 
          function(x){
            list(name = NULL, url = NULL, delimiter = NULL, nrecord = NULL)
          }
        )
        names(out) <- tbls
        out
      }
    )
    names(tbl_attrs) <- id
    
    # Fill attributes list
    
    xpath <- c(
      name = './/dataset/dataTable/physical/objectName',
      url = './/dataset/dataTable/physical/distribution/online/url',
      delimiter = './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter',
      nrecord = './/dataset/dataTable/numberOfRecords')
    
    invisible(
      lapply(
        seq_along(tbl_attrs),
        function(x){
          eml <- suppressMessages(
            EDIutils::api_read_metadata(names(tbl_attrs)[x]))
          tblnames <- xml2::xml_text(xml2::xml_find_all(eml, xpath[['name']]))
          pkg_id <- names(tbl_attrs)[x]
          lapply(
            seq_along(tbls),
            function(x){
              i <- stringr::str_detect(tblnames, paste0(tbls[x], '\\.[:alnum:]*$'))
              if (any(i)){
                tbl <- tbls[x]
                lapply(
                  seq_along(xpath),
                  function(x){
                    tbl_attrs[[pkg_id]][[tbl]][[x]] <<- xml2::xml_text(
                      xml2::xml_find_all(eml, xpath[x]))[i]
                  })
              }
            })
        }))
    
    # Initialize output tables ------------------------------------------------
    # Outputs are ecocomDP tables filled with NA_character_ of nrows equal to the 
    # number of records of the inputs.
    
    out <- lapply(
      seq_along(tbls),
      function(x){
        nrecord <- sum(as.numeric(unlist(purrr::map(
          .x = tbl_attrs, 
          list(tbls[x], 'nrecord')))))
        cnames <- na.omit(attrs_ecocomDP$column[attrs_ecocomDP$table == tbls[x]])
        out <- tibble::as_tibble(data.frame(matrix(NA_character_, 
                                                   nrow = nrecord, 
                                                   ncol = length(cnames)),
                                            stringsAsFactors = FALSE))
        colnames(out) <- cnames
        out
      }
    )
    names(out) <- tbls
    
  }
  
  # Fill output tables --------------------------------------------------------
  # Sequentially fill output tables
  
  message('Creating tables:')
  
  invisible(
    lapply(
      seq_along(tbls),
      function(i){
        message(paste0('  ', tbls[i]))
        counter <- 1
        url <- unlist(purrr::map(.x = tbl_attrs, 
                                 list(tbls[i], 'url')))
        delim <- unlist(purrr::map(.x = tbl_attrs, 
                                   list(tbls[i], 'delimiter')))
        nrecord <- cumsum(unlist(purrr::map(.x = tbl_attrs, 
                                            list(tbls[i], 'nrecord'))))
        lapply(
          seq_along(url),
          function(j){
            # Read file
            if (delim[j] == ','){
              d <- suppressWarnings(suppressMessages(readr::read_csv(
                url[j], 
                col_types = paste0(
                  rep('c', 
                      length(
                        na.omit(
                          attrs_ecocomDP$class[
                            attrs_ecocomDP$table == tbls[i]]))), 
                  collapse = ''))))
            } else if (delim[j] == '\\t'){
              d <- suppressWarnings(suppressMessages(readr::read_tsv(
                url[j], 
                col_types = paste0(
                  rep('c', 
                      length(
                        na.omit(
                          attrs_ecocomDP$class[
                            attrs_ecocomDP$table == tbls[i]]))), 
                  collapse = ''))))
            }
            # Create globally uniqe identifiers by appending package ID to 
            # primary and foreign keys.
            d <- dplyr::mutate_at(
              d, 
              .vars = na.omit(attrs_ecocomDP$column[
                (attrs_ecocomDP$table == tbls[i]) & 
                  (attrs_ecocomDP$globally_unique == 'yes')]),
              .funs = function(x){
                paste0(x, '_', names(url[j]))
              })
            # Clean up byproducts created with globally unique identifiers
            if (tbls[i] == 'location'){
              d$parent_location_id[
                stringr::str_detect(d$parent_location_id, 'NA')] <- NA_character_
            }
            # Fill output tables
            suppressWarnings(
              out[[i]][counter:nrecord[j], 
                       match(colnames(d), colnames(out[[i]]))] <<- d)
            counter <<- sum(counter, nrecord[j])
          }
        )
      }
    )
  )
  
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
              use_class <- attrs_ecocomDP$class[
                ((attrs_ecocomDP$table == use_tbl) & 
                   (!is.na(attrs_ecocomDP$column)) & 
                   (attrs_ecocomDP$column == use_col))]
              out[[use_tbl]][[use_col]] <<- col2class(
                column = out[[use_tbl]][[use_col]],
                class = use_class)
            }
          )
        }
      }
    )
  )
  
  # NEON ----------------------------------------------------------------------
  # Read from NEON
  
  # read_data <- function(id, site = "all", startdate = NA, enddate = NA, 
  #                       check.size = TRUE, nCores = 1, forceParallel = FALSE, 
  #                       path) {
  
  if (stringr::str_detect(id, "^DP1")) {
    out_neon <- ecocomDP::map_neon_data_to_ecocomDP(
      neon.data.product.id = id,
      site = site,
      startdate = startdate,
      enddate = enddate,
      check.size = check.size,
      nCores = nCores,
      forceParallel = FALSE)
  }
  
  # Combine sources -----------------------------------------------------------
  # Combine results from different sources
  
  
  
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
