#' Aggregate ecocomDP data packages
#'
#' @description  
#'     Aggregate ecocomDP data packages.
#'
#' @usage 
#'     aggregate_ecocomDP(
#'       package.id, 
#'       path = NULL
#'     )
#'     
#' @param package.id
#'     (character) IDs of data packages to aggregate. Use 
#'     \code{view_all_ecocomDP} to get package IDs of available ecocomDP 
#'     datasets. Previous data package revisions are valid. It's OK to if only
#'     one data package is entered.
#' @param path
#'     (character; optional) Path to the directory in which the aggregated 
#'     ecocomDP will be written.
#' @param neon.sites
#'     (character) NEON sites to use the data of. THIS ARGUMENT IS A WIP.
#'     
#' @return 
#'     \itemize{
#'         \item{(list) A named list of tibbles (one for each ecocomDP table)
#'         containing data from aggregated \code{package.id}s}
#'         \item{(.csv files) .csv files (one for each ecocomDP table) 
#'         containing data from aggregated \code{package.id}s}
#'     }
#'         
#' @export
#'

aggregate_ecocomDP <- function(package.id, path = NULL, neon.sites = NULL){

  message('Aggregating ecocomDP data packages: ')
  message(paste0('  ', package.id, collapse = '\n'))

  
  # Get table attributes ------------------------------------------------------
  # Get the URL, file name, number of records, and field delimiter of each 
  # package.id for initializing the outgoing data list.
    
  # Get ecocomDP attributes
  
  attrs_ecocomDP <- suppressMessages(
    readr::read_tsv(
      system.file('validation_criteria.txt', package = 'ecocomDP')
    )
  )
  
  tbls <- unique(attrs_ecocomDP$table)
  
  # Initialize attributes list
  
  tbl_attrs <- lapply(
    seq_along(package.id),
    function(x){
      middle <- vector('list', length(tbls))
      inner <- lapply(
        seq_along(middle), 
        function(x){
          list(
            name = NULL,
            url = NULL,
            delimiter = NULL,
            nrecord = NULL
          )
        }
      )
      names(inner) <- tbls
      inner
    }
  )
  
  names(tbl_attrs) <- package.id
  
  # Fill attributes list
  
  xpath <- c(
    name = './/dataset/dataTable/physical/objectName',
    url = './/dataset/dataTable/physical/distribution/online/url',
    delimiter = './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter',
    nrecord = './/dataset/dataTable/numberOfRecords'
  )

  invisible(
    lapply(
      seq_along(tbl_attrs),
      function(x){
        eml <- suppressMessages(EDIutils::api_read_metadata(names(tbl_attrs)[x]))
        tblnames <- xml2::xml_text(
          xml2::xml_find_all(
            eml,
            xpath[['name']]
          )
        )
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
                    xml2::xml_find_all(
                      eml,
                      xpath[x]
                    )
                  )[i]
                }
              )
            }
          }
        )
      }
    )
  )

  # Initialize outgoing data --------------------------------------------------
  # Each table is an ecocomDP table filled with NA_character_ of nrows equal 
  # to the number of records in the ecocomDP tables to be aggregated.

  out <- lapply(
    seq_along(tbls),
    function(x){
      nrecord <- sum(as.numeric(unlist(purrr::map(.x = tbl_attrs, list(tbls[x], 'nrecord')))))
      cnames <- na.omit(attrs_ecocomDP$column[
        attrs_ecocomDP$table == tbls[x]
        ])
      out <- tibble::as.tibble(data.frame(
        matrix(NA_character_, nrow = nrecord, ncol = length(cnames)), stringsAsFactors = FALSE))
      colnames(out) <- cnames
      out
    }
  )
  names(out) <- tbls

  # Fill aggregation tables ---------------------------------------------------
  
  # Read file
  # Align columns
  # Assign globally unique IDs
  
  message('Creating tables:')
  
  for (i in seq_along(tbls)){
    message(paste0('  ', tbls[i]))
    counter <- 1
    url <- unlist(purrr::map(.x = tbl_attrs, list(tbls[i], 'url')))
    delim <- unlist(purrr::map(.x = tbl_attrs, list(tbls[i], 'delimiter')))
    nrecord <- cumsum(unlist(purrr::map(.x = tbl_attrs, list(tbls[i], 'nrecord'))))
    for (j in seq_along(url)){
      # Read file
      if (delim[j] == ','){
        d <- suppressWarnings(
          suppressMessages(
            readr::read_csv(
              url[j], 
              col_types = paste0(rep('c', length(na.omit(attrs_ecocomDP$class[(attrs_ecocomDP$table == tbls[i])]))), collapse = '')
            )
          )
        )
      } else if (delim[j] == '\\t'){
        d <- suppressWarnings(
          suppressMessages(
            readr::read_tsv(
              url[j], 
              col_types = paste0(rep('c', length(na.omit(attrs_ecocomDP$class[(attrs_ecocomDP$table == tbls[i])]))), collapse = '')
            )
          )
        )
      }
      # Add uniqe identifier
      d <- dplyr::mutate_at(
        d, 
        .vars = na.omit(attrs_ecocomDP$column[(attrs_ecocomDP$table == tbls[i]) & (attrs_ecocomDP$globally_unique == 'yes')]),
        .funs = function(x){
          paste0(x, '_', names(url[j]))
        }
      )
      # Clean up
      if (tbls[i] == 'location'){
        d$parent_location_id[stringr::str_detect(d$parent_location_id, 'NA')] <- NA_character_
      }
      # Fill
      suppressWarnings(
        out[[i]][counter:nrecord[j], 
                 match(colnames(d), colnames(out[[i]]))] <- d
      )
      counter <- sum(counter, nrecord[j])
    }
  }
  
  # Coerce column classes -----------------------------------------------------

  for (i in 1:length(out)){
    use_tbl <- names(out[i])
    if (!is.null(out[[use_tbl]])){
      for (j in 1:length(colnames(out[[use_tbl]]))){
        use_col <- colnames(out[[use_tbl]])[[j]]
        use_class <- attrs_ecocomDP$class[
          ((attrs_ecocomDP$table == use_tbl) & (!is.na(attrs_ecocomDP$column)) & (attrs_ecocomDP$column == use_col))
          ]
        out[[use_tbl]][[use_col]] <- col2class(
          column = out[[use_tbl]][[use_col]],
          class = use_class
        )
      }
    }
  }

  # Write tables to file ------------------------------------------------------
  
  if (!missing(path)){
    
    message('Writing tables to file')
    EDIutils::validate_path(path)
    
    # observation
    if (!is.null(out$observation)){
      write.table(out$observation,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_observation.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # location
    if (!is.null(out$location)){
      write.table(out$location,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_location.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # taxon
    if (!is.null(out$taxon)){
      write.table(out$taxon,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_taxon.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # dataset_summary
    if (!is.null(out$dataset_summary)){
      write.table(out$dataset_summary,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_dataset_summary.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # observation_ancillary
    if (!is.null(out$observation_ancillary)){
      write.table(out$observation_ancillary,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_observation_ancillary.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # location_ancillary
    if (!is.null(out$location_ancillary)){
      write.table(out$location_ancillary,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_location_ancillary.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # taxon_ancillary
    if (!is.null(out$taxon_ancillary)){
      write.table(out$taxon_ancillary,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_taxon_ancillary.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    # variable_mapping
    if (!is.null(out$variable_mapping)){
      write.table(out$variable_mapping,
                  file = paste0(path,
                                '/',
                                "ecocomDP_export_variable_mapping.csv"),
                  col.names = T,
                  row.names = F,
                  sep = ",",
                  eol = "\r\n",
                  quote = F)
    }
    
  }
  
  message('Done')
  
  # Return
  
  out
  
}




# Get ecocomDP --------------------------------------------------------------

get_ecocomDP <- function(package.id, neon.sites = NULL){
  
  # Get data from EDI -------------------------------------------------------
  
  if ((str_detect(package.id, 'edi.')) | (str_detect(package.id, 'knb-lter'))){
    
    # Validate
    
    response <- httr::GET('https://pasta.lternet.edu/package/search/eml?q=keyword:ecocomDP&fl=packageid&rows=1000')
    
    if (!(status_code(response) == 200)){
      stop('The data repository is inaccessible. Please try again later.')
    }
    
    xml_in <- read_xml(response)
    pkg_ids <- xml2::xml_text(
      xml2::xml_find_all(
        xml_in,
        './/document/packageid'
      )
    )
    pkg_ids <- stringr::str_replace(pkg_ids, '\\.[:digit:]$', '')
    
    pkg_prts <- unlist(strsplit(package.id, split = ".", fixed = T))
    scope <- pkg_prts[1]
    identifier <- pkg_prts[2]
    revision <- pkg_prts[3]
    
    if (length(pkg_prts) != 3){
      stop('The data package ID does not contain all the necessary parts. The package ID must contain the scope, identifier, and revision number (e.g. "edi.124.3")')
    }
    
    response <- httr::GET(paste0('https://pasta.lternet.edu/package/eml/', scope, '/', identifier))
    revs <- unlist(
      stringr::str_split(
        suppressMessages(
          httr::content(
            response
          )
        ),
        pattern = '\\n'
      )
    )
    
    pkg_revs <- paste0(
      pkg_prts[1],
      '.',
      pkg_prts[2],
      '.',
      revs)
    
    if (!(package.id %in% pkg_revs)){
      stop(
        paste0(
          'Sorry, the data package.id "', 
          package.id, 
          '" is not available in the ecocomDP format.'
        )
      )
    }
    
    # Get EML
    
    eml <- get_eml(package.id)
    
    # Get ecocomDP tables from EDI
    
    dat_out <- get_edi_table(package.id, eml)
    
  }
  
  # Get data from NEON -------------------------------------------------------
  
  if (str_detect(package.id, 'DP')){
    
    # Validate
    
    criteria <- suppressWarnings(read.table(
      system.file('neon_data_products_for_ecocomDP.txt', package = 'ecocomDP'),
      header = T,
      sep = "\t",
      as.is = T,
      na.strings = "NA"))
    
    if (!(package.id %in% unique(criteria$data_product_id))){
      stop(
        paste0(
          'Sorry, the NEON data product "', 
          package.id, 
          '" is not available in the ecocomDP format.'
        )
      )
    }
    
    # Get NEON data and format into ecocomDP
    
    if (!is.null(neon.sites)){
      dat_out <- reformat_neon(dp.id = package.id,
                               neon.sites = neon.sites)
    } else {
      dat_out <- reformat_neon(dp.id = package.id)
    }
    
  }
  
  # Return tables -----------------------------------------------------------
  
  dat_out
  
}




# Get tables from EDI ---------------------------------------------------------
get_edi_table <- function(package.id, eml){
  entity_names <- unlist(
    xmlApply(eml["//dataset/dataTable/entityName"], 
             xmlValue)
  )
  entity_delimiters <- unlist(
    xmlApply(eml["//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"], 
             xmlValue)
  )
  entity_urls <- unlist(
    xmlApply(eml["//dataset/dataTable/physical/distribution/online/url"], 
             xmlValue)
  )
  data_out <- list(
    observation = NULL,
    location = NULL,
    taxon = NULL,
    dataset_summary = NULL,
    observation_ancillary = NULL,
    location_ancillary = NULL,
    taxon_ancillary = NULL,
    variable_mapping = NULL
  )
  
  if (length(grep('observation\\b', entity_names)) != 0){
    file_name <- entity_names[grep('observation\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('observation\\b', entity_names)]
    file_url <- entity_urls[grep('observation\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$observation <-read.table(sub("^https", "http", file_url),
                                        header = T, 
                                        sep=",",
                                        as.is = T,
                                        fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$observation <-read.table(sub("^https", "http", file_url),
                                        header = T, 
                                        sep="\t",
                                        as.is = T,
                                        fill = T)
    }
  }
  if (length(grep('location\\b', entity_names)) != 0){
    file_name <- entity_names[grep('location\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('location\\b', entity_names)]
    file_url <- entity_urls[grep('location\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$location <-read.table(sub("^https", "http", file_url),
                                     header = T, 
                                     sep=",",
                                     as.is = T,
                                     fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$location <-read.table(sub("^https", "http", file_url),
                                     header = T, 
                                     sep="\t",
                                     as.is = T,
                                     fill = T)
    }
  }
  if (length(grep('taxon\\b', entity_names)) != 0){
    file_name <- entity_names[grep('taxon\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('taxon\\b', entity_names)]
    file_url <- entity_urls[grep('taxon\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$taxon <-read.table(sub("^https", "http", file_url),
                                  header = T, 
                                  sep=",",
                                  as.is = T,
                                  fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$taxon <-read.table(sub("^https", "http", file_url),
                                  header = T, 
                                  sep="\t",
                                  as.is = T,
                                  fill = T)
    }
  }
  if (length(grep('dataset_summary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('dataset_summary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('dataset_summary\\b', entity_names)]
    file_url <- entity_urls[grep('dataset_summary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$dataset_summary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep=",",
                                            as.is = T,
                                            fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$dataset_summary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep="\t",
                                            as.is = T,
                                            fill = T)
    }
  }
  if (length(grep('observation_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('observation_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('observation_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('observation_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$observation_ancillary <-read.table(sub("^https", "http", file_url),
                                                  header = T, 
                                                  sep=",",
                                                  as.is = T,
                                                  fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$observation_ancillary <-read.table(sub("^https", "http", file_url),
                                                  header = T, 
                                                  sep="\t",
                                                  as.is = T,
                                                  fill = T)
    }
  }
  if (length(grep('location_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('location_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('location_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('location_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$location_ancillary <-read.table(sub("^https", "http", file_url),
                                               header = T, 
                                               sep=",",
                                               as.is = T,
                                               fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$location_ancillary <-read.table(sub("^https", "http", file_url),
                                               header = T, 
                                               sep="\t",
                                               as.is = T,
                                               fill = T)
    }
  }
  if (length(grep('taxon_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('taxon_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('taxon_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('taxon_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$taxon_ancillary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep=",",
                                            as.is = T,
                                            fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$taxon_ancillary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep="\t",
                                            as.is = T,
                                            fill = T)
    }
  }
  if (length(grep('variable_mapping\\b', entity_names)) != 0){
    file_name <- entity_names[grep('variable_mapping\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('variable_mapping\\b', entity_names)]
    file_url <- entity_urls[grep('variable_mapping\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$variable_mapping <-read.table(sub("^https", "http", file_url),
                                             header = T, 
                                             sep=",",
                                             as.is = T,
                                             fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$variable_mapping <-read.table(sub("^https", "http", file_url),
                                             header = T, 
                                             sep="\t",
                                             as.is = T,
                                             fill = T)
    }
  }
  data_out
}




# Get tables from NEON --------------------------------------------------------
get_neon_table <- function(package.id, eml){
  entity_names <- unlist(
    xmlApply(eml["//dataset/dataTable/entityName"], 
             xmlValue)
  )
  entity_delimiters <- unlist(
    xmlApply(eml["//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"], 
             xmlValue)
  )
  entity_urls <- unlist(
    xmlApply(eml["//dataset/dataTable/physical/distribution/online/url"], 
             xmlValue)
  )
  data_out <- list(
    observation = NULL,
    location = NULL,
    taxon = NULL,
    dataset_summary = NULL,
    observation_ancillary = NULL,
    location_ancillary = NULL,
    taxon_ancillary = NULL,
    variable_mapping = NULL
  )
  
  if (length(grep('observation\\b', entity_names)) != 0){
    file_name <- entity_names[grep('observation\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('observation\\b', entity_names)]
    file_url <- entity_urls[grep('observation\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$observation <-read.table(sub("^https", "http", file_url),
                                        header = T, 
                                        sep=",",
                                        as.is = T,
                                        fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$observation <-read.table(sub("^https", "http", file_url),
                                        header = T, 
                                        sep="\t",
                                        as.is = T,
                                        fill = T)
    }
  }
  if (length(grep('location\\b', entity_names)) != 0){
    file_name <- entity_names[grep('location\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('location\\b', entity_names)]
    file_url <- entity_urls[grep('location\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$location <-read.table(sub("^https", "http", file_url),
                                     header = T, 
                                     sep=",",
                                     as.is = T,
                                     fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$location <-read.table(sub("^https", "http", file_url),
                                     header = T, 
                                     sep="\t",
                                     as.is = T,
                                     fill = T)
    }
  }
  if (length(grep('taxon\\b', entity_names)) != 0){
    file_name <- entity_names[grep('taxon\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('taxon\\b', entity_names)]
    file_url <- entity_urls[grep('taxon\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$taxon <-read.table(sub("^https", "http", file_url),
                                  header = T, 
                                  sep=",",
                                  as.is = T,
                                  fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$taxon <-read.table(sub("^https", "http", file_url),
                                  header = T, 
                                  sep="\t",
                                  as.is = T,
                                  fill = T)
    }
  }
  if (length(grep('dataset_summary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('dataset_summary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('dataset_summary\\b', entity_names)]
    file_url <- entity_urls[grep('dataset_summary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$dataset_summary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep=",",
                                            as.is = T,
                                            fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$dataset_summary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep="\t",
                                            as.is = T,
                                            fill = T)
    }
  }
  if (length(grep('observation_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('observation_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('observation_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('observation_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$observation_ancillary <-read.table(sub("^https", "http", file_url),
                                                  header = T, 
                                                  sep=",",
                                                  as.is = T,
                                                  fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$observation_ancillary <-read.table(sub("^https", "http", file_url),
                                                  header = T, 
                                                  sep="\t",
                                                  as.is = T,
                                                  fill = T)
    }
  }
  if (length(grep('location_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('location_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('location_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('location_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$location_ancillary <-read.table(sub("^https", "http", file_url),
                                               header = T, 
                                               sep=",",
                                               as.is = T,
                                               fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$location_ancillary <-read.table(sub("^https", "http", file_url),
                                               header = T, 
                                               sep="\t",
                                               as.is = T,
                                               fill = T)
    }
  }
  if (length(grep('taxon_ancillary\\b', entity_names)) != 0){
    file_name <- entity_names[grep('taxon_ancillary\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('taxon_ancillary\\b', entity_names)]
    file_url <- entity_urls[grep('taxon_ancillary\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$taxon_ancillary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep=",",
                                            as.is = T,
                                            fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$taxon_ancillary <-read.table(sub("^https", "http", file_url),
                                            header = T, 
                                            sep="\t",
                                            as.is = T,
                                            fill = T)
    }
  }
  if (length(grep('variable_mapping\\b', entity_names)) != 0){
    file_name <- entity_names[grep('variable_mapping\\b', entity_names)]
    file_delimiter <- entity_delimiters[grep('variable_mapping\\b', entity_names)]
    file_url <- entity_urls[grep('variable_mapping\\b', entity_names)]
    if (file_delimiter == ","){
      data_out$variable_mapping <-read.table(sub("^https", "http", file_url),
                                             header = T, 
                                             sep=",",
                                             as.is = T,
                                             fill = T)
    } else if (file_delimiter == "\\t"){
      data_out$variable_mapping <-read.table(sub("^https", "http", file_url),
                                             header = T, 
                                             sep="\t",
                                             as.is = T,
                                             fill = T)
    }
  }
  data_out
}





# Assign field types ----------------------------------------------------------
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
