#' aggregate_ecocomDP
#'
#' @description  
#'     Aggregate one or more ecocomDP.
#'
#' @usage aggregate_ecocomDP(package.ids, path = NULL)
#'     
#' @param package.ids
#'     (character) One or more data package IDs listed in the data frame created by 
#'     `list_all_ecocomDP`. EDI data package IDs are of the form 
#'     'edi.package_number.revision_number' (e.g. edi.100.2). NEON data package
#'     IDs are of the form 'data_product.identifier.revision_number' (e.g.
#'     'DP1.20120.001'). NOTE: Previous data package revisions of ecocomDP 
#'     packages are valid entries.
#' @param path
#'     (character) Path to which the aggregated ecocomDP will be written.
#'     No path results in no data written to file.
#'     
#' @return 
#'     A single ecocomDP containing all the content of the individual ecocomDP
#'     listed in the argument "package.ids".
#'         
#' @export
#'

aggregate_ecocomDP <- function(package.ids, path = NULL){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(package.ids)){
    stop('Input argument "package.ids" is missing!')
  }

  # Create list of tables -----------------------------------------------------
  
  message('Loading requested packages ...')
  tables <- lapply(package.ids, get_ecocomDP)
  names(tables) <- package.ids
  
  # Fill empty fields with NA -------------------------------------------------
  
  message('Filling empty fields ...')
  tables <- lapply(tables, fill_empty_fields)
  
  # Assign globally unique IDS ------------------------------------------------
  
  message('Assigning globally unique IDs ...')
  tables <- mapply(assign_ids, tables, names(tables))
  
  # Coerce to common field types ----------------------------------------------
  
  message('Assigning field types ...')
  tables <- lapply(edi_data, assign_field_types)
  
  # Concatenate ecocomDPs -----------------------------------------------------
  
  message('Binding tables ...')
  tables_aggregated <- cat_tables(table.list = tables)

  # Export the aggregated ecocomDP --------------------------------------------
  
  if (!missing(path)){
    
    message('Writing aggregated data to file ...')
    validate_path(path)
    
    # observation
    if (!is.null(tables_aggregated$observation)){
      write.table(tables_aggregated$observation,
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
    if (!is.null(tables_aggregated$location)){
      write.table(tables_aggregated$location,
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
    if (!is.null(tables_aggregated$taxon)){
      write.table(tables_aggregated$taxon,
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
    if (!is.null(tables_aggregated$dataset_summary)){
      write.table(tables_aggregated$dataset_summary,
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
    if (!is.null(tables_aggregated$observation_ancillary)){
      write.table(tables_aggregated$observation_ancillary,
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
    if (!is.null(tables_aggregated$location_ancillary)){
      write.table(tables_aggregated$location_ancillary,
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
    if (!is.null(tables_aggregated$taxon_ancillary)){
      write.table(tables_aggregated$taxon_ancillary,
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
    if (!is.null(tables_aggregated$variable_mapping)){
      write.table(tables_aggregated$variable_mapping,
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
  
}




# Get ecocomDP --------------------------------------------------------------
get_ecocomDP <- function(package.id){
  
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
    
    dat_out <- reformat_neon(dp.id = package.id)
    
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




# Fill empty fields with NA ---------------------------------------------------
fill_empty_fields <- function(table.list){
  
  criteria <- read.table(
    system.file('validation_criteria.txt', package = 'ecocomDP'),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA")

  # observation
  cols_expected <- criteria$column[(criteria$table == 'observation') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$observation))]
  if (length(cols_missing) > 0){
    table.list$observation[cols_missing] <- NA_character_
    table.list$observation <- select(
      table.list$observation,
      observation_id,
      event_id,
      package_id,
      location_id,
      observation_datetime,
      taxon_id,
      variable_name,
      value,
      unit
      )
  }
  
  # location
  cols_expected <- criteria$column[(criteria$table == 'location') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$location))]
  if (length(cols_missing) > 0){
    table.list$location[cols_missing] <- NA_character_
    table.list$location <- select(
      table.list$location,
      location_id,
      location_name,
      latitude,
      longitude,
      elevation,
      parent_location_id
      )
  }
  
  # dataset_summary
  cols_expected <- criteria$column[(criteria$table == 'dataset_summary') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$dataset_summary))]
  if (length(cols_missing) > 0){
    table.list$dataset_summary[cols_missing] <- NA_character_
    table.list$dataset_summary <- select(
      table.list$dataset_summary,
      package_id,
      original_package_id,
      length_of_survey_years,
      number_of_years_sampled,
      std_dev_interval_betw_years,
      max_num_taxa,
      geo_extent_bounding_box_m2
    )
  }
  
  # taxon
  cols_expected <- criteria$column[(criteria$table == 'taxon') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$taxon))]
  if (length(cols_missing) > 0){
    table.list$taxon[cols_missing] <- NA_character_
    table.list$taxon <- select(table.list$taxon, taxon_id, taxon_rank, taxon_name, authority_system, authority_taxon_id)
  }
  
  # observation_ancillary
  cols_expected <- criteria$column[(criteria$table == 'observation_ancillary') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$observation_ancillary))]
  if ((length(cols_missing) > 0) & (!is.null(table.list$observation_ancillary))){
    table.list$observation_ancillary[cols_missing] <- NA_character_
    table.list$observation_ancillary <- select(
      table.list$observation_ancillary,
      package_id,
      original_package_id,
      length_of_survey_years,
      number_of_years_sampled,
      std_dev_interval_betw_years,
      max_num_taxa,
      geo_extent_bounding_box_m2
    )
  }
  
  # location_ancillary
  cols_expected <- criteria$column[(criteria$table == 'location_ancillary') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$location_ancillary))]
  if ((length(cols_missing) > 0) & (!is.null(table.list$location_ancillary))){
    table.list$location_ancillary[cols_missing] <- NA_character_
    table.list$location_ancillary <- select(
      table.list$location_ancillary,
      location_ancillary_id,
      location_id,
      datetime,
      variable_name,
      value,
      unit
    )
  }
  
  # taxon_ancillary
  cols_expected <- criteria$column[(criteria$table == 'taxon_ancillary') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$taxon_ancillary))]
  if ((length(cols_missing) > 0) & (!is.null(table.list$taxon_ancillary))){
    table.list$taxon_ancillary[cols_missing] <- NA_character_
    table.list$taxon_ancillary <- select(
      table.list$taxon_ancillary,
      taxon_ancillary_id,
      taxon_id,
      datetime,
      variable_name,
      value,
      unit,
      author
    )
  }
  
  # variable_mapping
  cols_expected <- criteria$column[(criteria$table == 'variable_mapping') & (!is.na(criteria$column))]
  cols_missing <- cols_expected[!(cols_expected %in% colnames(table.list$variable_mapping))]
  if ((length(cols_missing) > 0) & (!is.null(table.list$variable_mapping))){
    table.list$variable_mapping[cols_missing] <- NA_character_
    table.list$variable_mapping <- select(
      table.list$variable_mapping,
      variable_mapping_id,
      table_name,
      variable_name,
      mapped_system,
      mapped_id,
      mapped_label
    )
  }

  # Return
  
  table.list
  
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

# Assign globally unique IDs --------------------------------------------------
assign_ids <- function(table.list, table.list.name){
  
  # observation
  
  table.list$observation$observation_id <- paste0(
    table.list$observation$observation_id,
    '.',
    table.list.name
    )
  
  # location
  
  table.list$location$location_id <- paste0(
    table.list$location$location_id,
    '.',
    table.list.name
  )
  
  table.list$location$parent_location_id <- paste0(
    table.list$location$parent_location_id,
    '.',
    table.list.name
  )
  
  table.list$observation$location_id <- paste0(
    table.list$observation$location_id,
    '.',
    table.list.name
  )

  # dataset_summary
  
  table.list$dataset_summary$package_id <- paste0(
    table.list$dataset_summary$package_id,
    '.',
    table.list.name
  )
  
  table.list$observation$package_id <- paste0(
    table.list$observation$package_id,
    '.',
    table.list.name
  )
  
  # taxon
  
  table.list$taxon$taxon_id <- paste0(
    table.list$taxon$taxon_id,
    '.',
    table.list.name
  )
  
  table.list$observation$taxon_id <- paste0(
    table.list$observation$taxon_id,
    '.',
    table.list.name
  )
  
  # observation_ancillary
  
  if (!is.null(table.list$observation_ancillary)){
    
    table.list$observation_ancillary$observation_ancillary_id <- paste0(
      table.list$observation_ancillary$observation_ancillary_id,
      '.',
      table.list.name
    )
    
    table.list$observation_ancillary$event_id <- paste0(
      table.list$observation_ancillary$event_id,
      '.',
      table.list.name
    )
    
    table.list$observation$event_id <- paste0(
      table.list$observation$event_id,
      '.',
      table.list.name
    )
    
  }
  
  # location_ancillary
  
  if (!is.null(table.list$location_ancillary)){
    
    table.list$location_ancillary$location_ancillary_id <- paste0(
      table.list$location_ancillary$location_ancillary_id,
      '.',
      table.list.name
    )

    table.list$location_ancillary$location_id <- paste0(
      table.list$location_ancillary$location_id,
      '.',
      table.list.name
    )
    
  }
  
  # taxon_ancillary
  
  if (!is.null(table.list$taxon_ancillary)){
    
    table.list$taxon_ancillary$taxon_ancillary_id <- paste0(
      table.list$taxon_ancillary$taxon_ancillary_id,
      '.',
      table.list.name
    )
    
    table.list$taxon_ancillary$taxon_id <- paste0(
      table.list$taxon_ancillary$taxon_id,
      '.',
      table.list.name
    )

  }
  
  # variable_mapping
  
  if (!is.null(table.list$variable_mapping)){
    
    table.list$variable_mapping$variable_mapping_id <- paste0(
      table.list$variable_mapping$variable_mapping_id,
      '.',
      table.list.name
    )

  }
  
  # Return
  
  list(table.list)
  
}



# Concatenate ecocomDPs -----------------------------------------------------
cat_tables <- function(table.list){
  
  # Initialize storage
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
  
  # observation
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$observation)){
      data_out$observation <- rbind(
        data_out$observation,
        table.list[[i]]$observation
      )
    }
  }
  # location
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$location)){
      data_out$location <- rbind(
        data_out$location,
        table.list[[i]]$location
      )
    }
  }
  # taxon
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$taxon)){
      data_out$taxon <- rbind(
        data_out$taxon,
        table.list[[i]]$taxon
      )
    }
  }
  # dataset_summary
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$dataset_summary)){
      data_out$dataset_summary <- rbind(
        data_out$dataset_summary,
        table.list[[i]]$dataset_summary
      )
    }
  }
  # observation_ancillary
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$observation_ancillary)){
      data_out$observation_ancillary <- rbind(
        data_out$observation_ancillary,
        table.list[[i]]$observation_ancillary
      )
    }
  }
  # location_ancillary
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$location_ancillary)){
      data_out$location_ancillary <- rbind(
        data_out$location_ancillary,
        table.list[[i]]$location_ancillary
      )
    }
  }
  # taxon_ancillary
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$taxon_ancillary)){
      data_out$taxon_ancillary <- rbind(
        data_out$taxon_ancillary,
        table.list[[i]]$taxon_ancillary
      )
    }
  }
  # variable_mapping
  for (i in 1:length(table.list)){
    if (!is.null(table.list[[i]]$variable_mapping)){
      data_out$variable_mapping <- rbind(
        data_out$variable_mapping,
        table.list[[i]]$variable_mapping
      )
    }
  }
  
  data_out
  
}



