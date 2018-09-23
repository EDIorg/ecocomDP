#' aggregate_ecocomDP
#'
#' @description  
#'     Aggregate one or more ecocomDP.
#'
#' @usage aggregate_ecocomDP(package.ids)
#'     
#' @param package.ids
#'     (character) One or more data package IDs listed in the data frame created by 
#'     `list_all_ecocomDP`. EDI data package IDs are of the form 
#'     'edi.package_number.revision_number' (e.g. edi.100.2). NEON data package
#'     IDs are of the form 'data_product.identifier.revision_number' (e.g.
#'     'DP1.20120.001'). NOTE: Previous data package revisions of ecocomDP 
#'     packages are valid entries.
#'     
#' @return 
#'     A single ecocomDP containing all the content of the individual ecocomDP
#'     listed in the argument "package.ids".
#'         
#' @export
#'

aggregate_ecocomDP <- function(){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(package.ids)){
    stop('Input argument "package.ids" is missing!')
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
      
      # Get ecocomDP tables
      
      get_ecocomDP_tables <- function(package.id, eml){
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
        
      edi_data <- mapply(get_dataset_observation, pkg_ids, eml)

      }
    }
  }
    
    
  

  
  # - Validate inputs
  # - Load data into lists
  # - Assign globally unique IDs
  # - Unlist and glue together
  
}