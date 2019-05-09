#' view_landing_page
#'
#' @description  
#'     View the human readable landing page of a data package EML record.
#'
#' @usage view_landing_page(package.id)
#' 
#' @param package.id
#'     (character) The data package ID listed in the data frame created by 
#'     `list_all_ecocomDP`. EDI data package IDs are of the form 
#'     'edi.package_number.revision_number' (e.g. edi.100.2). NEON data package
#'     IDs are of the form 'data_product.identifier.revision_number' (e.g.
#'     'DP1.20120.001').
#'     
#' @return 
#'     The data package landing page.
#'         
#' @export
#'

view_landing_page <- function(package.id){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(package.id)){
    stop('Input argument "package.id" is missing!')
  }
  if (length(package.id) > 1){
    stop('Input argument "package.id" contains more than one package ID. Only one is allowed at a time.')
  }

  # Open data package landing page --------------------------------------------
  # Two methods are used. One for EDI and another for NEON.
  
  # EDI -----------------------------------------------------------------------
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
    
    # Open landing page
    
    pkg_prts <- unlist(strsplit(package.id, split = ".", fixed = T))
    scope <- pkg_prts[1]
    identifier <- pkg_prts[2]
    revision <- pkg_prts[3]
    
    pkg_url <- paste0(
      'http://portal.edirepository.org/nis/mapbrowse?scope=',
      scope,
      '&identifier=',
      identifier,
      '&revision=',
      revision
      )
    
    if (EDIutils::detect_os() == 'mac'){
      system(
        paste0(
          'open ',
          '"',
          pkg_url,
          '"'
        )
      )
    } else if (EDIutils::detect_os() == 'win'){
      system(
        paste0(
          'open ',
          '"',
          pkg_url,
          '"'
        )
      )
    }

    
    # NEON --------------------------------------------------------------------
  } else if (str_detect(package.id, 'DP')){
    
    # Validate
    
    criteria <- read.table(
      system.file('neon_data_products_for_ecocomDP.txt', package = 'ecocomDP'),
      header = T,
      sep = "\t",
      as.is = T,
      na.strings = "NA")
    
    if (!(package.id %in% unique(criteria$data_product_id))){
      stop(
        paste0(
          'Sorry, the NEON data product "', 
          package.id, 
          '" is not available in the ecocomDP format.'
        )
      )
    }
    
    # Open landing page
    
    if (EDIutils::detect_os() == 'mac'){
      system(
        paste0(
          'open ',
          system.file(
            paste0(
              package.id,
              '_neon.webarchive'
            ),
            package = 'ecocomDP'
          )
        )
      )
    } else if (EDIutils::detect_os() == 'win'){
      system(
        paste0(
          system.file(
            paste0(
              package.id,
              '_neon.webarchive'
            ),
            package = 'ecocomDP'
          )
        )
      )
    }
    
  } else {
    stop('The data package ID does not contain all the necessary parts. The package ID must contain the scope, identifier, and revision number (e.g. "edi.124.3")')
  }
  
  
  
}