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
  
  # Open data package landing page --------------------------------------------
  # Two methods are used. One for EDI and another for NEON.
  
  if ((str_detect(package.id, 'edi.')) | (str_detect(package.id, 'knb-lter'))){

    # Validate package.id
    
    # if (system.file(paste0(package.id, '_neon.webarchive')) == ''){
    #   stop('Invalid package.id. Try again :)')
    # }
    
    # Open landing page
    
    
  } else if (str_detect(package.id, 'DP')){
    
    # Validate package.id
    
    if (system.file(paste0(package.id, '_neon.webarchive')) == ''){
      stop('Invalid package.id. Try again :)')
    }
    
    # Open landing page
    
    if (detect_os() == 'mac'){
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
    } else if (detect_os() == 'win'){
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
    
  }
  
  
  
}