#' view_landing_page
#'
#' @description  
#'     View human readable version of a data package EML record.
#'
#' @usage view_landing_page(id)
#' 
#' @param id
#'     (integer) A single integer or vector of integers corresponding with 
#'     data package IDs listed in the data frame created by 
#'     `list_all_ecocomDP`.
#'     
#' @return 
#'     The data package landing page.
#'         
#' @export
#'

view_landing_page <- function(id){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(id)){
    stop('Input argument "id" is missing!')
  }
  
  # Look up ID in table exported to local workspace ---------------------------
  
  # Open data package landing page in EDI Data Repository ---------------------
  
  # Open apriori rendered data package landing page of NEON data product ------
  
  if (detect_os() == 'mac'){
    
    system(
      paste0(
        'open ',
        system.file(
          paste0(
            dp.id,
            '_neon.webarchive'
          ),
          package = 'ecocomDP'
        )
      )
    )
    
  } else if (detect_os() == 'win'){
   
    # system(
    #   paste0(
    #     'open ',
    #     system.file(
    #       paste0(
    #         dp.id,
    #         '_neon.webarchive'
    #       ),
    #       package = 'ecocomDP'
    #     )
    #   )
    # )
     
  }
  
}