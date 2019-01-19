#' Read in NEON EML for a specified data product
#'
#' @description  
#'     Get metadata from the NEON data portal for a data product ID. NOTE: The
#'     retrieved metadata file is specific to a random site and date range. 
#'     This function is intended to be used with `get_neon_data` and 
#'     `make_neon_eml` to craft the EML for a NEON data product converted to 
#'     the ecocomDP.
#'
#' @usage get_neon_metadata(dpid)
#' 
#' @param dpid 
#'     (character) NEON data product ID (e.g. DP1.20120.001).
#'
#' @return 
#'     EML metadata for a NEON data product.
#'         
#' @export
#'

get_neon_metadata <- function(
  dpid,
  url_prefix_data = 'http://data.neonscience.org:80/api/v0/data/',
  url_prefix_products = 'http://data.neonscience.org:80/api/v0/products'){
  
  # Request data
  
  req <- httr::GET(
    paste0(
      url_prefix_products,
      '/',
      dpid
    )
  )
  
  # View requested data
  
  req_content <- httr::content(req, as = 'parsed')
  req_text <- httr::content(req, as = 'text')
  avail <- jsonlite::fromJSON(
    req_text, 
    simplifyDataFrame = T, 
    flatten = T
  )
  
  # List data availability
  
  data_list <- unlist(avail$data$siteCodes$availableDataUrls)
  data <- GET(data_list[1])
  data_files <- jsonlite::fromJSON(httr::content(data, as = 'text'))
  
  # Read EML (.xml is listed twice, which to use?)
  
  data_eml <- EML::read_eml(
    data_files$data$files$url[
      grep(
        '.xml', 
        data_files$data$files$name
      )[1]
    ]
  )
  
  # Return object
  
  data_eml
  
}
