#' get_neon_metadata
#'
#' @description  
#'     Get metadata from the NEON data portal for input sites and dates.
#'
#' @usage get_neon_metadata(tables, data.path, criteria)
#' 
#' @param tables 
#'     (character) Names of valid L1 tables in your dataset working directory.
#'     Get valid table names with `validate_table_names`.
#' @param data.path
#'     (character) Path to the directory containing the L1 tables.
#' @param criteria
#'     (data frame) Validation criteria located at 
#'     /inst/validation_criteria.txt.
#'
#' @return 
#'     If foreign keys do not match primary keys, then an error is returned.
#'         
#' @export
#'

get_neon_metadata <- function(
  dpid = "DP1.20120.001", #data product ID (REQUIRED)
  data_table_name = 'sls_soilCoreCollection', #data table name (with or without the _pub suffix)
  sample_location_list = NA, # list of sites, domains, etc.
  sample_location_type = 'siteID', #location type
  sample_date_min = '2012-01-01', #start date -- default is 1-Jan-2012
  sample_date_max = Sys.Date(), #end date -- default is current date
  sample_date_format = '%Y-%m-%d', #date format
  ### more defaults
  data_package_type = 'basic',
  url_prefix_data = 'http://data.neonscience.org:80/api/v0/data/',
  url_prefix_products = 'http://data.neonscience.org:80/api/v0/products', # (REQUIRED)
  ...){
  
  # Check arguments -----------------------------------------------------------
  
  my_site_list <- c('COMO', 'ARIK')
  
  # Request data --------------------------------------------------------------
  
  req <- httr::GET(
    paste0(url_prefix_products, '/', dpid)
      )
  
  # View requested data
  req_content <- httr::content(req, as = 'parsed')
  
  req_text <- httr::content(req, as = 'text')
  avail <- jsonlite::fromJSON(req_text, simplifyDataFrame = T, flatten = T)
  
  # get data availability list of the product
  data_list <- unlist(avail$data$siteCodes$availableDataUrls)
  
  # get data availablity
  data <- GET(data_list[grep('ARIK/2015-03', data_list)])
  data_files <- jsonlite::fromJSON(httr::content(data, as = 'text'))
  
  # Get EML (.xml is listed twice, which to use?)
  data_eml <- EML::read_eml(
    data_files$data$files$url[grep('.xml', data_files$data$files$name)[1]]
    )
  
}