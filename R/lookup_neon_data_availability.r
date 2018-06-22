#' Lookup NEON data product availability
#' 
#'
#' @author
#' Eric R. Sokol \email{esokol@battelleecology.org}
#' 
#'   
#' @description This is a function to look up the sites and dates 
#' for which data are available on the NEON data portal for a given 
#' data product identifier (dpid). 
#' 
#'
#' @param dpid character sting for NEON data product ID
#' @param sample_location_list list of sites, domains, etc. If NA, retrieve all data for the given data table / dpid combination.
#' @param sample_location_type character sting for location type, such as 'siteID'. Must be one of the NEON controlled terms. If you're unsure, use 'siteID'
#' @param sample_date_min start date for query. Default is 1-Jan-2012, and this should capture the earliest NEON data record.
#' @param sample_date_max end date for query. Default is current date.
#' @param sample_date_format date format. Default/expected format is yyyy-mm-dd
#' @param data_package_type package type, either 'basic' or 'expanded'. If unsure, use 'expanded'
#' @param url_prefix_data data endpoint for NEON API.
#' @param url_prefix_products products endpoint for NEON API.
#'
#'
#' @return data frame with REST calls for sites and dates that have available data
#'
#' @examples
#' 
#' # look up by dpid
#' lookup_neon_data_availability("DP1.10086.001")
#' 
#' # look up by dpid for a specific site (CPER)
#' lookup_neon_data_availability(dpid = "DP1.10086.001",
#'                                      sample_location_list = 'CPER')
#'                                      
#' # look up by dpid for multiple sites                                      
#' lookup_neon_data_availability(dpid = "DP1.10086.001",
#'                                      sample_location_list = c('CPER','BART'))
#'
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'  
#' @export

lookup_neon_data_availability <- function(
  dpid = "DP1.10086.001", #data product ID
  sample_location_list = NA, # list of sites, domains, etc.
  sample_location_type = 'siteID', #location type
  sample_date_min = '2012-01-01', #start date -- default is 1-Jan-2012
  sample_date_max = Sys.Date(), #end date -- default is current date
  sample_date_format = '%Y-%m-%d', #date format
  ### more defaults
  data_package_type = 'basic',
  url_prefix_data = 'http://data.neonscience.org:80/api/v0/data/',
  url_prefix_products = 'http://data.neonscience.org:80/api/v0/products/',
  ...){
  
  require(dplyr)
  require(lubridate)
  require(readr)
  
  floor_date_min <- as.Date(sample_date_min, format = sample_date_format) %>% 
    floor_date(., unit = 'months') %>% format(., '%Y-%m-%d')
  floor_date_max <- as.Date(sample_date_max, format = sample_date_format) %>% 
    floor_date(., unit = 'months') %>% format(., '%Y-%m-%d')
  
  number_months <- (floor_date_min%--%floor_date_max)%/%months(1)
  
  sample_date_list <- ymd(floor_date_min) + months(1)*c(0:number_months)
  
  sample_year_month <- as.Date(sample_date_list) %>% format('%Y-%m')
  
  
  
  # Get records by data product
  # Soils data "DP1.10086.001"
  
  # step 1 -- use get to get available products -- using products endpoint to get catalog
  avail_json <- httr::GET(paste0(url_prefix_products,dpid))
  
  # step 2 -- get content using the "content" call, pass to fromJSON -- list of sites and months
  avail_content <- jsonlite::fromJSON(httr::content(avail_json, as="text"), simplifyDataFrame=T, flatten=T)
  
  # step 3 -- pull out urls for 
  df_avail_data <- data.frame(url = unlist(avail_content$data$siteCodes$availableDataUrls)) %>%
    mutate(url_info = gsub(url_prefix_data,'',url)) %>%
    tidyr::separate(url_info, into = c('dpid',sample_location_type,'year_month'), sep = '/')
  
  if(anyNA(sample_location_list)){
    df_avail_data_to_get <- dplyr::filter(df_avail_data,
                                          df_avail_data$year_month %in% sample_year_month)
  }else{
    df_avail_data_to_get <- dplyr::filter(df_avail_data,
                                          df_avail_data[,sample_location_type]%in%sample_location_list & 
                                            df_avail_data$year_month %in% sample_year_month)
  }
  
  return(df_avail_data_to_get)
}


