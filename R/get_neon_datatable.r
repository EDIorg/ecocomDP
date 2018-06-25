#' Get NEON data table
#' 
#' 
#' @author  Eric R. Sokol \email{esokol@battelleecology.org}
#' 
#' 
#' @description This is a function to retrieve a data table
#' from the NEON data portal for sites and dates provided by the
#' enduser.  
#'
#'
#' @param dpid character sting for NEON data product ID
#' @param data_table_name character sting for name of the data table to download, e.g., 'sls_soilCoreCollection' (with or without the _pub suffix)
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
#' @return data frame with selected NEON data
#' 
#' 
#' @examples  
#' sls_soilCoreCollection <- get_neon_datatable(
#'   sample_location_list = c('CPER','TALL'),
#'   sample_date_min = '2014-01-01',
#'   sample_date_max = '2014-12-31',
#'   dpid = "DP1.10086.001",
#'   data_table_name = 'sls_soilCoreCollection')
#' 
#' 
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'  
#' @export

get_neon_datatable <- function(
  dpid = "DP1.10086.001", #data product ID
  data_table_name = 'sls_soilCoreCollection', #data table name (with or without the _pub suffix)
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
  
  data_table_name <- gsub('(?i)_pub', '', data_table_name)
  
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
  
  # -- function to get available data ----
  
  fn_get_available_data <- function(url_to_get, data_table_name, data_package_type, ...){
    options(stringsAsFactors = FALSE)
    
    require(httr)
    require(jsonlite)
    require(dplyr)
    
    url_to_get <- as.character(url_to_get)
    
    tmp.df <- data.frame()
    tmp <- NULL
    
    try({tmp <- httr::GET(url_to_get)}, silent = TRUE)
    
    if(!is.null(tmp)){
      tmp.files <- jsonlite::fromJSON(httr::content(tmp, as='text'))
      tmp.files$data$files$name
      
      # should only be 1 url that has the correct data table name and data package type
      which_url_to_get <- dplyr::intersect(
        grep(data_table_name, tmp.files$data$files$name),
        grep(data_package_type, tmp.files$data$files$name))
      
      # make sure only getting 1 URL
      if(length(which_url_to_get) == 1){
        tmp.df <- suppressWarnings(
          readr::read_delim(
            tmp.files$data$files$url[which_url_to_get],
            delim = ",", 
            na = c('', 'NA', NA), 
            trim_ws = TRUE,
            col_types = cols(.default = 'c') # read everything is as character initially
          ))
        if("remarks" %in% colnames(tmp.df) ) {tmp.df$remarks <- as.character(tmp.df$remarks)}
      }
    }
    return(tmp.df)
  }
  
  # get data from portal
  list_data_from_portal <- mapply(FUN = fn_get_available_data,
                                  data_table_name = data_table_name,
                                  data_package_type = data_package_type,
                                  url_to_get = df_avail_data_to_get$url, 
                                  SIMPLIFY = FALSE)
  
  # convert list to a tibble using bind_rows
  df_data_from_portal <- do.call(bind_rows, list_data_from_portal)
  
  # function to check if a vector should be turned numeric, and then convert to numeric
  fn_make_numeric <- function(x_in){
    require(tidyverse)
    suppressWarnings(x_num <- x_in %>% as.double())
    x_char2 <- as.character(x_num)
    diff_list <- setdiff(x_in, x_char2)
    
    if(  length(diff_list) == 0 | 
         suppressWarnings(!anyNA(as.double(diff_list)))){
      return(x_num)
    } else {
      return(x_in)
    }
  }
  
  # change char columns to numeric if it makes sense
  df_data_from_portal <- df_data_from_portal %>% mutate_all(fn_make_numeric)
  
  ###########
  return(df_data_from_portal)
}