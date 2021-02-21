# general wrapper function for mapping NEON data products to ecocomDP
##############################################################################################
#' @title Map NEON data products to ecocomDP

#' @author Eric R Sokol \email{esokol@battelleecology.org}
#' @author Ruvi Jaimes \email{jaimesr@battelleecology.org}
#' @author Kari Norman \email{kari.norman@berkeley.edu}
#' @author Natalie Robinson \email{nrobinson@battelleecology.org}
#' 
#' @description
#' Pull files from the NEON API by data product, merge data for each table, and convert to ecocomDP format. Please see neonUtilities::loadByProduct for more information. 
#'
#' @param data.product.id The identifier of the NEON data product as it is listed in the "id" field in the table returned by the ecocomDP::search_data() function, e.g., neon.ecocomdp.20166.001.001 is the data product id for "ALGAE Periphyton, seston, and phytoplankton collection" converted to the ecocomDP format from the NEON source data product DP1.20166.001. 
#' @param ... Additional arguments passed to neonUtilities::loadByProduct(). 
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param nCores The number of cores to parallelize the stacking procedure. By default it is set to a single core.
#' @param forceParallel If the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Set to FALSE as default.
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.

#' @return Returns a named list, including: 
#' \item{metadata}{A list of information about the NEON data product returned by neonUtilities::getProductInfo.}
#' \item{tables}{A list of data.frames following the ecocomDP format.}
#'     \item{location}{A table, which has the lat long for each NEON site included in the data set}
#'     \item{taxon}{A table, which describes each taxonID that appears in the data set}
#'     \item{observation}{A table, which describes each taxonID that appears in the data set}


#' @examples
#' \dontrun{
#' # Retrieve and map NEON ALGAE data to ecocomDP format
#' my_result <- ecocomDP::map_neon_data_to_ecocomDP(
#'   neon.data.product.id = "DP1.20166.001",
#'   site = c("MAYF", "PRIN"),
#'   startdate = "2016-1",
#'   enddate = "2018-11",
#'   check.size = FALSE)
#' }
#' 
#' \dontrun{
#' my_result <- map_neon_data_to_ecocomDP( neon.data.product.id = "DP1.20120.001", 
#'                                         site= c('COMO','LECO'),
#'                                         startdate = "2019-06", 
#'                                         enddate = "2019-09")

#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Eric R Sokol (2020-04-17)
#     original creation
##############################################################################################
map_neon_data_to_ecocomDP <- function(
  data.product.id,
  ... #arguments set to neonUtilities::loadByProduct
){
  
  # get metadata
  metadata_all <- neonUtilities::getProductInfo(data.product.id)
  
  # call custom mapping function if available for given NEON data.product.id
  if(data.product.id == "neon.ecocomdp.20166.001.001"){
    #ALGAE method v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.20166.001.001(...)
    
  }else if(data.product.id == "neon.ecocomdp.20120.001.001"){
    #MACROINVERTEBRATE v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.20120.001.001(...)
    
  }else if(data.product.id == "neon.ecocomdp.10043.001.001"){
    #MOSQUITO v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.10043.001.001(...)
  
  }else if(data.product.id == "neon.ecocomdp.10022.001.001"){
    #BEETLE v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.10022.001.001(...)
    
  }else if(data.product.id == "neon.ecocomdp.10022.001.002"){
    #HERPETOLOGY v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.10022.001.002(...)
    
  }else if(data.product.id == "neon.ecocomdp.10003.001.001"){
    #BIRD v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.10003.001.001(...)
    
  }else if(data.product.id == "neon.ecocomdp.20107.001.001"){
    #FISH v01
    ecocomDP_tables <- ecocomDP::map_neon.ecocomdp.20107.001.001(...)
    
  }else{
    message(paste0("WARNING: ecocomDP mapping not currently available for ",data.product.id))
    ecocomDP_tables <- list(
      location = data.frame(),
      taxon = data.frame(),
      observation = data.frame())
  }
  
  # combine neon metadata and ecocomDP tables
  out_list <- list(
    metadata = metadata_all,
    tables = ecocomDP_tables)
  
  # browser()
  
  # return
  return(out_list)
}
