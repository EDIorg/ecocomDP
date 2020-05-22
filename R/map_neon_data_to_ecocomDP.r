# general wrapper function for mapping NEON data products to ecocomDP
##############################################################################################
#' @title Map NEON data products to ecocomDP

#' @author
#' Eric R Sokol \email{esokol@battelleecology.org}

#' @description
#' Pull files from the NEON API by data product, merge data for each table, and convert to ecocomDP format. Please see neonUtilities::loadByProduct for more information. 
#'
#' @param neon.data.product.id The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.20166.001 (neon.data.product.id for Periphyton, seston, and phytoplankton collection)
#' @param ... Additional arguments passed to neonUtilities::loadByProduct(). 
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param nCores The number of cores to parallelize the stacking procedure. By default it is set to a single core.
#' @param forceParallel If the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Set to FALSE as default.

#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.

#' @return Returns a named list, including: 
#' \item{neon_metadata}{A list of information about the NEON data product returned by neonUtilities::getProductInfo.}
#' \item{ecocomDP_tables}{A list of data.frames following the ecocomDP format.}


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

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Eric R Sokol (2020-04-17)
#     original creation
##############################################################################################
map_neon_data_to_ecocomDP <- function(
  neon.data.product.id,
  ... #arguments set to neonUtilities::loadByProduct
){
  
  # get metadata
  metadata_all <- neonUtilities::getProductInfo(neon.data.product.id)
  
  # call custom mapping function if available for given NEON neon.data.product.id
  if(neon.data.product.id == "DP1.20166.001"){
    ecocomDP_tables <- map_neon_data_to_ecocomDP.ALGAE(...)
  }else{
    message(paste0("WARNING: ecocomDP mapping not currently available for ",neon.data.product.id))
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