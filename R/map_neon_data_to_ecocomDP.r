# general wrapper function for mapping NEON data products to ecocomDP
##############################################################################################
#' @title Map NEON data products to ecocomDP

#' @author Eric R Sokol \email{esokol@battelleecology.org}
#' @author Ruvi Jaimes
#' 
#' @description
#' Pull files from the NEON API by data product, merge data for each table, and convert to ecocomDP format. Please see neonUtilities::loadByProduct for more information. 
#'
#' @param id (character) Identifier of dataset to read. Identifiers are listed in the "id" column of the \code{search_data()} output. e.g., \code{neon.ecocomdp.20166.001.001} is the \code{id} for "ALGAE Periphyton, seston, and phytoplankton collection" converted to the ecocomDP format from the NEON source data product DP1.20166.001. 
#' @param neon.data.save.dir (character) Directory to save NEON source data that has been downloaded via \code{neonUtilities::loadByProduct()}
#' @param neon.data.read.path (character) Path to read in an RDS file of 'stacked NEON data' from \code{neonUtilities::loadByProduct()}
#' @param neon.data.list (list) A list of stacked NEON data tables to be mapped to ecocomDP. Must match the chosen mapping method in 'id'
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
#'     \item{tables$location}{A table, which has the lat long for each NEON site (\code{location_id}) included in the data set}
#'     \item{tables$taxon}{A table, which describes each \code{taxon_id} that appears in the data set}
#'     \item{tables$observation}{A table, with a record for each \code{observation_id} that appears in the data set}


#' @examples
#' \dontrun{
#' # Retrieve and map NEON ALGAE data to ecocomDP format
#' my_result <- ecocomDP:::map_neon_data_to_ecocomDP(
#'   id = "neon.ecocomdp.20166.001.001",
#'   site = c("MAYF", "PRIN"),
#'   startdate = "2016-1",
#'   enddate = "2018-11",
#'   check.size = FALSE)
#' }
#' 
#' \dontrun{
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' 
#' @export


# changelog and author contributions / copyrights
#   Eric R Sokol (2020-04-17)
#     original creation
##############################################################################################
map_neon_data_to_ecocomDP <- function(
  id,
  neon.data.save.dir = NULL,
  neon.data.read.path = NULL,
  neon.data.list = NULL,
  ... #arguments set to neonUtilities::loadByProduct
){
  
  # get NEON DPID
  neon.data.product.id <- id %>% 
    gsub("neon\\.ecocomdp", "DP1",.) %>% 
    gsub("\\.[0-9]{3}$","",.)
  
  # check arguments passed via dots for neonUtilities
  dots_updated <- list(..., dpID = neon.data.product.id)
  
  #Error handling if user provides a value for "package" other than "expanded"
  if("package" %in% names(dots_updated) && dots_updated$package != "expanded") message(
    paste0("WARNING: expanded package for ", neon.data.product.id, 
           " is required to execute this request. Downloading the expanded package"))
  
  dots_updated[["package"]] <- "expanded"
  
  
  # Getting the raw NEON data ----
  # check if neon.data.list is provided
  if(is.null(neon.data.list)){
    
    #if not, check if a filepath to an RDS file is provided
    if(!is.null(neon.data.read.path)){
      neon.data.list <- readRDS(neon.data.read.path)
      
      data_access_method_described <- paste0(
        "original NEON data accessed from file: ",
        neon.data.read.path)
      
      # if not, read data from NEON API
    }else{
      neon.data.list <- rlang::exec(
        neonUtilities::loadByProduct,
        !!!dots_updated)
      
      data_access_method_described <- paste0(
        "original NEON data accessed using neonUtilities v", 
        packageVersion("neonUtilities"))
    }
  }else{
    data_access_method_described <- paste0(
      "original NEON data accessed from R object")
  }

  

  # save NEON download if given a filepath
  if(!is.null(neon.data.save.dir)){
    neon.data.save.dir <- gsub("/$","",neon.data.save.dir)
    neon_data_filename <- paste0(neon.data.save.dir,"/",
                                 neon.data.product.id,"_",
                                 format(Sys.time(), "%Y%m%d%H%M%S"),
                                 ".RDS")
    saveRDS(neon.data.list, file = neon_data_filename)
  }

  
  # call custom mapping function if available for given NEON id
  if(id == "neon.ecocomdp.20166.001.001"){
    #ALGAE method v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.20166.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.20120.001.001"){
    #MACROINVERTEBRATE v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.20120.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10043.001.001"){
    #MOSQUITO v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10043.001.001(neon.data.list, ...)
  
  }else if(id == "neon.ecocomdp.10022.001.001"){
    #BEETLE v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10022.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10022.001.002"){
    #HERPETOLOGY v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10022.001.002(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10003.001.001"){
    #BIRD v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10003.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.20107.001.001"){
    #FISH v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.20107.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10058.001.001"){
    #PLANT v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10058.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10072.001.001"){
    #SMALL_MAMMAL v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10072.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10093.001.001"){
    #TICK v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10093.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.20219.001.001"){
    # ZOOPLANKTON (uses MACROINVERTEBRATE NEON taxon table) v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.20219.001.001(neon.data.list, ...)
    
  }else if(id == "neon.ecocomdp.10092.001.001"){
    # TICK_PATHOGENS (no NEON taxon table) v01
    ecocomDP_tables <- ecocomDP:::map_neon.ecocomdp.10092.001.001(neon.data.list, ...)
    
  }else{
    message(paste0("WARNING: ecocomDP mapping not currently available for ",id))
    ecocomDP_tables <- list(
      location = data.frame(),
      taxon = data.frame(),
      observation = data.frame())
  }
  
 
  # get taxon group
  neon_dp_table <- ecocomDP::search_data("NEON")
  taxon_group <- neon_dp_table$title[neon_dp_table$id == id] %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    dplyr::first()
  
  # combine neon metadata and ecocomDP tables
  out_list <- list(
    metadata = list(
      data_package_info = list(
        data_package_id = ecocomDP_tables$dataset_summary$package_id,
        taxonomic_group = taxon_group,
        orig_NEON_data_product_id = neon.data.product.id,
        NEON_to_ecocomDP_mapping_method = id,
        data_access_method = data_access_method_described,
        data_access_date_time = Sys.time()),
      orig_NEON_data_product_info = neonUtilities::getProductInfo(
        dpID = neon.data.product.id,
        token = ifelse("token" %in% names(dots_updated), dots_updated$token, NA_character_))),
    tables = ecocomDP_tables)
  
  # return
  return(out_list)
}
