##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon_data_to_ecocomDP.MACROINVERTEBRATE(site= c('COMO','LECO'),
#'                                                          startdate = "2019-06", 
#'                                                          enddate = "2019-09")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for MACROINVERTEBRATE from neon.data.product.id DP1.20120.001 from the NEON data portal and map to the ecocomDP 
#' @export

# changelog and author contributions / copyrights
#   Eric R Sokol & Ruvi Jaimes (2020-06-08)
#     original creation
##############################################################################################

##### my version ----
# updated by Eric on 6/9/2020 ~5:10pm
map_neon_data_to_ecocomDP.MACROINVERTEBRATE <- function(
  neon.data.product.id ="DP1.20120.001",
  ...){
  
  
  # get all tables for this data product for the specified sites in my_site_list, store them in a list called all_tabs
  all_tabs <- neonUtilities::loadByProduct(
    dpID = neon.data.product.id,
    ...)
  
  
  # extract the table with the field data from the all_tabs list of tables
  inv_fielddata <- all_tabs$inv_fieldData
  
  
  # extract the table with the taxonomy data from all_tabls list of tables 
  inv_taxonomyProcessed <- all_tabs$inv_taxonomyProcessed
  
  # location ----
  # get relevant location info from the data
  table_location_raw <- inv_fielddata %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct() 
  # create a location table, which has the lat long for each NEON site included in the data set
  # start with the inv_fielddata table and pull out latitude, longitude, and elevation for each NEON site that occurs in the data
  table_location <- suppressMessages(
    inv_fielddata %>% 
      ecocomDP::make_location(cols = c("domainID", "siteID", "namedLocation")))
  
  # populate latitude
  table_location$latitude <- table_location_raw$decimalLatitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  #populate longitude
  table_location$longitude <- table_location_raw$decimalLongitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # populate elevation
  table_location$elevation <- table_location_raw$elevation[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # taxon ----
  # create a taxon table, which describes each taxonID that appears in the data set
  # start with inv_taxonomyProcessed
  table_taxon <- inv_taxonomyProcessed %>%
    
    # keep only the coluns listed below
    dplyr::select(acceptedTaxonID, taxonRank, scientificName, identificationReferences) %>%
    
    # remove rows with duplicate information
    dplyr::distinct() %>%
    
    # rename some columns
    dplyr::rename(taxon_id = acceptedTaxonID,
           taxon_rank = taxonRank,
           taxon_name = scientificName,
           authority_taxon_id = identificationReferences)%>%
    dplyr::mutate(
      authority_system = "NEON_external_lab",
    )
    
  
  
  # observation ----
  # Make the observation table.
  # start with inv_taxonomyProcessed
  table_observation <- inv_taxonomyProcessed %>% 
    
    # select a subset of columns from inv_taxonomyProcessed
    dplyr::select(uid,
           sampleID,
           namedLocation, 
           collectDate,
           subsamplePercent,
           individualCount,
           estimatedTotalCount,
           acceptedTaxonID) %>%
    
    # Join the columns selected above with two columns from inv_fielddata (the two columns are sampleID and benthicArea)
    dplyr::left_join(inv_fielddata %>% dplyr::select(sampleID, benthicArea)) %>%
    
    # some new columns called 'variable_name', 'value', and 'unit', and assign values for all rows in the table.
    # variable_name and unit are both assigned the same text strint for all rows. 
    dplyr::mutate(variable_name = 'density',
           value = estimatedTotalCount / benthicArea,
           unit = 'count per square meter') %>% 
    
    # rename some columns
    dplyr::rename(observation_id = uid,
           event_id = sampleID,
           # package_id = NA,
           location_id = namedLocation,
           observation_datetime = collectDate,
           taxon_id = acceptedTaxonID) %>%
    
    # make a new column called package_id, assign it NA for all rows
    dplyr::mutate(package_id = paste0(neon.data.product.id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    
    # only keep the columns listed below
    dplyr::select(observation_id, event_id, package_id,
           location_id, observation_datetime,
           taxon_id, variable_name, value, unit)
  

  # return ----
  # list of tables to be returned, with standardized names for elements
  out_list <- list(
    location = table_location,
    taxon = table_taxon,
    observation = table_observation)
  
  # return out_list -- this is output from this function
  return(out_list)
  
} #END of function
