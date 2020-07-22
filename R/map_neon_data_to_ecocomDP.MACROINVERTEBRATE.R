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
  
  # replace parent_id with parent_name
  table_location$parent_location_id <- table_location$location_name[
    match(table_location$parent_location_id, table_location$location_id)]
  
  # replace loc_id with meaningful names
  table_location$location_id <- table_location$location_name
  
  # get neon location info lookup tables
  neon_domain_list <- neon_site_list %>%
    dplyr::select(`Domain Number`, `Domain Name`) %>%
    dplyr::distinct()
  
  neon_site_info_list <- neon_site_list %>%
    dplyr::select(-c(`Domain Number`,`Domain Name`)) %>%
    dplyr::distinct()
  

  
  # update location_names and lat longs where possible
  for(location_id in table_location$location_id){
    if(location_id %in% neon_domain_list$`Domain Number`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_domain_list$`Domain Name`[neon_domain_list$`Domain Number`==location_id]
    }else if(location_id %in% neon_site_info_list$`Site ID`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_site_info_list$`Site Name`[neon_site_info_list$`Site ID`==location_id]
      
      table_location$latitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Latitude[neon_site_info_list$`Site ID`==location_id]
      
      table_location$longitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Longitude[neon_site_info_list$`Site ID`==location_id]
    
    }
  }
  
  

  
  # make ancillary table that indicates the location type 
  table_location_ancillary <- table_location_raw %>% 
    dplyr::select(domainID, siteID, namedLocation) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "value",
      values_to = "location_id") %>%
    dplyr::mutate(
      variable_name = "NEON location type",
      location_ancillary_id = paste0("NEON_location_type_",location_id))
  
  

  
  
  
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
           authority_system = identificationReferences)
    
  
  

  
  
  # observation ----
  # Make the observation table.
  # start with inv_taxonomyProcessed
  # NOTE: the observation_id = uuid for record in NEON's inv_taxonomyProcessed table 
  table_observation <- inv_taxonomyProcessed %>% 
    # select a subset of columns from inv_taxonomyProcessed
    dplyr::select(uid,
                  sampleID,
                  namedLocation, 
                  collectDate,
                  subsamplePercent,
                  individualCount,
                  estimatedTotalCount,
                  acceptedTaxonID, 
                  domainID, 
                  siteID) %>%
    dplyr::distinct() %>% 
    
    # suppressMessages(ecocomDP::make_location(cols = c("domainID", "siteID", "namedLocation"))) %>% 
    
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
                  observation_datetime = collectDate,
                  taxon_id = acceptedTaxonID) %>%
    
    # make a new column called package_id, assign it NA for all rows
    dplyr::mutate(package_id = paste0(neon.data.product.id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>% 
    dplyr::left_join(table_location, by = c("namedLocation" = "location_name")) %>% 
    dplyr::select(observation_id, 
                  event_id, 
                  package_id, 
                  location_id, 
                  observation_datetime, 
                  taxon_id, 
                  variable_name, 
                  value,
                  unit)
  
  
  
  
  # only keep the columns listed below
  # dplyr::select(observation_id, event_id, package_id,
  #        location_id, observation_datetime,
  #        taxon_id, variable_name, value, unit)
  

  table_observation_ancillary_wide <- inv_fielddata %>% 
    dplyr::select(eventID, sampleID) %>% 
    dplyr::filter(!is.na(sampleID)) %>%
    dplyr::rename(neon_sample_id = sampleID,
           neon_event_id = eventID) %>% 
    dplyr::mutate(event_id = neon_sample_id) 
  
  table_observation_ancillary <- table_observation_ancillary_wide %>%
    tidyr::pivot_longer(
      cols = -event_id,
      names_to = "variable_name",
      values_to = "value") %>% 
    dplyr::mutate(
      observation_ancillary_id = paste0(variable_name, "_for_", event_id))
    
  




  # make dataset_summary -- required table
  years_in_data <- table_observation$observation_datetime %>% lubridate::year()
  years_in_data %>% ordered()
  
  table_dataset_summary <- data.frame(
    package_id = table_observation$package_id[1],
    original_package_id = neon.data.product.id,
    length_of_survey_years = max(years_in_data) - min(years_in_data) + 1,
    number_of_years_sampled	= years_in_data %>% unique() %>% length(),
    std_dev_interval_betw_years = years_in_data %>% 
      unique() %>% sort() %>% diff() %>% stats::sd(),
    max_num_taxa = table_taxon$taxon_id %>% unique() %>% length()
  )
  
  

  # return ----
  # list of tables to be returned, with standardized names for elements
  out_list <- list(
    location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  # return out_list -- this is output from this function
  return(out_list)
  
} #END of function

# my_result <- map_neon_data_to_ecocomDP.MACROINVERTEBRATE(site= c('COMO','LECO'), startdate = "2019-06",enddate = "2019-09")
# my_result <- map_neon_data_to_ecocomDP.MACROINVERTEBRATE(site= c('COMO','LECO'),check.size = FALSE)
# my_result <- map_neon_data_to_ecocomDP(neon.data.product.id = "DP1.20120.001", site= c('COMO','LECO'), check.size = FALSE, token = Sys.getenv("NEON_TOKEN"))
# my_result <- read_data(id = "DP1.20120.001", site= c('COMO','LECO'), check.size = FALSE, token = Sys.getenv("NEON_TOKEN"))


