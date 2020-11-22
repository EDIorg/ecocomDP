# general wrapper function for mapping NEON data products to ecocomDP

map_neon_data_to_ecocomDP <- function(
  dpID,
  ... #arguments set to neonUtilities::loadByProduct
){
  
  # get metadata
  metadata_all <- neonUtilities::getProductInfo(dpID)
  
  # call custom mapping function if available for given NEON dpID
  if(dpID == "DP1.20166.001"){
    ecocomDP_tables <- map_neon_data_to_ecocomDP.ALGAE(...)
  }else{
    message(paste0('WARNING: ecocomDP mapping not currently available for ',dpID))
    ecocomDP_tables <- list(
      location = data.frame(),
      taxon = data.frame(),
      observation = data.frame())
  }
  
  # combine neon metadata and ecocomDP tables
  out_list <- list(
    neon_metadata = metadata_all,
    ecocomDP_tables = ecocomDP_tables)
  
  # browser()
  
  # return
  return(out_list)
}




# fxn to get and map ALGAE data

map_neon_data_to_ecocomDP.ALGAE <- function(
  ...
){
  
  
  
  # get all tables
  all_tabs_in <- neonUtilities::loadByProduct(
    # hard coded arguments
    dpID = "DP1.20166.001", 
    package = "basic", 
    
    # dots for passing user input
    ...
    
    # # required input from users
    # site = c("MAYF", "PRIN"), 
    # startdate = "2016-1", 
    # enddate = "2018-11",
    
    # # optional input from users
    # avg = "all",
    # check.size = FALSE,
    # nCores = 1,
    # forceParallel = FALSE,
  )
  
  
  
  # tables needed for ALGAE to populate the ecocomDP tables
  
  field_data_in <- all_tabs_in$alg_fieldData
  tax_long_in <- all_tabs_in$alg_taxonomyProcessed
  biomass_in <-all_tabs_in$alg_biomass
  
  
  
  #Observation table
  
  ###change NA's to 0's in tax_long_in data for calculations only
  # tax_long_in$perBottleSampleVolume[is.na(tax_long_in$perBottleSampleVolume)] <- 0
  #join algae biomass and taxonomy data 
  
  
  
  # browser()
  
  
  
  # Note that observational data are in the tax table returned by the lab, 
  # however, we need "fieldSampleVolume" from the biomass table to standardize
  # algal counts returned by the lab. Thus we're joining tables here by sampleID, 
  # but only keeping sampleID, parentSampleID, and fieldSampleVolume from the biomass table.
  
  alg_tax_biomass <- biomass_in %>%
    select(parentSampleID, sampleID, fieldSampleVolume) %>%
    distinct() %>%
    left_join(tax_long_in, by = "sampleID") %>%
    distinct()
  
  
  # browser()
  
  
  
  # only keep cols in field_data_in that are unique to that table
  field_data_names_to_keep <- c('parentSampleID',
                                names(field_data_in) %>% setdiff(names(alg_tax_biomass)))
  
  field_data_in <- field_data_in[,field_data_names_to_keep] %>%
    distinct()
  
  # create the observation table by joining with field_data_in
  table_observation_raw <- alg_tax_biomass %>% 
    left_join(field_data_in, by = "parentSampleID") %>%
    filter(algalParameterUnit=='cellsPerBottle')%>%
    mutate(density=
             case_when(
               algalSampleType %in% c('seston') ~ algalParameterValue / perBottleSampleVolume,
               TRUE ~ (algalParameterValue / perBottleSampleVolume) * (fieldSampleVolume / benthicArea) #add phytoplankton back in when applicable
             ),cell_density_standardized_unit = case_when(
               algalSampleType == 'phytoplankton' ~ 'cells/mL',
               TRUE ~ 'cells/m2'))
  
  # rename fields for ecocomDP
  table_observation_ecocomDP <- table_observation_raw %>%
    mutate(
      eventIDpackage_id = NA) %>%
    rename(
      observation_id = uid, 
      location_id = namedLocation,
      observation_datetime = collectDate,
      taxon_id = acceptedTaxonID,
      variable_name = algalParameter,
      value = density,
      unit = cell_density_standardized_unit
    ) 
  
  # make observation table
  table_observation <- table_observation_ecocomDP %>%
    select(
      observation_id,
      eventIDpackage_id,
      location_id,
      observation_datetime,
      taxon_id,
      variable_name,
      value,
      unit
    ) %>% distinct()
  
  # make observation ancillary table
  table_observation_ancillary <- table_observation_ecocomDP %>%
    select(
      -c(
        eventIDpackage_id,
        location_id,
        observation_datetime,
        taxon_id,
        variable_name,
        value,
        unit
      )) %>% distinct()
  
  

  # ecocomDP location table
  
  # get relevant location info from the data
  table_location_raw <- table_observation_raw %>%
    select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
    distinct() 
  
  # make ecocomDP format location table, identifying hierarchical spatial structure
  table_location <- ecocomDP::make_location(
    table_observation_raw,
    cols = c("domainID", "siteID", "namedLocation"))
  table_location <- table_location$location
  
  # populate latitude
  table_location$latitude <- table_location_raw$decimalLatitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  #populate longitude
  table_location$longitude <- table_location_raw$decimalLongitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # populate elevation
  table_location$elevation <- table_location_raw$elevation[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  
  

  # # Taxon table using available data
  # 
  # # NOTE: we could use the NEON taxon tables API as an alternative to populate
  # # the taxon table. We could make it more standardized, but it might be 
  # # more resource intensive
  table_taxon_raw <- tax_long_in %>%
    select(acceptedTaxonID, taxonRank, scientificName, identificationReferences) %>%
    distinct() 
  
  # # make ecocomDP format taxon table using ecocomDP function
  # table_taxon <- ecocomDP::make_taxon(
  #     taxa = table_taxon_raw$scientificName,
  #     taxon.id = table_taxon_raw$acceptedTaxonID,
  #     name.type = 'scientific',
  #     data.sources = 3)
  
  # alternative for making ecocomDP format taxon table 
  table_taxon <- table_taxon_raw %>%
    rename(
      taxon_id = acceptedTaxonID,
      taxon_rank = taxonRank,
      taxon_name = scientificName,
      authority_taxon_id = identificationReferences
    ) %>%
    mutate(
      authority_system = 'NEON_external_lab',
    )
  
  
  
  
  # list of tables to be returned, with standardized names for elements
  out_list <- list(
    location = table_location,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary)
}
