##############################################################################################
#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for ALGAE from neon.data.product.id DP1.20166.001 (Periphyton, seston, and phytoplankton collection) from the NEON data portal and map to the ecocomDP format
#' 
#' @export

# changelog and author contributions / copyrights
#   Eric R Sokol (2020-04-17)
#     original creation
##############################################################################################
map_neon.ecocomdp.20166.001.002 <- function(
  neon.data.product.id = "DP1.20166.001",
  ...
){
  
  #NEON target taxon group is ALGAE
  neon_method_id <- "neon.ecocomdp.20166.001.001"
  
  # get all tables
  all_tabs_in <- neonUtilities::loadByProduct(
    dpID = neon.data.product.id, 
    ...)
  
  
  
  # tables needed for ALGAE to populate the ecocomDP tables
  if("alg_fieldData" %in% names(all_tabs_in)){
    field_data_in <- all_tabs_in$alg_fieldData
  }else{
    # field_data_in <- data.frame()
    # if no data, return an empty list
    warning(paste0(
      "WARNING: No field data available for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())
  }
  
  
  if("alg_taxonomyProcessed" %in% names(all_tabs_in)){
    tax_long_in <- all_tabs_in$alg_taxonomyProcessed
  }else{
    # tax_long_in <- data.frame()
    # if no data, return an empty list
    warning(paste0(
      "WARNING: No taxon count data available for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())
  }
  
  
  if("alg_biomass" %in% names(all_tabs_in)){
    biomass_in <- all_tabs_in$alg_biomass
  }else{
    # if no data, return an empty list
    warning(paste0(
      "WARNING: Missing required data for calculating taxon densities for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())  
  }
  
  
  #Observation table
  
  ###change NA"s to 0"s in tax_long_in data for calculations only
  # tax_long_in$perBottleSampleVolume[is.na(tax_long_in$perBottleSampleVolume)] <- 0
  #join algae biomass and taxonomy data 

  

  
  # Note that observational data are in the tax table returned by the lab, 
  # however, we need "fieldSampleVolume" from the biomass table to standardize
  # algal counts returned by the lab. Thus we"re joining tables here by sampleID, 
  # but only keeping sampleID, parentSampleID, and fieldSampleVolume from the biomass table.
  
  alg_tax_biomass <- biomass_in %>%
    dplyr::select(parentSampleID, sampleID, fieldSampleVolume) %>%
    dplyr::distinct() %>%
    dplyr::left_join(tax_long_in, by = "sampleID") %>%
    dplyr::distinct()

  
  
  
  # only keep cols in field_data_in that are unique to that table
  field_data_names_to_keep <- c("parentSampleID",
                                names(field_data_in) %>% 
                                  dplyr::setdiff(names(alg_tax_biomass)))
  
  field_data_in <- field_data_in[,field_data_names_to_keep] %>%
    dplyr::distinct()
  
  
 
  # create the observation table by joining with field_data_in
  table_observation_raw <- alg_tax_biomass %>% 
    dplyr::left_join(field_data_in, by = "parentSampleID") %>%
    dplyr::filter(algalParameterUnit=="cellsPerBottle") %>%
    dplyr::mutate(
      density = dplyr::case_when(
        algalSampleType %in% c("seston", "phytoplankton") ~ algalParameterValue / perBottleSampleVolume,
        #add phytoplankton back in when applicable
        TRUE ~ (algalParameterValue / perBottleSampleVolume) * (fieldSampleVolume / (benthicArea * 10000))),
      cell_density_standardized_unit = dplyr::case_when(
        algalSampleType %in% c("phytoplankton","seston") ~ "cells/mL",
        TRUE ~ "cells/cm2"))

  
  
  
  # rename fields for ecocomDP
  table_observation_ecocomDP <- table_observation_raw %>%
    dplyr::mutate(
      package_id = paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    dplyr::rename(
      observation_id = uid, 
      neon_sample_id = sampleID,
      neon_event_id = eventID,
      location_id = namedLocation,
      observation_datetime = collectDate,
      taxon_id = acceptedTaxonID,
      variable_name = algalParameter,
      value = density,
      unit = cell_density_standardized_unit) %>%
    dplyr::mutate(
      event_id = neon_sample_id
    )
  

  # make observation table
  table_observation <- table_observation_ecocomDP %>%
    dplyr::select(
      observation_id,
      event_id,
      package_id,
      location_id,
      observation_datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>% 
    dplyr::distinct()
  
  
  
  # make observation ancillary table. First convert POSIXct POSIXt classed
  # variables to character, otherwise gathering will produce a warning.
  table_observation_ecocomDP$identifiedDate <- as.character(
    table_observation_ecocomDP$identifiedDate)
  
  col_names_to_keep <- c(
    "event_id",
    "neon_sample_id",
    "neon_event_id",
    "parentSampleID",
    "sampleCondition",
    "laboratoryName",
    "perBottleSampleVolume",
    "aquaticSiteType",
    "habitatType",
    "algalSampleType",
    "samplerType",
    "benthicArea",
    "samplingProtocolVersion",
    "phytoDepth1","phytoDepth2","phytoDepth3",
    "substratumSizeClass")
  
  table_observation_ancillary <- table_observation_ecocomDP[
    ,names(table_observation_ecocomDP) %in% col_names_to_keep
  ] %>%
    dplyr::distinct() 
  
  table_observation_ancillary <- table_observation_ancillary %>% 
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(
      cols = -event_id,
      names_to = "variable_name", 
      values_to = "value") %>% 
    dplyr::mutate(
      observation_ancillary_id = paste0(variable_name, "_for_", event_id)) %>%
    dplyr::filter(!is.na(value))
  
  # table_observation_ancillary$observation_ancillary_id <- 
  #   paste0("obsan_", seq(nrow(table_observation_ancillary)))
  
  table_observation_ancillary$unit <- NA_character_
  table_observation_ancillary <- dplyr::select(
    table_observation_ancillary,
    observation_ancillary_id,
    event_id,
    variable_name,
    value,
    unit) %>% 
    dplyr::distinct()
  
  
  
  # ecocomDP location table
  
  # get relevant location info from the data
  table_location_raw <- table_observation_raw %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct() 
  
  # make ecocomDP format location table, identifying hierarchical spatial structure
  table_location <- suppressMessages(
    ecocomDP::make_location(
      table_observation_raw,
      cols = c("domainID", "siteID", "namedLocation")))

  # code to handle updated make_location (updated 18 Sep 2020 in make_location branch)
  if(class(table_location) == "list" &&
     "location" %in% names(table_location)){
    
    table_location <- table_location$location %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        location_name = location_name %>% 
          strsplit("=") %>%
          unlist() %>%
          dplyr::last()) 
  }
  
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
      location_ancillary_id = paste0("NEON_location_type_",location_id)) %>%
    dplyr::distinct()
  
  
  # # Taxon table using available data
  # 
  # # NOTE: we could use the NEON taxon tables API as an alternative to populate
  # # the taxon table. We could make it more standardized, but it might be 
  # # more resource intensive
  # table_taxon_raw <- tax_long_in %>%
  #   dplyr::select(acceptedTaxonID, taxonRank, scientificName, identificationReferences) %>%
  #   dplyr::distinct() 
  
  # # make ecocomDP format taxon table using ecocomDP function
  # table_taxon <- ecocomDP::make_taxon(
  #     taxa = table_taxon_raw$scientificName,
  #     taxon.id = table_taxon_raw$acceptedTaxonID,
  #     name.type = "scientific",
  #     data.sources = 3)
  
  # # alternative for making ecocomDP format taxon table 
  # table_taxon <- table_taxon_raw %>%
  #   dplyr::rename(
  #     taxon_id = acceptedTaxonID,
  #     taxon_rank = taxonRank,
  #     taxon_name = scientificName,
  #     authority_taxon_id = identificationReferences
  #   ) %>%
  #   dplyr::mutate(
  #     authority_system = "NEON_external_lab",
  #   )
  
  
  table_taxon <- tax_long_in %>%
    
    # keep only the coluns listed below
    dplyr::select(acceptedTaxonID, taxonRank, scientificName, 
                  identificationReferences, laboratoryName) %>%
    
    # if no idenficationReference provided, fill in authority_system with laboratoryName
    dplyr::mutate(
      identificationReferences = dplyr::case_when(
        is.na(identificationReferences) ~ laboratoryName,
        TRUE ~ identificationReferences)) %>%
    
    # remove rows with duplicate information
    dplyr::distinct() %>%
    
    # rename some columns
    dplyr::rename(taxon_id = acceptedTaxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = identificationReferences) %>%
    dplyr::select(taxon_id, taxon_rank, taxon_name, authority_system) %>%
    
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarize(
      authority_system = paste(authority_system, collapse = " | ")
    )
  
  
  # make dataset_summary -- required table
  years_in_data <- table_observation$observation_datetime %>% lubridate::year()
  # years_in_data %>% ordered()
  
 
  
  table_dataset_summary <- data.frame(
    package_id = table_observation$package_id[1],
    original_package_id = neon.data.product.id,
    length_of_survey_years = max(years_in_data) - min(years_in_data) + 1,
    number_of_years_sampled	= years_in_data %>% unique() %>% length(),
    std_dev_interval_betw_years = years_in_data %>% 
      unique() %>% sort() %>% diff() %>% stats::sd(),
    max_num_taxa = table_taxon$taxon_id %>% unique() %>% length()
  )
  
  
  # list of tables to be returned, with standardized names for elements
  out_list <- list(
    location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  
  
  return(out_list)
}