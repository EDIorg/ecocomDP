##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for ZOOPLANKTON taxa from neon.data.product.id DP1.20219.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for ZOOPLANKTON taxa
map_neon.ecocomdp.20219.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.20219.001",
  ...){
  
  #NEON target taxon group is ZOOPLANKTON
  neon_method_id <- "neon.ecocomdp.20219.001.001"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  ### Get the Data
  allTabs_zoop <- neon.data.list
  
  

  # download field data
  zoo_fielddata <- tidyr::as_tibble(allTabs_zoop$zoo_fieldData)
  
  # download zooplankton counts
  zoo_taxonomyProcessed <- tidyr::as_tibble(allTabs_zoop$zoo_taxonomyProcessed)
  
  # Location table
  zoo_location <- zoo_fielddata %>%
    dplyr::select(domainID:sampleID, samplerType, towsTrapsVolume) %>%
    dplyr::select(-additionalCoordUncertainty) %>%
    dplyr::distinct()
  

  
  # Observation table
  data_zooplankton <- zoo_taxonomyProcessed %>%
    dplyr::filter(sampleCondition == "condition OK") %>%
    dplyr::select(
      uid, 
      sampleID, subsampleType,
      taxonID, scientificName, family, taxonRank, identificationReferences,
      # zooVolumePerBottle,
      # zooSubsampleVolume,
      # individualCount,
      adjCountPerBottle,
      zooMinimumLength, zooMaximumLength, zooMeanLength,
      laboratoryName,
      release, publicationDate) %>%
    dplyr::left_join(zoo_location, by = "sampleID") %>%
    dplyr::mutate(density = adjCountPerBottle / towsTrapsVolume,
                  density_unit = "count per liter") %>%
    dplyr::distinct() %>%
    dplyr::filter(
      !is.na(density),
      is.finite(density),
      density >= 0)
  
  
  

  # taxon ----
  # create a taxon table, which describes each taxonID that appears in the data set
  table_taxon <- data_zooplankton %>%
    
    # keep only the coluns listed below
    dplyr::select(taxonID, taxonRank, scientificName, identificationReferences) %>%
    
    # remove rows with duplicate information
    dplyr::distinct() %>%
    
    # rename some columns
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = identificationReferences) %>%
    # concatenate different references for same taxonID
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarize(
      authority_system = paste(authority_system, collapse = "; "))
  
  

  
  # location ----
  # get relevant location info from the data
  table_location_raw <- data_zooplankton %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation,
                  namedLocation, aquaticSiteType, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "aquaticSiteType", "geodeticDatum"))
  
  
  

  
  # observation ----
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  table_observation_wide_all <- data_zooplankton %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr:: rename(
      observation_id = uid, 
      neon_event_id = eventID,
      neon_sample_id = sampleID,
      datetime = collectDate, 
      taxon_id = taxonID,
      value = density,
      unit = density_unit) %>%
    dplyr::mutate(
      event_id = neon_sample_id,
      variable_name = "density") 
  
  table_observation <- table_observation_wide_all %>%
    dplyr::select(
      observation_id,
      event_id,
      package_id,
      location_id,
      datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>%
    dplyr::filter(!is.na(taxon_id))
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_wide_all,
    ancillary_var_names = c(
      "observation_id",
      "neon_event_id",
      "neon_sample_id",
      "samplerType",
      "towsTrapsVolume",
      "laboratoryName",
      "release",
      "publicationDate"))
  
  
  # data summary ----
  # make dataset_summary -- required table
  
  years_in_data <- table_observation$datetime %>% lubridate::year()
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
  
  
  
  # return tables ----
  
  out_list <- list(
    location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  return(out_list)
}