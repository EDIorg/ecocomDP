##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for MACROINVERTEBRATE from neon.data.product.id DP1.20120.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################

map_neon.ecocomdp.20120.001.001 <- function(
  neon.data.list,
  neon.data.product.id ="DP1.20120.001",
  ...){
  #NEON target taxon group is MACROINVERTEBRATE
  neon_method_id <- "neon.ecocomdp.20120.001.001"
 
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  
  # get all tables for this data product for the specified sites in my_site_list, store them in a list called all_tabs
  all_tabs <- neon.data.list
   
  
  # extract the table with the field data from the all_tabs list of tables
  if("inv_fieldData" %in% names(all_tabs)){
    inv_fielddata <- all_tabs$inv_fieldData %>%
      dplyr::filter(!is.na(sampleID))
    
    # known problem with dupes published in the inv_fieldData table as of 2021-02-18
    # this anticipated to be fixed in data release next year (Jan 2022)
    # use sampleID as primary key, keep the first uid associated with any sampleID that has multiple uids

    de_duped_uids <- inv_fielddata %>% 
      dplyr::group_by(sampleID) %>%
      dplyr::summarise(n_recs = length(uid),
                       n_unique_uids = length(unique(uid)),
                       uid = dplyr::first(uid))
    
    inv_fielddata <- de_duped_uids %>%
      dplyr::select(uid, sampleID) %>%
      dplyr::left_join(inv_fielddata)

  }else{
    # if no data, return an empty list
    warning(paste0(
      "WARNING: No field data available for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())
  }
 
  
  # extract the table with the taxonomy data from all_tabls list of tables
  if("inv_taxonomyProcessed" %in% names(all_tabs)){
    inv_taxonomyProcessed <- all_tabs$inv_taxonomyProcessed
  }else{
    # if no data, return an empty list
    warning(paste0(
      "WARNING: No taxon count data available for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())
  }
 

  # observation ----
  
  my_package_id <- paste0(
    neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  
  # Make the observation table.
  # start with inv_taxonomyProcessed
  # NOTE: the observation_id = uuid for record in NEON's inv_taxonomyProcessed table 
  table_observation_raw <- inv_taxonomyProcessed %>% 
    dplyr::filter(targetTaxaPresent == "Y") %>%
    dplyr::distinct() %>% 
    # add counts for taxa (e.g., different size classes) in same sample together
    dplyr::group_by(sampleID, acceptedTaxonID, scientificName, taxonRank, ) %>%
    dplyr::summarize(
      estimatedTotalCount = sum(estimatedTotalCount, na.rm = TRUE),
      individualCount = sum(individualCount),
      subsamplePercent = subsamplePercent %>% unique() %>% paste(collapse = "|"),
      identificationReferences = identificationReferences %>% unique() %>% paste(collapse = "|"),
      laboratoryName = laboratoryName %>% unique() %>% paste(collapse = "|"),
      publicationDate = publicationDate %>% unique() %>% paste(collapse = "|"),
      release = release %>% unique() %>% paste(collapse = "|")
    ) %>%
    # Join the columns selected above with two columns from inv_fielddata (the two columns are sampleID and benthicArea)
    dplyr::left_join(
      inv_fielddata %>% 
        dplyr::select(
          sampleID, benthicArea,
          collectDate,
          namedLocation,
          eventID, habitatType,
          samplerType, substratumSizeClass,
          ponarDepth, snagLength, snagDiameter,
          remarks)) %>%
    # some new columns called 'variable_name', 'value', and 'unit', and assign values for all rows in the table.
    # variable_name and unit are both assigned the same text strint for all rows. 
    dplyr::ungroup() %>%
    dplyr::mutate(         
      observation_id = paste0("obs_",1:nrow(.)),
      neon_sample_id = sampleID,
      variable_name = 'density',
      value = estimatedTotalCount / benthicArea,
      unit = 'count per square meter') %>% 
    # rename some columns
    dplyr::rename(
      event_id = sampleID,
      neon_event_id = eventID,
      datetime = collectDate,
      location_id = namedLocation,
      taxon_id = acceptedTaxonID) %>%
    # make a new column called package_id, assign it NA for all rows
    dplyr::mutate(package_id = my_package_id) %>%
    # filter out invalid records
    dplyr::filter(
      !is.na(value),
      value >= 0,
      is.finite(value))
  
  table_observation <- table_observation_raw %>%
    dplyr::select(observation_id, 
                  event_id, 
                  package_id, 
                  location_id, 
                  datetime, 
                  taxon_id, 
                  variable_name, 
                  value,
                  unit) %>%
    dplyr::distinct()
  
  
  
  # ancillary observation table ----
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_raw,
    ancillary_var_names = c(
      "observation_id",
      "sampleID",
      "neon_event_id",
      "individualCount",
      "subsamplePercent",
      "estimatedTotalCount",
      "benthicArea",
      "habitatType",
      "samplerType", "substratumSizeClass",
      "ponarDepth", "snagLength", "snagDiameter",
      "laboratoryName",
      "remarks",
      "release",
      "publicationDate"))
  
  
  
  # location ----
  # get relevant location info from the data
  table_location_raw <- inv_fielddata %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude,
                  aquaticSiteType,
                  decimalLongitude, elevation) %>%
    dplyr::distinct() %>%
    dplyr::filter(namedLocation %in% table_observation$location_id)
  
  # create a location table, which has the lat long for each NEON site included in the data set
  # start with the inv_fielddata table and pull out latitude, longitude, and elevation for each NEON site that occurs in the data
  
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation","aquaticSiteType"))
  
  

  # taxon ----
  # create a taxon table, which describes each taxonID that appears in the data set
  # start with inv_taxonomyProcessed
  
  # This approach takes too long
  # my_dots <- list(...)
  # 
  # if("token" %in% names(my_dots)){
  #   my_token <- my_dots$token
  # }else{
  #   my_token <- NA
  # }
  # 
  # # get macroinvert taxon table from NEON
  # neon_inv_taxon_table <- neonOS::getTaxonList(
  #   taxonType = "MACROINVERTEBRATE", 
  #   token = my_token) %>%
  #   dplyr::filter(taxonID %in% table_observation$taxon_id)
  # 
  # table_taxon <- neon_inv_taxon_table %>%
  #   dplyr::select(taxonID, taxonRank, scientificName, nameAccordingToID) %>%
  #   dplyr::distinct() %>% 
  #   dplyr::rename(taxon_id = taxonID,
  #                 taxon_rank = taxonRank,
  #                 taxon_name = scientificName,
  #                 authority_system = nameAccordingToID) %>%
  #   dplyr::select(taxon_id,
  #                 taxon_rank,
  #                 taxon_name,
  #                 authority_system) 
  

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
                  authority_system = identificationReferences) %>%
    # concatenate different references for same taxonID
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarize(
      authority_system = paste(authority_system, collapse = "; ")) %>%
    dplyr::filter(taxon_id %in% table_observation$taxon_id)
  
  
  
  
  # make dataset_summary -- required table ----
  years_in_data <- table_observation$datetime %>% lubridate::year()
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