##############################################################################################
# @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for ALGAE from neon.data.product.id DP1.20166.001 (Periphyton, seston, and phytoplankton collection) from the NEON data portal and map to the ecocomDP format
##############################################################################################
map_neon.ecocomdp.20166.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.20166.001",
  ...
){
  #NEON target taxon group is ALGAE
  neon_method_id <- "neon.ecocomdp.20166.001.001"
  
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  
  # get all tables
  all_tabs_in <- neon.data.list
  
  
  
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
    
    # biomass_in <- all_tabs_in$alg_biomass
    
    alg_biomass <- tidyr::as_tibble(all_tabs_in$alg_biomass) %>%
      dplyr::mutate(estPerBottleSampleVolume = preservativeVolume + labSampleVolume) %>%
      dplyr::filter(analysisType == 'taxonomy')
    
  }else{
    # if no data, return an empty list
    warning(paste0(
      "WARNING: Missing required data for calculating taxon densities for NEON data product ",
      neon.data.product.id, " for the dates and sites selected."))
    return(list())  
  }
  
  
  #Observation table ----
  
  ###change NA"s to 0"s in tax_long_in data for calculations only
  # tax_long_in$perBottleSampleVolume[is.na(tax_long_in$perBottleSampleVolume)] <- 0
  #join algae biomass and taxonomy data 
  
  
  # Note that observational data are in the tax table returned by the lab, 
  # however, we need "fieldSampleVolume" from the biomass table to standardize
  # algal counts returned by the lab. Thus we"re joining tables here by sampleID, 
  # but only keeping sampleID, parentSampleID, and fieldSampleVolume from the biomass table.
  
  alg_tax_biomass <- alg_biomass %>%
    dplyr::select(parentSampleID, sampleID, fieldSampleVolume, estPerBottleSampleVolume) %>%
    dplyr::distinct() %>%
    dplyr::left_join(tax_long_in, 
                     by = "sampleID",
                     multiple = "all") %>%
    dplyr::mutate(perBottleSampleVolume =
                    dplyr::case_when(
                      is.na(perBottleSampleVolume) ~ estPerBottleSampleVolume,
                      perBottleSampleVolume == 0 ~ estPerBottleSampleVolume,
                      perBottleSampleVolume > 0 ~ perBottleSampleVolume)) %>%
    dplyr::distinct()
  
  
  # alg_tax_biomass <- biomass_in %>%
  #   dplyr::select(parentSampleID, sampleID, fieldSampleVolume) %>%
  #   dplyr::distinct() %>%
  #   dplyr::left_join(tax_long_in, by = "sampleID") %>%
  #   dplyr::distinct()
  
  
  
  
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
        TRUE ~ "cells/cm2")) %>%
    dplyr::filter(
      sampleCondition == "Condition OK",
      !is.na(density),
      density >= 0,
      is.finite(density))
  
  # check for dups ----
  dup_samples <- table_observation_raw %>%
    dplyr::group_by(
      sampleID, acceptedTaxonID) %>%
    dplyr::summarize(
      n_recs = dplyr::n(),
      # unique_vals = paste(unique(density), collapse = "|"),
      density_aggregate = sum(density)) %>%
    dplyr::filter(n_recs > 1)
  
  # filter out duplicates
  obs_raw_no_dups <- table_observation_raw %>%
    dplyr::anti_join(
      dup_samples %>% dplyr::select(sampleID, acceptedTaxonID))
  
  # for dup taxa, use summed densities
  obs_raw_summed_dups <- dup_samples %>% 
    dplyr::select(
      sampleID, acceptedTaxonID, density_aggregate) %>% 
    dplyr::left_join(
      table_observation_raw %>% 
        dplyr::select(-density),
      multiple = "first") %>%
    dplyr::rename(
      density = density_aggregate)
  
  # recombine data
  table_observation_raw <- dplyr::bind_rows(
    obs_raw_no_dups, 
    obs_raw_summed_dups)
  
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  # rename fields for ecocomDP
  table_observation_ecocomDP <- table_observation_raw %>%
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr::rename(
      observation_id = uid, 
      neon_sample_id = sampleID,
      neon_event_id = eventID,
      location_id = namedLocation,
      datetime = collectDate,
      taxon_id = acceptedTaxonID,
      value = density,
      unit = cell_density_standardized_unit) %>%
    dplyr::mutate(
      event_id = neon_sample_id,
      variable_name = "cell density"
    )
  
  
  
  # make observation table
  table_observation <- table_observation_ecocomDP %>%
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
    dplyr::distinct()
  
  
  
  
  
  
  # make observation ancillary table. First convert POSIXct POSIXt classed
  # variables to character, otherwise gathering will produce a warning.
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_ecocomDP,
    ancillary_var_names = c(
      "observation_id",
      "neon_sample_id",
      "neon_event_id",
      "parentSampleID",
      "sampleCondition",
      "laboratoryName",
      "perBottleSampleVolume",
      "algalSampleType",
      "samplerType",
      "habitatType",
      "benthicArea",
      "samplingProtocolVersion",
      "phytoDepth1","phytoDepth2","phytoDepth3",
      "substratumSizeClass",
      "release",
      "publicationDate"))
  
  
  
  # location ----
  # get relevant location info from the data, use neon helper functions 
  # to make location and ancillary location tables
  
  table_location_raw <- table_observation_raw %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  aquaticSiteType, 
                  decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct()
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation","aquaticSiteType"))
  
  
  
  
  # # Taxon table using available data
  
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
    # concatenate different references for same taxonID
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarize(
      authority_system = paste(authority_system, collapse = "; ")) %>%
    dplyr::filter(taxon_id %in% table_observation$taxon_id)
  
  
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