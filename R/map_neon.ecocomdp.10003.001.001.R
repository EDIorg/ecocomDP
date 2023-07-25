##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for BIRD taxa from neon.data.product.id DP1.10003.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for BIRD
map_neon.ecocomdp.10003.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10003.001",
  ...){
  
  #NEON target taxon group is BIRD
  neon_method_id <- "neon.ecocomdp.10003.001.001"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  allTabs_bird <- neon.data.list
  
  
  # extract NEON data tables from list object ---- 
  allTabs_bird$brd_countdata <- tidyr::as_tibble(allTabs_bird$brd_countdata)
  
  allTabs_bird$brd_perpoint <- tidyr::as_tibble(allTabs_bird$brd_perpoint)
  
  data_bird <- dplyr::left_join(
    allTabs_bird$brd_countdata,
    dplyr::select(allTabs_bird$brd_perpoint, 
                  -uid,
                  -startDate))
  
  # table(data_bird$samplingImpractical) # all NA
  # table(data_bird$samplingImpracticalRemarks)
  
  data_bird <- dplyr::select(
    data_bird, 
    # -uid, 
    -identifiedBy, 
    # -eventID, # it is just plotID, pointID, startDate
    -measuredBy,
    -samplingImpractical, -samplingImpracticalRemarks)
  
  # remove invalde records
  data_bird <- data_bird %>%
    dplyr::filter(
    is.finite(clusterSize),
    clusterSize >= 0,
    !is.na(clusterSize))
  
  
  
  #location ----
  
  
  table_location_raw <- data_bird %>%
    dplyr::select(domainID, siteID, plotID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, plotType, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", 
                            "plotType","geodeticDatum"))
  
  
  
  
  # taxon ----
  my_dots <- list(...)
  
  if("token" %in% names(my_dots)){
    my_token <- my_dots$token
  }else{
    my_token <- NA
  }
  
  # get bird taxon table from NEON
  neon_bird_taxon_table <- neonOS::getTaxonList(
    taxonType = "BIRD", 
    token = my_token) %>%
    dplyr::filter(taxonID %in% data_bird$taxonID)
  
  table_taxon <- neon_bird_taxon_table %>%
    dplyr::select(taxonID, taxonRank, scientificName, nameAccordingToID) %>%
    dplyr::distinct() %>% 
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = nameAccordingToID) %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) 
  
  
  
  
  
  # observation ----
  my_package_id = paste0(
    neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  table_observation_wide_all <- data_bird %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(package_id = my_package_id) %>%
    dplyr:: rename(
      observation_id = uid, 
      event_id = eventID,
      datetime = startDate, 
      taxon_id = taxonID,
      value = clusterSize) %>%
    dplyr::mutate(
      variable_name = "cluster size",
      unit = "count of individuals")  %>%
    dplyr::filter(!is.na(taxon_id))
  
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
      unit)
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_wide_all,
    ancillary_var_names = c(
      "observation_id",
      "pointID",
      "pointCountMinute",
      "targetTaxaPresent",
      "observerDistance",
      "detectionMethod",
      "visualConfirmation",
      "sexOrAge",
      "clusterCode",
      "nativeStatusCode",
      "endCloudCoverPercentage",
      "observedHabitat",
      "observedAirTemp",
      "startCloudCoverPercentage",
      "endCloudCoverPercentage",
      "startRH",
      "endRH",
      "kmPerHourObservedWindSpeed",
      "laboratoryName",
      "samplingProtocolVersion",
      "remarks",
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