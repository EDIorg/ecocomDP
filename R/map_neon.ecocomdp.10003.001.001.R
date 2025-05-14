##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for BIRD taxa from neon.data.product.id DP1.10003.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for BIRD
map_neon.ecocomdp.10003.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10003.001",
  ...){
  
  # devtools::install_github('NEONScience/NEON-geolocation/geoNEON', force = T) 
  library(geoNEON)

  
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
  brd_countdata <- tidyr::as_tibble(allTabs_bird$brd_countdata)
  
  brd_perpoint <- tidyr::as_tibble(allTabs_bird$brd_perpoint)
  
  
  # get pointID-specific location information
  brd_perpoint_locs <- geoNEON::getLocTOS(data = brd_perpoint,
                                        dataProd = "brd_perpoint",
                                        token = neon_token)

  # Create the event table (location for specific pointIDs)
  brd_perpoint_mod <- brd_perpoint_locs %>% select(c(-decimalLatitude, -decimalLongitude)) %>%
  rename(
    locality = points,
    eventRemarks = remarks,
    eventDate = startDate,
    decimalLatitude = adjDecimalLatitude,
    decimalLongitude = adjDecimalLongitude,
    coordinateUncertaintyInMeters = adjCoordinateUncertainty) 

  brd_perpoint_mod <- brd_perpoint_mod %>%
    mutate(minimumElevationInMeters = adjElevation,
         maximumElevationInMeters = adjElevation,
         countryCode = "US",
         habitat = paste("NLCD:", nlcdClass, sep = " "),
         samplingProtocol = paste0("https://data.neonscience.org/api/v0/documents/",
                                   samplingProtocolVersion),
         samplingEffort = "6 minutes",
         sampleSizeValue = 49087.38, # sampled area actually varies based on visibility and acoustics, but this value is the area of 125 m radius circle around observer at a single sampling pointID
         sampleSizeUnit = "square metre",
         truncDate = lubridate::date(eventDate),
         datasetID = "https://doi.org/10.48443/00pg-vm19"
         )

  
  brd_perpoint_mod$eventID <- ifelse(brd_perpoint_mod$uid == "0c2a3910-fbd4-4343-a419-193a3329a8ca", "UNDE_009.B2.2021-06-10", brd_perpoint_mod$eventID) # change pointID from C3 to B2 to resolve duplicate and allow merge
  brd_perpoint_mod$pointID <- ifelse(brd_perpoint_mod$uid == "0c2a3910-fbd4-4343-a419-193a3329a8ca", "B2", brd_perpoint_mod$pointID) # change pointID from C3 to B2 to resolve duplicate and allow merge

  data_bird <- dplyr::left_join(
    brd_countdata,
    dplyr::select(brd_perpoint_mod, 
                  -uid))

  
  # table(data_bird$samplingImpractical) # all NA
  # table(data_bird$samplingImpracticalRemarks)
  
  data_bird <- dplyr::select(
    data_bird, 
    -identifiedBy, 
    # -eventID, # it is just plotID, pointID, startDate
    -measuredBy,
    -samplingImpractical, -samplingImpracticalRemarks)
  
  
  #location ----
  
  table_location_raw <- data_bird %>%
    dplyr::select(domainID, siteID, plotID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  habitat, plotType, geodeticDatum, samplingProtocol,samplingEffort, countryCode, datasetID, eventRemarks) %>%
    dplyr::distinct() 
#  table_location_raw$coordinateUncertaintyInMeters <- as.character(table_location_raw$coordinateUncertaintyInMeters) # ecocomDP ancillary location table fields can't be numeric
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "habitat", 
                            "plotType","geodeticDatum","samplingProtocol","samplingEffort", "countryCode", "datasetID","eventRemarks") )
    #,"coordinateUncertaintyInMeters"))
  
  
  
  
  # taxon ----
  # my_dots <- list(...) # gives "Error: '...' used in an incorrect context"
  # 
  # if("token" %in% names(my_dots)){
  #   my_token <- my_dots$token
  # }else{
  # my_token <- NA
  # }
  
  my_token <- Sys.getenv('NEON_PAT')
  
  # get bird taxon table from NEON
  neon_bird_taxon_table <- neonOS::getTaxonList(
    taxonType = "BIRD", 
    token = my_token) %>%
    dplyr::filter(taxonID %in% data_bird$taxonID)
  
  table_taxon <- neon_bird_taxon_table %>%
    dplyr::select(taxonID, taxonRank, scientificName, vernacularName, family, kingdom, nameAccordingToID) %>%
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