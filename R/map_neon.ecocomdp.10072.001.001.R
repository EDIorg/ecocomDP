##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for SMALL_MAMMAL taxa from neon.data.product.id DP1.10072.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for SMALL_MAMMAL taxa
map_neon.ecocomdp.10072.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10072.001",
  ...){
  
  
  ### This code restructures small mammal data (raw abundances) from NEON
  ### which has been downloaded using neonUtilities::loadByProduct() 
  ### for each NEON site, we provide mammal raw abundance aggregated per day (across all years),
  ### per month (bout) (across all years), and per year
  
  #NEON target taxon group is SMALL_MAMMALS ----
  neon_method_id <- "neon.ecocomdp.10072.001.001"
  
  
  # make sure neon.data.list matches the method ----
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  ### Get the Data ----
  d <- neon.data.list
  
  
  # Join perplotnight data with pertrapnight data ----
  mamjn <- dplyr::inner_join(
    d$mam_perplotnight, d$mam_pertrapnight,
    by = c("nightuid","domainID","siteID","plotID","namedLocation",
           "collectDate","endDate"), 
    multiple = "all",
    suffix = c("_perplotnight","_pertrapnight"))
  
  
  
  # Cleam up joined data ----
  # pull out year, month, and day
  # create bout IDs
  dat.mam <- mamjn %>%
    dplyr::mutate(
      year = lubridate::year(collectDate),
      month = lubridate::month(collectDate),
      day = lubridate::day(collectDate),
      #bouts are designated by eventID in perplotnight table.  If that entry is missing
      #bout is estimated as the year_month
      bout = ifelse(is.na(eventID), paste(year, month, sep = "_"), eventID)) %>%
    tidyr::as_tibble() %>%
    dplyr::group_by(nightuid) %>%
    dplyr::mutate(
      n_trap_nights_per_night_uid = length(unique(trapCoordinate))) %>%
    dplyr::ungroup() %>%
    ### Aummarize raw abundances per day, month (bout), and year
    ### We keep scientificName of NA or blank (no captures) at this point
    dplyr::mutate(
      scientificName = ifelse(scientificName == "", NA, scientificName))
  
  
  # Get taxon table ----
  # Download the taxon table using neonOS
  mam.tax <- neonOS::getTaxonList(taxonType="SMALL_MAMMAL", 
                                  recordReturnLimit=1000, verbose=T)
  #G et a list of target taxa:
  targetTaxa <- mam.tax %>% 
    filter(taxonProtocolCategory == "target") %>% 
    select(taxonID, scientificName)
  
  
  # Filter out unwanted records ----
  ### Remove species that are bycatch (non-target), dead, or escapted while processing
  ### remove all where the fate of the individual, unless marked and released, is
  ### 'dead' = dead, 'escaped' = escaped while handling, 'nontarget' = released, non-target species,
  
  dat.mam <- dat.mam %>% 
    dplyr::filter(
      trapStatus %in% c("5 - capture","4 - more than 1 capture in one trap")) %>%
    #SP NOTE/QUESTION FOR ERIC: I added speciesGroup to capture the species complexes that are
    #impossible to distinguish in some regions (e.g., PELEPEMA)
    #SP The first filtering step gets rid of unprocessed individuals since they don't have taxonIDs at a useful level
    dplyr::filter(
      taxonRank %in% c("genus", "species", "subspecies", "speciesGroup",NA),
      taxonID %in% targetTaxa$taxonID) %>%
    # filter out recaptures within a bout, but not all recaptures
    dplyr::group_by(bout) %>%
    dplyr::filter(!duplicated(tagID)) %>%
    dplyr::ungroup()
  
  ### Select and clean up columns to carry forward
  data_small_mammal <- dat.mam %>%
    dplyr::select(
      uid_pertrapnight,
      nightuid, bout,
      n_trap_nights_per_night_uid,
      domainID, siteID, plotID, namedLocation, plotType,
      collectDate,
      nlcdClass, decimalLatitude,
      decimalLongitude, geodeticDatum, coordinateUncertainty,
      elevation, elevationUncertainty, 
      trapCoordinate, trapStatus, 
      year, month,
      day, taxonID, scientificName, taxonRank, identificationReferences,
      nativeStatusCode, sex, lifeStage,
      pregnancyStatus, tailLength, totalLength, weight,
      release_pertrapnight, publicationDate_pertrapnight) %>%
    dplyr::rename(uid = uid_pertrapnight, 
                  release = release_pertrapnight, 
                  publicationDate = publicationDate_pertrapnight) %>%
    dplyr::filter(!is.na(taxonID),
                  !is.na(n_trap_nights_per_night_uid),
                  is.finite(n_trap_nights_per_night_uid),
                  n_trap_nights_per_night_uid > 0 
                  ) %>%
    dplyr::distinct()
  
  
  
  #location ----
  table_location_raw <- data_small_mammal %>%
    dplyr::select(domainID, siteID, plotID, plotType, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "plotType", "nlcdClass", "geodeticDatum"))
  
  
  # taxon ----
  my_dots <- list(...)
  
  if("token" %in% names(my_dots)){
    my_token <- my_dots$token
  }else{
    my_token <- NA
  }
  
  table_taxon <- mam.tax %>%
    dplyr::select(taxonID, taxonRank, scientificName, nameAccordingToID) %>%
    dplyr::distinct() %>% 
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = nameAccordingToID) %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) 
  
  
  
  # observation ----
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  table_observation_all <- data_small_mammal %>%
    dplyr::rename(location_id = namedLocation) %>%
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr::rowwise() %>%
    dplyr:: rename(
      datetime = collectDate, 
      taxon_id = taxonID) %>%
    dplyr::mutate(
      event_id = paste(location_id, year, month, sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      occurrence = 1)
  
  table_event <- table_observation_all %>%
    dplyr::select(package_id, 
                  plotID, 
                  plotType,
                  location_id,
                  event_id, 
                  nightuid, n_trap_nights_per_night_uid,
                  datetime, 
                  year, month) %>%
    dplyr::distinct() %>%
    dplyr::group_by(
      package_id, event_id, location_id, plotID, year, month) %>%
    dplyr::summarize(
      datetime = dplyr::first(datetime),
      n_trap_nights_per_bout_per_plot = sum(n_trap_nights_per_night_uid),
      n_nights_per_bout = length(unique(nightuid))) %>%
    dplyr::ungroup()
  
  # calculate counts
  table_raw_counts <- table_observation_all %>%
    dplyr::select(
      event_id,
      taxon_id,
      nativeStatusCode, 
      release, publicationDate,
      occurrence) %>%
    dplyr::group_by_at(dplyr::vars(
      event_id, taxon_id)) %>%
    dplyr::summarize(
      raw_count = sum(occurrence),
      nativeStatusCode = paste(unique(nativeStatusCode), collapse = "|"),
      release = paste(unique(release), collapse = "|"), 
      publicationDate= paste(unique(publicationDate), collapse = "|")) 
  
  # standardize counts to sampling effort
  table_event_counts <- table_event %>%
    dplyr::left_join(
      table_raw_counts, 
      by = "event_id",
      multiple = "all") %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)),
      value = 100 * raw_count / n_trap_nights_per_bout_per_plot,
      variable_name = "count",
      unit = "unique individuals per 100 trap nights per plot per month")
  
  # format and select columns for observation table
  table_observation <- table_event_counts %>%
    dplyr::select(
      observation_id,
      event_id,
      location_id,
      package_id,
      location_id,
      datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(taxon_id))
  
  # put remaining columns in the observation_ancillary table
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_event_counts,
    ancillary_var_names = c(
      "observation_id",
      "year",
      "month",
      "n_trap_nights_per_bout_per_plot",
      "n_nights_per_bout",
      "nativeStatusCode",
      "release", 
      "publicationDate")) %>% 
    dplyr::distinct()
  
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
  