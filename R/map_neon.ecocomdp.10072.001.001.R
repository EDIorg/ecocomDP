##############################################################################################
##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon.ecocomdp.10072.001.001(
#' site = c("NIWO","DSNY"), 
#' startdate = "2016-01",
#' enddate = "2017-11",
#' token = Sys.getenv("NEON_TOKEN"),
#'   check.size = FALSE)
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for SMALL_MAMMAL taxa from neon.data.product.id DP1.10072.001 from the NEON data portal and map to the ecocomDP format
#' @export

##############################################################################################
# mapping function for SMALL_MAMMAL taxa
map_neon.ecocomdp.10072.001.001 <- function(
  neon.data.product.id = "DP1.10072.001",
  ...){
  
  # Authors: Marta Jarzyna, Eric Sokol
  
  ### This code downloads, unzips and restructures small mammal data (raw abundances) from NEON
  ### for each NEON site, we provide mammal raw abundance aggregated per day (across all years),
  ### per month (bout) (across all years), and per year
  ### By Marta Jarzyna
  
  #NEON target taxon group is SMALL_MAMMALS
  neon_method_id <- "neon.ecocomdp.10072.001.001"
  
  # check arguments passed via dots for neonUtilities
  dots_updated <- list(..., dpID = neon.data.product.id)
  
  
  ### Get the Data
  d <- rlang::exec( 
    neonUtilities::loadByProduct,
    !!!dots_updated)
  
  
  
  
  dat.mam = dplyr::mutate(d$mam_pertrapnight,
                          collectDate = lubridate::ymd(collectDate),
                          year = lubridate::year(collectDate),
                          month = lubridate::month(collectDate),
                          day = lubridate::day(collectDate),
                          # Since data collection usually takes place on several consecutive
                          # days within a given month, we consider each month to be a separate bout
                          bout = paste(year, month, sep = "_")) %>%
    tibble::as_tibble()
  table(dat.mam$plotType) # all distributed
  
  ### Here we provide the code that summarizes raw abundances per day, month (bout), and year
  ### We keep scientificName of NA or blank (no captures) at this point
  # dat.mam <- filter(dat.mam, scientificName != "") #, !is.na(scientificName))
  dat.mam <- dplyr::mutate(dat.mam, scientificName = ifelse(scientificName == "", NA, scientificName))
  
  ### Remove species that are bycatch (non-target), dead, or escapted while processing
  ### remove all where the fate of the individual, unless marked and released, is
  ### 'dead' = dead, 'escaped' = escaped while handling, 'nontarget' = released, non-target species,
  ### should 'released' (= target or opportunistic species released without full processing) be also removed?
  ## D Li: probably not, released have 64,400 records and processed only have 7,365
  table(dat.mam$fate)
  dat.mam <- dplyr::filter(dat.mam, !fate %in% c("dead", "escaped", "nontarget"))
  #dat.mam <- filter(dat.mam, fate != "released")
  
  # unique(dat.mam$scientificName)
  group_by(dat.mam, taxonRank) %>% tally() # mostly are sp and genus level
  dat.mam <- dplyr::filter(dat.mam, taxonRank %in% c("genus", "species", "subspecies", NA))
  
  ### Remove recaptures -- Y and U (unknown); only retain N
  # table(dat.mam$recapture)
  # sum(is.na(dat.mam$recapture))
  dat.mam <- dplyr::filter(dat.mam, recapture == "N" | is.na(recapture))
  
  ### Get raw abundances per day
  data_small_mammal <- dat.mam %>%
    dplyr::select(uid, nightuid,
                  domainID, siteID, plotID, namedLocation, 
                  collectDate,
                  nlcdClass, decimalLatitude,
                  decimalLongitude, geodeticDatum, coordinateUncertainty,
                  elevation, elevationUncertainty, 
                  trapCoordinate, trapStatus, 
                  year, month,
                  day, taxonID, scientificName, taxonRank, identificationReferences,
                  nativeStatusCode, sex, lifeStage,
                  pregnancyStatus, tailLength, totalLength, weight) %>%
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::distinct()
  
  

  #location ----
  table_location_raw <- data_small_mammal %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- ecocomDP::make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- ecocomDP::make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", "geodeticDatum"))
  
  
  # taxon ----
  
  if("token" %in% names(dots_updated)){
    my_token <- dots_updated$token
  }else{
    my_token <- NA
  }
  
  # get SMALL_MAMMAL taxon table from NEON
  neon_taxon_table <- neonUtilities::getTaxonTable(
    taxonType = "SMALL_MAMMAL", token = my_token) %>%
    dplyr::filter(taxonID %in% unique(na.omit(data_small_mammal$taxonID)))
  
  table_taxon <- neon_taxon_table %>%
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
  table_observation_all <- data_small_mammal %>%
    dplyr::rename(location_id = namedLocation) %>%
    dplyr::mutate(
      package_id = paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    dplyr:: rename(
      observation_datetime = collectDate, 
      taxon_id = taxonID,
      event_id = nightuid) %>%
    dplyr::mutate(
      occurrence = 1)
  
  table_observation_aggregated <- table_observation_all %>%
    dplyr::select(
      event_id,
      package_id,
      location_id,
      observation_datetime,
      taxon_id, 
      occurrence) %>%
    dplyr::group_by_at(dplyr::vars(-occurrence)) %>%
    dplyr::summarise(
      value = sum(occurrence)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:length(value)),
      variable_name = "count",
      unit = "count per trapping grid per night"
    )
    
  table_observation <- table_observation_aggregated %>%
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
    dplyr::filter(!is.na(taxon_id))
  
  table_observation_ancillary <- ecocomDP::make_neon_ancillary_observation_table(
    obs_wide = table_observation_all,
    ancillary_var_names = c(
      "event_id",
      "plotID",
      "boutNumber",
      "year","month")) %>% 
    dplyr::distinct()
  
  # data summary ----
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
  