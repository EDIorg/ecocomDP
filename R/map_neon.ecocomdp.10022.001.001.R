##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for BEETLE from neon.data.product.id DP1.10022.001 from the NEON data portal and map to the ecocomDP 
#
##############################################################################################


map_neon.ecocomdp.10022.001.001 <- function(
    neon.data.list,
    neon.data.product.id = "DP1.10022.001",
    ...){
  #NEON target taxon group is BEETLES
  neon_method_id <- "neon.ecocomdp.10022.001.001"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  # get data
  beetles_raw <- neon.data.list
  
  
  # helper function to calculate mode of a column/vector
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  # start with the fielddata table, which describes all sampling events
  data_beetles <- tidyr::as_tibble(beetles_raw$bet_fielddata) %>%
    dplyr::filter(sampleCollected == "Y") %>% #there's an entry for every trap, whether or not they got samples, only want ones with samples
    dplyr::select(sampleID, domainID, siteID, plotID, namedLocation,
                  trapID, setDate, collectDate, eventID, trappingDays,
                  release, publicationDate, 
                  plotType, samplingProtocolVersion, remarks) %>%
    # eventID's are inconsistently separated by periods, so we remove them
    dplyr::mutate(eventID = stringr::str_remove_all(eventID, "[.]")) %>%
    #calculate a new trapdays column
    dplyr::mutate(trappingDays = lubridate::interval(lubridate::ymd(setDate),
                                                     lubridate::ymd(collectDate)) %/%
                    lubridate::days(1))
  
  #Find the traps that have multiple collectDates/bouts for the same setDate
  #need to calculate their trap days from the previous collectDate, not the setDate
  
  data_adjTrappingDays <- data.frame()
  
  try({
    data_adjTrappingDays <- data_beetles %>%
      dplyr::select(namedLocation, trapID, setDate, collectDate, trappingDays, eventID,
                    release, publicationDate, 
                    plotType, samplingProtocolVersion, remarks) %>%
      dplyr::group_by(
        dplyr::across(
          -c("collectDate", "trappingDays", "eventID"))) %>%
      dplyr::filter(
        dplyr::n_distinct(collectDate) > 1) 
    
    if(nrow(data_adjTrappingDays) > 0){
      data_adjTrappingDays <- data_adjTrappingDays %>%
        dplyr::group_by(namedLocation, trapID, setDate) %>%
        dplyr::mutate(diffTrappingDays = trappingDays - min(trappingDays)) %>%
        dplyr::mutate(adjTrappingDays = dplyr::case_when(
          diffTrappingDays == 0 ~ trappingDays,
          TRUE ~ diffTrappingDays)) %>%
        dplyr::select(-c(trappingDays, diffTrappingDays))
    }
    
  }, silent = TRUE)
  
  if(nrow(data_adjTrappingDays)>0){
    data_beetles <- data_beetles %>%
      #update with adjusted trapping days where needed
      dplyr::left_join(data_adjTrappingDays)
  }else{
    data_beetles$adjTrappingDays <- NA_real_
  }
  
  data_beetles <- data_beetles %>%
    #update with adjusted trapping days where needed
    # dplyr::left_join(adjTrappingDays) %>%
    dplyr::mutate(
      trappingDays = dplyr::case_when(
        !is.na(adjTrappingDays) ~ adjTrappingDays,
        TRUE ~ trappingDays
      )) %>%
    dplyr::select(-adjTrappingDays, -setDate) %>%
    # for some eventID's (bouts) collection happened over two days,
    # change collectDate to the date that majority of traps were collected on
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup() %>%
    # there are also some sites for which all traps were set and collect on the same date, but have multiple eventID's
    # we want to consider that as all one bout so we create a new ID based on the site and collectDate
    tidyr::unite(boutID, siteID, collectDate, remove = FALSE) %>%
    dplyr::select(-eventID) %>%
    # join with bet_sorting, which describes the beetles in each sample
    dplyr::left_join(beetles_raw$bet_sorting %>%
                       # only want carabid samples, not bycatch
                       dplyr::filter(sampleType %in% c("carabid", "other carabid")) %>%
                       dplyr::select(uid,
                                     sampleID, subsampleID, sampleType, taxonID,
                                     scientificName, taxonRank, identificationReferences,
                                     nativeStatusCode,
                                     individualCount),
                     by = "sampleID",
                     multiple = "all") %>%
    dplyr::filter(!is.na(subsampleID)) #even though they were marked a sampled, some collection times don't acutally have any samples
  
  ### Clean up Taxonomy of Samples ###
  
  #Some samples were pinned and reidentified by more expert taxonomists, replace taxonomy with their ID's (in bet_parataxonomist) where available
  data_pin <- data_beetles %>%
    dplyr::left_join(beetles_raw$bet_parataxonomistID %>%
                       dplyr::select(subsampleID, individualID, taxonID, scientificName,
                                     taxonRank,
                                     nativeStatusCode),
                     by = "subsampleID",
                     multiple = "all") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(nativeStatusCode = ifelse(is.na(nativeStatusCode.y), 
                                            nativeStatusCode.x, nativeStatusCode.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), "sort", "pin")) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  # some subsamples weren't fully ID'd by the pinners, so we have to recover the unpinned-individuals
  lost_indv <- data_pin %>%
    dplyr::filter(!is.na(individualID)) %>%
    dplyr::group_by(subsampleID, individualCount) %>%
    dplyr::summarise(n_ided = dplyr::n_distinct(individualID)) %>%
    dplyr::filter(n_ided < individualCount) %>%
    dplyr::mutate(unidentifiedCount = individualCount - n_ided) %>%
    dplyr::select(subsampleID, individualCount = unidentifiedCount) %>%
    dplyr::left_join(
      dplyr::select(data_beetles, -individualCount), 
      by = "subsampleID") %>%
    dplyr::mutate(identificationSource = "sort")
  
  # add unpinned-individuals back to the pinned id's, adjust the individual counts so pinned individuals have a count of 1
  data_pin <- data_pin %>%
    dplyr::mutate(individualCount = ifelse(identificationSource == "sort", individualCount, 1)) %>%
    dplyr::bind_rows(lost_indv)
  
  
  #Join expert ID's to beetle dataframe
  data_expert <- dplyr::left_join(
    data_pin,
    dplyr::select(beetles_raw$bet_expertTaxonomistIDProcessed,
                  individualID,taxonID,scientificName,
                  taxonRank,
                  nativeStatusCode),
    by = 'individualID', na_matches = "never") %>%
    dplyr::distinct()
  
  
  
  #Update with expert taxonomy where available
  data_expert <- data_expert %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(nativeStatusCode = ifelse(is.na(nativeStatusCode.y), 
                                            nativeStatusCode.x, nativeStatusCode.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), identificationSource, "expert")) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y")) %>%
    dplyr::group_by(individualID) %>%
    dplyr::filter(dplyr::n() == 1 | is.na(individualID)) %>% #remove individuals with more than one ID, retain NA individualID's
    dplyr::ungroup()
  
  
  #Get raw counts table
  beetles_counts <- data_expert %>%
    dplyr::select(-c(subsampleID, sampleType, identificationSource, individualID)) %>%
    dplyr::group_by_at(dplyr::vars(-individualCount)) %>%
    dplyr::summarise(count = sum(individualCount)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  
  
  
  
  # making tables ----
  # Observation Tables
  # All individuals of the same species collected at the same time/same location are considered the same observation, regardless of how they were ID'd
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  # aggregate dup counts
  beetles_counts_agg <- beetles_counts %>% 
    dplyr::group_by(sampleID, taxonID) %>%
    dplyr::summarize(
      count = sum(count))
  
  beetles_counts_agg <- beetles_counts_agg %>% 
    dplyr::left_join(
      beetles_counts %>% 
        dplyr::select(-count) %>%
        dplyr::distinct(),
      multiple = "first") %>%
    dplyr::ungroup()
  
  # dups <- beetles_counts_agg %>% filter(n_counts > 1)
  
  table_observation_raw <- beetles_counts_agg %>%
    dplyr::distinct() %>%
    dplyr::rename(
      location_id = namedLocation,
      abundance = count,
      datetime = collectDate,
      taxon_id = taxonID) %>%
    
    # remove invalde records
    dplyr::filter(
      is.finite(abundance),
      abundance >= 0,
      !is.na(abundance)) %>%
  
    # creat required ecocomDP fields
    dplyr::mutate(
      package_id = my_package_id, 
      observation_id = paste0("obs_",1:nrow(.)),
      # event_id = observation_id,
      variable_name = "abundance",
      value = abundance/trappingDays,
      unit = "count per trap day") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      event_id = sampleID) %>%
    dplyr::ungroup()
  

  
  # # check for dups
  # dup_check <- table_observation_raw %>% group_by(sampleID, taxon_id) %>%
  #   summarize(
  #     n_counts = n(),
  #     count_vals = paste(abundance, collapse = "|")
  #   )
  # 
  # dup_check %>% filter(n_counts > 1)
  
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
  
  
  
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_raw,
    ancillary_var_names = c(
      "observation_id",
      # "neon_event_id",
      "sampleID",
      "boutID",
      "trapID",
      "trappingDays",
      "release", "publicationDate",
      "samplingProtocolVersion", 
      "remarks",
      "nativeStatusCode")) %>%
    # add units where appropriate
    dplyr::mutate(
      unit = dplyr::case_when(
        variable_name == "trappingDays" ~ "days",
        TRUE ~ NA_character_
      )
    )
  
  
  
  
  # location ----
  # get relevant location info from the data, use neon helper functions
  # to make location and ancillary location tables
  
  table_location_raw <- beetles_raw$bet_fielddata %>%
    dplyr::select(domainID, siteID, plotID, namedLocation,
                  decimalLatitude, decimalLongitude, elevation,
                  plotType, nlcdClass, geodeticDatum) %>%
    dplyr::distinct() %>%
    dplyr::filter(namedLocation %in% table_observation$location_id)
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names <- c("namedLocation",
                             "plotType", "nlcdClass", "geodeticDatum"))
  
  
  
  # taxonomy ----
  my_dots <- list(...)
  
  if("token" %in% names(my_dots)){
    my_token <- my_dots$token
  }else{
    my_token <- NA
  }
  
  # get beetle taxon table from NEON
  neon_bet_taxon_table <- neonOS::getTaxonList(
    taxonType = "BEETLE", 
    token = my_token) %>%
    dplyr::filter(taxonID %in% table_observation$taxon_id)
  
  table_taxon <- neon_bet_taxon_table %>%
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
  
  
  
  
  # data summary ----
  # make dataset_summary -- required table
  
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
  
  
  
  # return tables ----
  
  out_list <- list(
    location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  return(out_list)
} # end of function