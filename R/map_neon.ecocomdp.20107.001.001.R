##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for FISH taxa from neon.data.product.id DP1.20107.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for FISH
map_neon.ecocomdp.20107.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.20107.001",
  ...){

  #NEON target taxon group is FISH
  neon_method_id <- "neon.ecocomdp.20107.001.001"
  
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  # get token from dots if available
  my_dots <- list(...)
  
  if("token" %in% names(my_dots)){
    my_token <- my_dots$token
  }else{
    my_token <- NA
  }
  
  
  # get fish taxon table ----
  # get taxon table from API, may take a few minutes to load
  # Fish electrofishing, gill netting, and fyke netting counts
  # http://data.neonscience.org/data-product-view?dpCode=DP1.20107.001
  full_taxon_fish <- neonOS::getTaxonList(
    taxonType = "FISH", token = my_token)
  
  # -- make ordered taxon_rank list for a reference (subspecies is smallest rank, kingdom is largest)
  # a much simple table with useful levels of taxonomic resolution;
  # this might not be needed if taxon rank is extracted  from scientific names using stringr functions
  taxon_rank_fish <- c(
    'superclass', 'class', 'subclass', 'infraclass', 'superorder',
    'order', 'suborder', 'infraorder', 'section', 'subsection',
    'superfamily', 'family', 'subfamily', 'tribe', 'subtribe', 'genus',
    'subgenus','speciesGroup','species','subspecies') %>% rev() # get the reversed version
  
  
  # get data FISH data from NEON api
  all_fish <- neon.data.list
  
  # The object returned by loadByProduct() is a named list of data frames.
  # To work with each of them, select them from the list using the $ operator.
  
  # this joins reach-level data, you can just join by any two of these, and the results are the same, dropping the unique ID
  # join field data table with the pass tables from all_fish
  fsh_dat1 <- dplyr::left_join(
    all_fish$fsh_perPass, 
    all_fish$fsh_fieldData, 
    by = c("reachID", "siteID"),
    suffix = c("_perPass","_filedData"),
    multiple = "all") %>%
    dplyr::filter(is.na(samplingImpractical) | samplingImpractical == "") %>% #remove records where fish couldn't be collected, both na's and blank data
    tidyr::as_tibble()
  
  # get rid of dupe col names and .x suffix
  fsh_dat1 <- fsh_dat1[, !grepl("_filedData", names(fsh_dat1))]
  names(fsh_dat1) <- gsub("_perPass", "", names(fsh_dat1))
  
  # add individual fish counts; perFish data = individual level per each specimen captured
  # this joins reach-level field data with individual fish measures
  fsh_dat_indiv <- dplyr::left_join(
    all_fish$fsh_perFish, 
    fsh_dat1, 
    by = "eventID", 
    multiple = "all") %>%
    tidyr::as_tibble()
  
  # get rid of dupe col names and .x suffix
  fsh_dat_indiv <- fsh_dat_indiv[, !grepl('\\.y', names(fsh_dat_indiv))]
  names(fsh_dat_indiv) <- gsub('\\.x', '', names(fsh_dat_indiv))
  
  # fill in missing reachID from event ID
  fsh_dat_indiv$reachID <- ifelse(is.na(fsh_dat_indiv$reachID),
                                  substr(fsh_dat_indiv$eventID, 1, 16),
                                  fsh_dat_indiv$reachID)
  
  
  # add bulk fish counts; bulk count data = The number of fish counted during bulk processing in each pass
  # this will join all the reach-level field data with the species data with bulk counts
  fsh_dat_bulk <- dplyr::left_join(
    all_fish$fsh_bulkCount, 
    fsh_dat1, by = "eventID") %>%
    tidyr::as_tibble()
  
  # get rid of dupe col names and .x suffix
  fsh_dat_bulk <- fsh_dat_bulk[, !grepl('\\.y', names(fsh_dat_bulk))]
  names(fsh_dat_bulk) <- gsub('\\.x', '', names(fsh_dat_bulk))
  
  #fill in missing reachID
  fsh_dat_bulk$reachID <- ifelse(is.na(fsh_dat_bulk$reachID),
                                 substr(fsh_dat_bulk$eventID, 1, 16),
                                 fsh_dat_bulk$reachID)
  
  # add taxonRank into fsh_dat from full_taxon_fish
  # join the above with bulkCount dataset
  fsh_dat_bulk <- dplyr::left_join(
    fsh_dat_bulk, 
    dplyr::select(
      full_taxon_fish, taxonID, scientificName, taxonRank),
    by = c("taxonID", "scientificName"))
  
  # combine indiv and bulk counts
  fsh_dat <- dplyr::bind_rows(fsh_dat_indiv, fsh_dat_bulk)
  
  # add count = 1 for indiv data
  # before row_bind, the indiv dataset did not have a col for count
  # after bind, the bulk count col has "NAs" need to add "1", since indiv col has individual fish per row
  fsh_dat$count <- ifelse(is.na(fsh_dat$bulkFishCount), 1, fsh_dat$bulkFishCount)
  
  # get all records that have rank upto subspecies, variable taxonomic resolution
  fsh_dat_fine <- dplyr::filter(
    fsh_dat, 
    taxonRank %in% c("class", "family", "genus", "order", "phylum", "species", "subgenus", "subspecies"))
  
  # select passes with no fish were caught
  no_fsh <- dplyr::filter(all_fish$fsh_perPass, targetTaxaPresent == "N")
  
  # all records from fieldData where sampling was done, where sampling was not impractical
  fsh_sampled <- dplyr::filter(all_fish$fsh_fieldData, is.na(samplingImpractical) | samplingImpractical == "")
  
  #join the no-fish dataset with the field dataset where sampling was done
  no_fsh2 <- dplyr::left_join(x = no_fsh, y = fsh_sampled, by = c("reachID", "siteID", "domainID", "namedLocation"))
  
  # get rid of dupe col names and .x suffix
  no_fsh2 <- no_fsh2[, !grepl('\\.y', names(no_fsh2))]
  names(no_fsh2) <- gsub('\\.x', '', names(no_fsh2))
  
  # fill in missing reachID from event ID
  no_fsh2$reachID <- ifelse(is.na(no_fsh2$reachID),
                            substr(no_fsh2$eventID, 1, 16),
                            no_fsh2$reachID)
  
  # add a count variables, which should be zero since these are the reaches 
  # with zero fish captures
  no_fsh2 <- no_fsh2 %>% 
    dplyr::mutate(
      count = rep_len(0, length.out = nrow(no_fsh2)),
      scientificName = NA, taxonID = NA) %>%
    dplyr::mutate(
      dplyr::across(c("scientificName", "taxonID"), ~as.character(.)))
  
  # join the no_fish sampling sessions to the sampling sessions with fish
  if(nrow(no_fsh2) > 0) fsh_dat_fine <- dplyr::bind_rows(fsh_dat_fine, no_fsh2)
  
  # need to convert POSIXct format into as.character and then back to date-time format
  # then, fill in missing site ID info and missing startDate into
  fsh_dat_fine$startDate <- dplyr::if_else(
    is.na(fsh_dat_fine$startDate),
    lubridate::as_datetime(substr(as.character(fsh_dat_fine$passStartTime), 1, 10)), 
    fsh_dat_fine$startDate) # 1-10: number of characters on date
  
  fsh_dat_fine$siteID <- dplyr::if_else(
    is.na(fsh_dat_fine$siteID),
    substr(fsh_dat_fine$eventID, 1, 4), 
    fsh_dat_fine$siteID) # four characters on site
  
  # grouping vars for aggregating density measurements
  my_grouping_vars <- c(
    'domainID','siteID','aquaticSiteType','namedLocation',
    'startDate', 'endDate', 'reachID','eventID','samplerType', 
    'aquaticSiteType', 
    'netSetTime', 'netEndTime',
    'netDeploymentTime', 'netLength', 'netDepth', 'fixedRandomReach',
    'measuredReachLength','efTime', 'efTime2',
    # "passStartTime", "passEndTime", 
    'netDeploymentTime', 
    'scientificName', 'taxonID', 'passNumber', 'taxonRank', 'targetTaxaPresent')
  
  my_grouping_vars <- dplyr::intersect(
    my_grouping_vars,
    names(fsh_dat_fine))
  # added a few metrics to quantify catch per unit effort such as 'passNumber', efish time, net deployment time
  
  
  
  # aggregate densities for each species group, pull out year and month from StartDate
  fsh_dat_aggregate <- fsh_dat_fine %>%
    dplyr::select(dplyr::all_of(c(my_grouping_vars, 'count'))) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(my_grouping_vars))) %>%
    dplyr::summarize(
      number_of_fish = sum(count),
      n_obs = dplyr::n()) %>%
    dplyr::mutate(
      year = startDate %>% lubridate::year(),
      month = startDate %>% lubridate::month()) %>% 
    dplyr::ungroup()
  
  
  #### there should not be dups at this point
  #### check for dups
  
  dup_counts <- fsh_dat_aggregate %>%
    group_by(taxonID, eventID) %>%
    summarize(
      n_agg_count_vals = n(),
      agg_count_vals = paste(number_of_fish, collapse = "|"),
      n_obs_vals = paste(n_obs, collapse = "|")
    )
  
  dups <- dup_counts %>%
    filter(n_agg_count_vals > 1)
  
  
  
  
  
  
  # some aquaticSiteType are NA, replace NAs if-based wildcarding namedLocation
  # grepl is for wildcarding
  fsh_dat_aggregate$aquaticSiteType <- dplyr::if_else(
    is.na(fsh_dat_aggregate$aquaticSiteType),
    dplyr::if_else(
      grepl("fish.point", fsh_dat_aggregate$namedLocation), 'stream','lake'),
    fsh_dat_aggregate$aquaticSiteType)
  
  # sampler type is also missing in a few cases, reaplace from eventID, with a wildcard
  fsh_dat_aggregate$samplerType <- dplyr::if_else(
    is.na(fsh_dat_aggregate$samplerType),
    dplyr::if_else(
      grepl("e-fisher", fsh_dat_aggregate$eventID), 
      'electrofisher',
      dplyr::if_else(grepl("gill", fsh_dat_aggregate$eventID), 
                     'gill net', 'mini-fyke net')),
    fsh_dat_aggregate$samplerType)
  
  
  
  
  
  # make sure that e-fish samples have "" or NA for netset and netend times, this will leave actual missing data as na
  # change netset/netend times to a datetime format if needed: lubridate::as_datetime(fsh_xx$netSetTime, format="%Y-%m-%d T %H:%M", tz="GMT")
  ## then calculate the netdeploymenttime (in hours) from netset and netend time
  # to calculate pass duration (in mins) and also calculate average efish time (secs); also if efishtime is zero, --> na's
  data_fish <-
    fsh_dat_aggregate %>% 
    dplyr::mutate(
      netSetTime = dplyr::if_else(
        condition = samplerType ==  "electrofisher" | samplerType == "two electrofishers",
        # true = lubridate::as_datetime(NA), false = netSetTime),
        true = NA_character_, false = as.character(netSetTime)),
      netEndTime = dplyr::if_else(
        condition = samplerType ==  "electrofisher" | samplerType == "two electrofishers",
        # true = lubridate::as_datetime(NA), false = netEndTime),
        true = NA_character_, false = as.character(netEndTime)),
      netDeploymentTime = dplyr::case_when(
        samplerType == grepl("electrofish", samplerType) ~ netDeploymentTime,
        is.na(netDeploymentTime) ~ as.numeric(difftime(
          netEndTime, netSetTime, tz = "GMT", units = "hours")),
        TRUE ~ netDeploymentTime),
      mean_efishtime = base::rowMeans(
        dplyr::select(., c("efTime", "efTime2")), na.rm = T),
      mean_efishtime = dplyr::case_when(
        mean_efishtime == 0 ~ NA_real_,
        TRUE ~ mean_efishtime))
  
  
  
  
  
  # with the above changes, we have efish time and and net duration to calculate 
  # catch per unit effort before moving to wide format
  # CPUE with efish time, calculated by = (total number of fish/average e-fish time 
  # in secs * 3600) as fish captured per 1-hr of e-fishing
  # CPUE with gill nets and fyke nets calculated by = (total number of fish/netDeployment time in hours * 24) 
  # as fish captured per 1-day-long net deployment of e-fishing
  data_fish <- data_fish %>% 
    dplyr::mutate(
      catch_per_effort = dplyr::if_else(
        condition = samplerType ==  "electrofisher" | samplerType == "two electrofishers",
        true = number_of_fish/mean_efishtime * 3600,
        false = dplyr::if_else(
          condition = samplerType ==  "mini-fyke net" | samplerType == "gill net",
          true = number_of_fish/netDeploymentTime * 24, 
          false = as.numeric(NA))))
  
  
  
  
  # is this necessary? fielddata was prevoiusly joined
  data_fish = dplyr::left_join(
    data_fish, 
    all_fish$fsh_fieldData %>% 
      dplyr::select(
        namedLocation, decimalLatitude, decimalLongitude, 
        geodeticDatum, elevation),
    by = "namedLocation",
    multiple = "all") %>%
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::distinct() %>%
    
    # remove invalid records
    dplyr::filter(
      is.finite(catch_per_effort),
      catch_per_effort >= 0,
      !is.na(catch_per_effort))
  
  
  # #### there should not be dups at this point
  # #### check for dups
  # 
  # dup_counts <- data_fish %>%
  #   group_by(taxonID, eventID) %>%
  #   summarize(
  #     n_cpue_vals = n(),
  #     cpue_vals = paste(catch_per_effort, collapse = "|"))
  # 
  # dups <- dup_counts %>%
  #   filter(n_cpue_vals > 1)
  # 
  # print(dups)
  
  
  
  
  #location ----
  table_location_raw <- data_fish %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  geodeticDatum, aquaticSiteType) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "aquaticSiteType", "geodeticDatum"))
  
  
  
  # observation ----
  
  my_package_id <- paste0(
    neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  table_observation_wide_all <- data_fish %>%
    dplyr::left_join(
      all_fish$fsh_perPass %>% 
        dplyr::select(
          eventID, 
          publicationDate, release) %>% 
        dplyr::distinct(),
      by = "eventID"
    ) %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr::rename(
      # neon_event_id = eventID,
      datetime = startDate, 
      taxon_id = taxonID,
      value = catch_per_effort) %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)), 
      event_id = eventID,
      # event_id = observation_id,
      # neon_event_id = eventID,
      variable_name = "abundance",
      unit = "catch per unit effort") 
  
  
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
      # "neon_event_id",
      "reachID",
      "samplerType",
      "habitatType",
      "subdominantHabitatType",
      "waterTemp",
      "dissolvedOxygen",
      "specificConductance",
      "netSetTime",        
      "netEndTime",
      "netDeploymentTime",
      "netLength",
      "netDepth",
      "fixedRandomReach",
      "measuredReachLength",
      "efTime",
      "efTime2",
      "passStartTime",
      "passEndTime", 
      "mean_efishtime",
      "remarks",
      "release",
      "publicationDate"))
  
  
  
  
  
  # taxon ----
  
  table_taxon <- full_taxon_fish %>%
    dplyr::select(taxonID, taxonRank, scientificName, nameAccordingToID) %>%
    dplyr::filter(taxonID %in% data_fish$taxonID) %>%
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