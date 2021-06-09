##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve positivity data for TICK_PATHOGEN taxa from neon.data.product.id DP1.10092.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for TICK_PATHOGEN taxa
map_neon.ecocomdp.10092.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10092.001",
  ...){
  
  # Download and clean tick pathogen data
  
  #NEON target taxon group is SMALL_MAMMALS
  neon_method_id <- "neon.ecocomdp.10092.001.001"
  
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  
  ### Get the Data
  tickpathdat <- neon.data.list
  
  

  
  # names(tickpathdat)
  tick_path <- tidyr::as_tibble(tickpathdat$tck_pathogen)
  # names(tick_path)
  # table(tick_path$laboratoryName)
  tick_pathqa <- tidyr::as_tibble(tickpathdat$tck_pathogenqa)
  
  #### Raw dataset inspection ###
  ## Record original number of tests
  # nrow(tick_path)
  
  ## Record original number of test results that are not NA
  # nrow(filter(tick_path, !is.na(testResult)))
  
  #### Quality check file ###
  ## Remove any samples that have flagged quality checks
  # table(tick_pathqa$criteriaMet)
  remove_qa_uid_batchID <- tick_pathqa[tick_pathqa$criteriaMet != "Y", c("uid","batchID")]
  
  # ## Record which batchIDs in tick_path are not in quality file
  # print(paste0("## Batches NOT found in quality file: "))
  # setdiff(tick_path$batchID, tick_pathqa$batchID)
  # setdiff(tick_pathqa$batchID, tick_path$batchID)
  # Note: in the qa file it says NEON_2017831, but in the tick_path file it's NEON_20170831. I think these are the same thing.
  
  #### Tick Pathogen Quality assessment ###
  ## Checking uid is unique
  Is_all_ID_uniqud <- any(duplicated(tick_path$uid))
  if (Is_all_ID_uniqud)
    warning("uid is not unique. Check file manually.")
  
  ## Check there isn't any important missing data
  # List of important variables
  checkVar <- c("domainID","siteID","plotID", "decimalLatitude", "decimalLongitude",
                "plotType", "nlcdClass", "elevation", "collectDate",
                "subsampleID", "batchID", "testingID", "testPathogenName")
  # Check which variables are missing
  which_missing_var <- sapply(checkVar, function(v) {any(is.na(tick_path[, v]))})
  
  
  # Identify samples with missing values
  if(any(which_missing_var)){
    removeSamples_missingvalue <- tick_path[
      as.vector(is.na(tick_path[, checkVar[which_missing_var]])),
      c("uid","namedLocation","collectDate","sampleCondition","remarks")]
    # # Will need to filter these out.
    # removeSamples_missingvalue
  }else{
    removeSamples_missingvalue <- data.frame()
  }

  
  ## Look at sample conditions; filter out all non-OKAY
  # table(tick_path$sampleCondition) # We'll get rid of all "non-okay" samples
  removeSamples_sampleCondition <- tick_path[
    tick_path$sampleCondition != "OK",
    c("uid","namedLocation","collectDate","sampleCondition","remarks")]
  # removeSamples_sampleCondition
  
  ## How many test results are NA?
  # sum(is.na(tick_path$testResult)) # a lot. I think some are 2019 data, and others are just unknown/untested
  removeSamples_testResult <- tick_path[is.na(tick_path$testResult),]
  
  # ## Check DNA quality of each testing batch
  # tick_path %>% dplyr::filter(testPathogenName == "HardTick DNA Quality") %>%
  #   dplyr::select(testResult, testPathogenName) %>%
  #   table(useNA = "ifany")
  
  # Only positive DNA test results-- filter out others
  removeSamples_DNAQual <- tick_path %>%
    dplyr::filter(testPathogenName == "HardTick DNA Quality", testResult != "Positive") %>%
    dplyr::select(uid, namedLocation, collectDate, sampleCondition, remarks)
  # removeSamples_DNAQual
  
  # ## Check number of ticks tested per site per year
  # ## Site x year for all tested ticks:
  # tick_path %>% 
  #   filter(!is.na(testResult),
  #          !(testPathogenName %in% c("HardTick DNA Quality","Ixodes pacificus"))) %>%
  #   mutate(year = year(collectDate)) %>%
  #   pivot_wider(
  #     id_cols=c(siteID, year, testingID), 
  #     names_from=testPathogenName, 
  #     values_from=testResult)%>%
  #   select(siteID, year) %>% table()
  # It is NOT true that only a subset of 130 ticks were tested from each site and year... do they mean PLOT?
  # or maybe they mean "by species"?
  # By species-- try the two main ones
  ## Site x year for Ambame only:
  # tick_path %>%
  #   filter(!is.na(testResult), !(testPathogenName %in% c("HardTick DNA Quality","Ixodes pacificus"))) %>%
  #   separate(subsampleID, into=c("plotID","date","species","lifestage"), sep="[.]", remove=FALSE) %>%
  #   mutate(year = year(collectDate)) %>%
  #   pivot_wider(id_cols=c(siteID, year, species, testingID), names_from=testPathogenName, values_from=testResult)%>%
  #   filter(species=="AMBAME")  %>% select(siteID, year) %>% table()
  
  ## Site x year for Ixosca only:
  # tick_path %>%
  #   filter(!is.na(testResult), !(testPathogenName %in% c("HardTick DNA Quality","Ixodes pacificus"))) %>%
  #   separate(subsampleID, into=c("plotID","date","species","lifestage"), sep="[.]", remove=FALSE) %>%
  #   mutate(year = year(collectDate)) %>%
  #   pivot_wider(id_cols=c(siteID, year, species, testingID), names_from=testPathogenName, values_from=testResult)%>%
  #   filter(species=="IXOSCA")  %>% select(siteID, year) %>% table()
  
  # By plot
  ## Plot x year:
  # tick_path %>%
  #   filter(!is.na(testResult), !(testPathogenName %in% c("HardTick DNA Quality","Ixodes pacificus"))) %>%
  #   separate(subsampleID, into=c("plotID","date","species","lifestage"), sep="[.]", remove=FALSE) %>%
  #   mutate(year = year(collectDate)) %>%
  #   pivot_wider(id_cols=c(plotID, year, species, testingID), names_from=testPathogenName, values_from=testResult)%>%
  #   select(plotID, year) %>% table()
  
  ### Which species were tested for what pathogens?
  # Create table
  ## Which species was tested for which pathogens:
  # tick_path %>%
  #   filter(!is.na(testResult)) %>%
  #   separate(subsampleID, into=c("plotID","date","species","lifestage"), sep="[.]", remove=FALSE) %>%
  #   select(testPathogenName, species) %>% table()
  
  ## Get rid of hardtick DNA and Ixodes pacificus tests
  remove_testPathogenName <- tick_path %>%
    dplyr::filter(testPathogenName %in% c("HardTick DNA Quality","Ixodes pacificus"))
  
  # Note that this does NOT follow their description from online, which says certain
  # species were tested for certain pathogens. In reality, it seems almost all
  # individuals were tested for all pathogens (with a few exceptions)
  
  #### Minor edits to data file ###
  
  ## First, we need to merge B. burgdeferi and B. burgdeferi sensu lato-- these are supposed to be one row.
  # They should be exclusive from each other, so let's check that first.
  
  # tick_path %>% filter(testPathogenName %in% c("Borrelia burgdorferi", "Borrelia burgdorferi sensu lato")) %>%
  #   select(testingID, testPathogenName) %>% nrow()
  # tick_path %>% filter(testPathogenName %in% c("Borrelia burgdorferi", "Borrelia burgdorferi sensu lato")) %>%
  #   select(testingID) %>% distinct() %>% nrow()
  # # Verified that testingIDs are not duplicated over B burgdorferi or B burgdorferi sensu lato. We can merge them without problem.
  
  #### Filtering ###
  data_tick_pathogen <- tick_path %>%
    dplyr::filter(!(uid %in% c(remove_qa_uid_batchID$uid, removeSamples_missingvalue$uid,
                        removeSamples_sampleCondition$uid, removeSamples_testResult$uid,
                        removeSamples_DNAQual$uid, remove_testPathogenName$uid))) %>%
    dplyr::mutate(testPathogenName = ifelse(testPathogenName == "Borrelia burgdorferi",
                                     "Borrelia burgdorferi sensu lato",
                                     testPathogenName)) %>%
    # Changing B burgdorferi to sensu lato
    tidyr::separate(subsampleID, into = c("plotID2","date2","vectorSpecies ","lifeStage"),
             sep = "[.]", remove=FALSE) %>% # Adding a host species column
    dplyr::select(-c("plotID2","date2")) %>% # remove extra columns
    as.data.frame()
  
  
  # observation ----
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  table_observation_all <- data_tick_pathogen %>%
    dplyr::select(-uid) %>%
    dplyr::mutate(
      package_id = my_package_id,
      event_id = paste0(namedLocation,"_",collectDate),
      # neon_event_id = event_id,
      pos_result_count = dplyr::case_when(
        testResult == "Positive" ~ 1,
        TRUE ~ 0),
      ) %>%
    dplyr::rowwise() %>%
    dplyr:: rename(
      location_id = namedLocation,
      datetime = collectDate, 
      taxon_id = testPathogenName)
      
  # get col names that only have one unique value per event_id
  
  d_tmp <- table_observation_all %>% 
    dplyr::group_by(event_id) %>% 
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::summarise_if(is.numeric, max) %>%
    as.data.frame()
    
  col_names_for_events <- names(d_tmp[,which(d_tmp==1)])
  
  event_data <- table_observation_all[
    ,c("event_id",
       col_names_for_events)] %>%
    dplyr::distinct()
  
  names_2_keep <- unique(
    c("event_id",
      "taxon_id",
      "pos_result_count",
      col_names_for_events))

  


  table_observation_aggregate <- table_observation_all[,names_2_keep] %>%
    dplyr::group_by(event_id, taxon_id) %>%
    dplyr::summarize(
      n_tests = length(pos_result_count),
      n_positive_tests = sum(pos_result_count)) %>%
    dplyr::mutate(
      value = n_positive_tests/n_tests) %>% 
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)),
      variable_name = "positivity rate",
      unit = "positive tests per pathogen per sampling event"
    ) %>%
    dplyr::left_join(event_data,.) %>%
    dplyr::distinct()

  

  
  table_observation <- table_observation_aggregate %>%
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
  


  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_aggregate,
    ancillary_var_names = c(
      "observation_id",
      # "neon_event_id",
      "n_tests",
      "n_positive_tests",
      "vectorSpecies ",
      "lifeStage",
      "batchID",
      "subsampleID",
      "testProtocolVersion",
      "remarks",
      "laboratoryName",
      "release",
      "publicationDate")) %>% 
    dplyr::distinct()
  

  
  
  # taxon ----
  table_taxon <- data_tick_pathogen %>%
    dplyr::select(testPathogenName, laboratoryName) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      taxon_id = testPathogenName,
      taxon_name = testPathogenName,
      taxon_rank = dplyr::case_when(
        grepl(" sp\\.| spp\\.",testPathogenName) ~ "genus",
        TRUE ~ "species"),
      authority_system =laboratoryName) %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) %>%
    dplyr::distinct() %>%
    # concatenate different references for same taxonID
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarise(
      authority_system = paste(unique(c(authority_system)), collapse = "; "))
  

  #location ----
  table_location_raw <- data_tick_pathogen %>%
    dplyr::select(domainID, siteID, plotID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  plotType,
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass",
                            "plotType","geodeticDatum"))
  
  
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