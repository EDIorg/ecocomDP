##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for MOSQUITO from neon.data.product.id DP1.10043.001 from the NEON data portal and map to the ecocomDP 

##############################################################################################
# mapping function for MOSQUITO
map_neon.ecocomdp.10043.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10043.001",
  ...){
  #NEON target taxon group is MOSQUITO
  neon_method_id <- "neon.ecocomdp.10043.001.001"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  # getting data ----  
  mos_allTabs <- neon.data.list
  
  
  # Define important data colums
  cols_oi_trap <- c('collectDate','eventID','namedLocation','sampleID')
  cols_oi_sort <- c('collectDate','sampleID','namedLocation','subsampleID')
  cols_oi_arch <- c('startCollectDate','archiveID','namedLocation')
  cols_oi_exp_proc <- c('collectDate','subsampleID','namedLocation')
  
  
  
  # Remove rows with missing important data, replacing blank with NA and factors with characters
  
  mos_trapping <- mos_allTabs$mos_trapping %>%
    dplyr::mutate(
      setDate = as.character(setDate),
      collectDate = as.character(collectDate)) %>%
    dplyr::mutate_all(list(~dplyr::na_if(.,""))) %>%
    tidyr::drop_na(dplyr::all_of(cols_oi_trap)) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::distinct()
  
  
  mos_sorting <- mos_allTabs$mos_sorting %>% 
    dplyr::mutate (
      setDate = as.character(setDate),
      collectDate = as.character(collectDate), 
      sortDate = as.character(sortDate)) %>% 
    dplyr::mutate_all(list(~dplyr::na_if(.,""))) %>% 
    tidyr::drop_na(dplyr::all_of(cols_oi_sort)) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::distinct()
  
  
  mos_archivepooling <- mos_allTabs$mos_archivepooling %>% 
    dplyr::mutate(
      startCollectDate = as.character(startCollectDate),
      endCollectDate = as.character(endCollectDate)) %>% 
    dplyr::mutate_all(list(~dplyr::na_if(.,""))) %>% 
    tidyr::drop_na(dplyr::all_of(cols_oi_arch)) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::distinct()
  
  
  mos_expertTaxonomistIDProcessed <- mos_allTabs$mos_expertTaxonomistIDProcessed %>% 
    dplyr::mutate(
      setDate= as.character(setDate),
      collectDate = as.character(collectDate), 
      identifiedDate = as.character(identifiedDate)) %>% 
    dplyr::mutate_all(list(~dplyr::na_if(.,""))) %>% 
    tidyr::drop_na(dplyr::all_of(cols_oi_exp_proc)) %>%
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::distinct()
  
  
  
  
  

  
  # # Look for duplicate sampleIDs #
  # length(which(duplicated(mos_trapping$sampleID)))  # 0
  # length(which(duplicated(mos_sorting$subsampleID))) # 0
  # length(which(duplicated(mos_archivepooling$archiveID))) # 0
  
  
  
  # # Make sure no "upstream" records are missing primary "downstream" reference
  # length(which(!mos_sorting$sampleID %in% mos_trapping$sampleID))  # 0
  # length(which(!mos_expertTaxonomistIDProcessed$subsampleID %in% mos_sorting$subsampleID))  # 0

  
  
  # Clear expertTaxonomist:individualCount if it is 0 and there is no taxonID. These aren't ID'ed samples
  mos_expertTaxonomistIDProcessed$individualCount[
    mos_expertTaxonomistIDProcessed$individualCount == 0 & 
      is.na(mos_expertTaxonomistIDProcessed$taxonID)] <- NA
  
  
  ####
  # Join data
  
  # Add trapping info to sorting table 
  # Note - 59 trapping records have no associated sorting record and don't come through the left_join (even though targetTaxaPresent was set to Y or U)
  mos_dat <- mos_sorting %>%
    dplyr::select(-c(uid,collectDate, domainID, namedLocation, plotID, 
                     setDate, siteID)) %>%
    dplyr::left_join(dplyr::select(mos_trapping,-uid),by = 'sampleID') 
  
  if(!"remarks" %in% names(mos_dat)) mos_dat[,"remarks"] <- NA_character_
  if(!"dataQF" %in% names(mos_dat)) mos_dat[,"dataQF"] <- NA_character_
  
  mos_dat <- mos_dat %>%
    dplyr::rename(sampCondition_sorting = sampleCondition.x,
                  sampCondition_trapping = sampleCondition.y,
                  remarks_sorting = remarks,
                  dataQF_trapping = dataQF)
  
  

  
  # # Verify sample barcode consistency before removing one column
  # which(mos_dat$sampleCode.x != mos_dat$sampleCode.y)
  # # Consistency is fine with barcodes
  
  

  
  
  
  mos_dat <- dplyr::rename(mos_dat,sampleCode = sampleCode.x) %>% 
    dplyr::select(-sampleCode.y)
  
  
  # Join expert ID data --
  mos_dat <- mos_dat %>%
    dplyr::left_join(dplyr::select(
      mos_expertTaxonomistIDProcessed,
      -c(collectDate,domainID,namedLocation,plotID,setDate,siteID,targetTaxaPresent)),
                     by='subsampleID') %>%
    dplyr::rename(remarks_expertID = remarks)
  
  
  
  
  

  # # Verify sample barcode and labName consistency before removing one column
  # which(mos_dat$subsampleCode.x != mos_dat$subsampleCode.y)
  # which(mos_dat$laboratoryName.x != mos_dat$laboratoryName.y)
  
  

  
  
  
  # Rename columns and add estimated total individuals for each subsample/species/sex with identification, where applicable
  #  Estimated total individuals = # individuals iD'ed * (total subsample weight/ subsample weight)
  mos_dat <- dplyr::rename(mos_dat,subsampleCode = subsampleCode.x, 
                           laboratoryName = laboratoryName.x) %>% 
    dplyr::select(-c(subsampleCode.y, laboratoryName.y)) %>%
    dplyr::mutate(
      estimated_totIndividuals = ifelse(
        !is.na(individualCount),
        round(individualCount * (totalWeight/subsampleWeight)), NA))
  
  
  # Add archive data --
  mos_dat <- mos_dat %>%
    dplyr::left_join(
      dplyr::select(mos_archivepooling,-c(domainID,uid,namedLocation,siteID)),
      by = 'archiveID')
  
  
  
  
  

  # # Verify sample barcode consistency before removing one column
  # which(mos_dat$archiveIDCode.x != mos_dat$archiveIDCode.y)
  
  # Consistency is fine with barcodes
  
  
  
  
  

  mos_dat <- dplyr::rename(mos_dat,archiveIDCode = archiveIDCode.x) %>% 
    dplyr::select(-archiveIDCode.y)
  
  # exclude records with taxonID is NA and remove dups
  mos_dat <- mos_dat %>%
    dplyr::filter(
      !is.na(taxonID),
      targetTaxaPresent == "Y", # very few N or U
      sampleCondition == "No known compromise",
      taxonRank != "family") %>%
    dplyr::distinct()
  
  
  


  #location ----
  table_location_raw <- mos_dat %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  plotType, nlcdClass,
                  decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", "plotType"))
  
  
  

  # taxon ----
  table_taxon <- mos_dat %>%
    dplyr::select(taxonID, taxonRank, scientificName,identificationReferences) %>%
    
    dplyr::distinct() %>% 
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = identificationReferences) %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) %>%
    dplyr::filter(!is.na(taxon_id)) %>%
    # concatenate different references for same taxonID
    dplyr::group_by(taxon_id, taxon_rank, taxon_name) %>%
    dplyr::summarize(
      authority_system = paste(authority_system, collapse = "; "))
  

  

  
  
  # observation ----
  
  my_package_id <- paste0(
    neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  table_observation_raw <- mos_dat %>% 
    dplyr::mutate(variable_name = "abundance",
                  value = (individualCount/subsampleWeight) * totalWeight / trapHours,
                  unit = "count per trap hour") %>% 
    dplyr::rename(
      observation_id = uid,
      neon_event_id = eventID,
      location_id = namedLocation,
      datetime = collectDate,
      taxon_id = taxonID,
      release = release.x,
      publicationDate = publicationDate.x) %>%
    dplyr::mutate(
      package_id = my_package_id,
      event_id = sampleID) %>%
    dplyr::filter(!is.na(taxon_id))
  
  
  
  table_observation <- table_observation_raw %>%
    dplyr::select(observation_id,
                  event_id,
                  package_id,
                  location_id,
                  datetime,
                  taxon_id,
                  variable_name,
                  value,
                  unit) 
  
  
  
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_raw,
    ancillary_var_names = c(
      "observation_id",
      "neon_event_id",
      "sortDate",
      "sampleID",
      "subsampleID",
      "totalWeight", #grams
      "subsampleWeight", #grams
      "weightBelowDetection",
      "laboratoryName",
      "trapHours", #hours
      "samplingProtocolVersion",
      "remarks_sorting",
      "sex",
      "nativeStatusCode",
      "release",
      "publicationDate")) %>%
    # add units where appropriate
    dplyr::mutate(
      unit = dplyr::case_when(
        variable_name == "totalWeight" ~ "grams",
        variable_name == "subsampleWeight" ~ "grams",
        variable_name == "trapHours" ~ "hours",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::distinct()
      
  
  
  # make data summary table ----
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
  
} # end of function

