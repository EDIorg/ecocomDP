##############################################################################################
##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon_data_to_ecocomDP.MOSQUITO(site = c("NIWO","DSNY"), 
#'                                                   startdate = "2016-01", 
#'                                                   enddate = "2018-11")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for MOSQUITO from neon.data.product.id DP1.10043.001 from the NEON data portal and map to the ecocomDP 
#' @export

##############################################################################################
map_neon_data_to_ecocomDP.MOSQUITO <- function(
  neon.data.product.id = "DP1.10043.001",
  ...){
  
  mos_allTabs <- neonUtilities::loadByProduct(
    dpID = neon.data.product.id, 
    package = "expanded",
    ...)
  
  
  

  # getting data ----  
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
  
  
  ### problem here
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
  mos_expertTaxonomistIDProcessed$individualCount[mos_expertTaxonomistIDProcessed$individualCount == 0 & is.na(mos_expertTaxonomistIDProcessed$taxonID)] <- NA
  
  
  ####
  # Join data
  
  # Add trapping info to sorting table 
  # Note - 59 trapping records have no associated sorting record and don't come through the left_join (even though targetTaxaPresent was set to Y or U)
  mos_dat <- mos_sorting %>%
    dplyr::select(-c(uid,collectDate, domainID, namedLocation, plotID, setDate, siteID)) %>%
    dplyr::left_join(dplyr::select(mos_trapping,-uid),by = 'sampleID') %>%
    dplyr::rename(sampCondition_sorting = sampleCondition.x,
                  sampCondition_trapping = sampleCondition.y,
                  remarks_sorting = remarks,
                  dataQF_trapping = dataQF)
  
  # # Verify sample barcode consistency before removing one column
  # which(mos_dat$sampleCode.x != mos_dat$sampleCode.y)
  
  # Consistency is fine with barcodes
  mos_dat <- dplyr::rename(mos_dat,sampleCode = sampleCode.x) %>% 
    dplyr::select(-sampleCode.y)
  
  
  # Join expert ID data --
  mos_dat <- mos_dat %>%
    dplyr::left_join(dplyr::select(mos_expertTaxonomistIDProcessed,
                            -c(collectDate,domainID,namedLocation,plotID,setDate,siteID,targetTaxaPresent)),
                     by='subsampleID') %>%
    dplyr::rename(remarks_expertID = remarks)
  
  
  # Verify sample barcode and labName consistency before removing one column
  which(mos_dat$subsampleCode.x != mos_dat$subsampleCode.y)
  which(mos_dat$laboratoryName.x != mos_dat$laboratoryName.y)
  
  # Rename columns and add estimated total individuals for each subsample/species/sex with identification, where applicable
  #  Estimated total individuals = # individuals iD'ed * (total subsample weight/ subsample weight)
  mos_dat <- dplyr::rename(mos_dat,subsampleCode = subsampleCode.x, laboratoryName = laboratoryName.x) %>% 
    dplyr::select(-c(subsampleCode.y, laboratoryName.y)) %>%
    dplyr::mutate(estimated_totIndividuals = ifelse(!is.na(individualCount),round(individualCount * (totalWeight/subsampleWeight)), NA))
  
  
  # Add archive data --
  mos_dat <- mos_dat %>%
    dplyr::left_join(dplyr::select(mos_archivepooling,-c(domainID,uid,namedLocation,siteID)),by = 'archiveID')
  
  # Verify sample barcode consistency before removing one column
  which(mos_dat$archiveIDCode.x != mos_dat$archiveIDCode.y)
  
  # Consistency is fine with barcodes
  mos_dat <- dplyr::rename(mos_dat,archiveIDCode = archiveIDCode.x) %>% 
    dplyr::select(-archiveIDCode.y)
  
  # exclude records with taxonID is NA and remove dups
  mos_dat <- mos_dat %>%
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::distinct()
  
  
  
  
  #location ----
  table_location_raw <- mos_dat %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct() 
  
  table_location <- suppressMessages(
    mos_dat %>%
      ecocomDP::make_location(cols = c("domainID", "siteID", "namedLocation")))
  
  # code to handle updated make_location (updated 18 Sep 2020 in make_location branch)
  if(class(table_location) == "list" &&
     "location" %in% names(table_location)){
    
    table_location <- table_location$location %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        location_name = location_name %>% 
          strsplit("=") %>%
          unlist() %>%
          dplyr::last()) 
  }
  
 
  
  # populate latitude
  table_location$latitude <- table_location_raw$decimalLatitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  #populate longitude
  table_location$longitude <- table_location_raw$decimalLongitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # populate elevation
  table_location$elevation <- table_location_raw$elevation[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  
  
  # replace parent_id with parent_name
  table_location$parent_location_id <- table_location$location_name[
    match(table_location$parent_location_id, table_location$location_id)]
  
  # replace loc_id with meaningful names
  table_location$location_id <- table_location$location_name
  
  # get neon location info lookup tables
  neon_domain_list <- neon_site_list %>%
    dplyr::select(`Domain Number`, `Domain Name`) %>%
    dplyr::distinct()
  
  neon_site_info_list <- neon_site_list %>%
    dplyr::select(-c(`Domain Number`,`Domain Name`)) %>%
    dplyr::distinct()
  
  
  
  # update location_names and lat longs where possible
  for(location_id in table_location$location_id){
    if(location_id %in% neon_domain_list$`Domain Number`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_domain_list$`Domain Name`[neon_domain_list$`Domain Number`==location_id]
    }else if(location_id %in% neon_site_info_list$`Site ID`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_site_info_list$`Site Name`[neon_site_info_list$`Site ID`==location_id]
      
      table_location$latitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Latitude[neon_site_info_list$`Site ID`==location_id]
      
      table_location$longitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Longitude[neon_site_info_list$`Site ID`==location_id]
      
    }
  }
  
  
  # make ancillary table that indicates the location type 
  table_location_ancillary <- table_location_raw %>% 
    dplyr::select(domainID, siteID, namedLocation) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "value",
      values_to = "location_id") %>%
    dplyr::mutate(
      variable_name = "NEON location type",
      location_ancillary_id = paste0("NEON_location_type_",location_id)) %>%
    dplyr::distinct()
  

  # taxon ----
  table_taxon <- mos_dat %>%
    dplyr::select(taxonID, taxonRank, scientificName,identificationReferences) %>%
    
    dplyr::distinct() %>% 
    
    
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = identificationReferences) %>%
    
    # dplyr::mutate(authority_system = "NEON_external_lab") %>% 
    
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) %>%
    
    dplyr::filter(!is.na(taxon_id))
  
  

  # observation ----
  table_observation <- mos_dat %>% 
    dplyr::select(uid, 
                  sampleID, 
                  namedLocation, 
                  # startCollectDate,
                  collectDate,
                  taxonID,
                  totalWeight,
                  trapHours,
                  individualCount,
                  subsampleWeight) %>% 
    
    dplyr::mutate(variable_name = "individuals",
                  value = (individualCount/subsampleWeight) * totalWeight / trapHours,
                  unit = "count per trap hour") %>% 
    
    dplyr::rename(observation_id = uid,
                  event_id = sampleID,
                  location_id = namedLocation,
                  observation_datetime = collectDate,
                  taxon_id = taxonID) %>%
    
    dplyr::mutate(package_id = paste0(neon.data.product.id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    
    dplyr::select(observation_id,
                  event_id,
                  package_id,
                  location_id,
                  observation_datetime,
                  taxon_id,
                  variable_name,
                  value,
                  unit) %>%
    dplyr::filter(!is.na(taxon_id))
  
  table_observation_ancillary_wide <- mos_dat %>% 
    dplyr::select(eventID, sampleID) %>% 
    dplyr::filter(!is.na(sampleID)) %>%
    dplyr::rename(neon_sample_id = sampleID,
           neon_event_id = eventID) %>% 
    dplyr::mutate(event_id = neon_sample_id) %>%
    dplyr::distinct()
  
  table_observation_ancillary <- table_observation_ancillary_wide %>%
    tidyr::pivot_longer(
      cols = -event_id,
      names_to = "variable_name",
      values_to = "value") %>% 
    dplyr::mutate(
      observation_ancillary_id = paste0(variable_name, "_for_", event_id)) %>%
    dplyr::distinct()
  
  
  
  # make data summary table ----
  years_in_data <- table_observation$observation_datetime %>% lubridate::year()
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

