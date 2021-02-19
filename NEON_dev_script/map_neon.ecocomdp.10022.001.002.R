##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon.ecocomdp.10022.001.001(site = c('ABBY','BARR'),
#'                                               startdate = "2019-06", 
#'                                               enddate = "2019-09")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for BEETLE from neon.data.product.id DP1.10022.001 from the NEON data portal and map to the ecocomDP 
#' @export
#' 
##############################################################################################


map_neon.ecocomdp.10022.001.002 <- function(
  neon.data.product.id = "DP1.10022.001",
  ...){

  # getting data ----
  #download data
  beetles_raw <- neonUtilities::loadByProduct(dpID = neon.data.product.id, 
                                              ...)
  

  # helper function to calculate mode  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  

  # start data table with fielddata
  data <- beetles_raw$bet_fielddata %>%
    dplyr::filter(sampleCollected == "Y") %>% #there's an entry for every trap, whether or not they got samples, only want ones with samples
    dplyr::select(sampleID, domainID, siteID, 
                  # plotID, 
                  namedLocation, 
                  trapID, setDate, collectDate, eventID, trappingDays) %>%
    #eventID's are inconsistently separated by periods, so remove
    dplyr::mutate(eventID = stringr::str_remove_all(eventID, "[.]")) %>%
    dplyr::mutate(trappingDays = lubridate::interval(lubridate::ymd(setDate), lubridate::ymd(collectDate)) %/% lubridate::days(1))
  
  
  
  
  # join fielddata with bet_worting
  data <- data %>%
    #for some eventID's (bouts) collection happened over two days,
    #change collectDate to the date that majority of traps were collected on
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup() %>%
    #there are also some sites for which all traps were set and collect on the same date, but have multiple eventID's
    #we want to consider that as all one bout so we'll just create a new ID based on the site and collectDate
    tidyr::unite(boutID, siteID, collectDate, remove = FALSE) %>%
    dplyr::select(-eventID) %>%
    #and join to sample data
    dplyr::left_join(beetles_raw$bet_sorting %>%
                       dplyr::filter(sampleType %in% c("carabid", "other carabid")) %>% #only want carabid samples, not bycatch
                       dplyr::select(uid, 
                                     sampleID, subsampleID, sampleType, 
                                     taxonID, scientificName, taxonRank, 
                                     individualCount, identificationQualifier, identificationReferences),
                     by = "sampleID") %>%
    dplyr::filter(!is.na(subsampleID)) #even though they were marked a sampled, some collection times don't acutally have any samples
  
  
  

  # join data with bet_parataxonomistID
  # Join taxonomic data from pinning with the sorting data
  # Replace sorting taxon info with pinning taxon info (people that pin specimens are more experienced with taxonomy), where available
  data_pin <- data %>%
    dplyr::left_join(beetles_raw$bet_parataxonomistID %>% 
                       dplyr::select(subsampleID, individualID, taxonID, scientificName, 
                              taxonRank,identificationQualifier,
                              identificationReferences), by = "subsampleID") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), "sort", "pin")) %>%
    dplyr::mutate(identificationReferences = ifelse(is.na(identificationReferences.y), 
                                                    identificationReferences.x, identificationReferences.y)) %>%
    dplyr::mutate (identificationQualifier = ifelse(is.na(taxonID.y), identificationQualifier.x, identificationQualifier.y)) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  
  
  #some subsamples weren't fully ID'd by the pinners, so we have to recover the unpinned-individuals
  lost_indv <- data_pin %>% 
    dplyr::filter(!is.na(individualID)) %>%
    dplyr::group_by(subsampleID, individualCount) %>%
    dplyr::summarise(n_ided = dplyr::n_distinct(individualID)) %>% 
    dplyr::filter(n_ided < individualCount) %>%
    dplyr::mutate(unidentifiedCount = individualCount - n_ided) %>%
    dplyr::select(subsampleID, individualCount = unidentifiedCount) %>%
    dplyr::left_join(data %>% 
                       dplyr::select(-individualCount), by = "subsampleID") %>%
    dplyr::mutate(identificationSource = "sort")
  
  
  
  #add unpinned-individuals back to the pinned id's, adjust the individual counts so pinned individuals have a count of 1
  data_pin <- data_pin %>%
    dplyr::mutate(individualCount = ifelse(identificationSource == "sort", individualCount, 1)) %>%
    dplyr::bind_rows(lost_indv)
  

  
  #Join expert data to existing pinning and sorting data
  #There are ~10 individualID's for which experts ID'd more than one species (not all experts agreed), we want to exclude those expert ID's as per Katie Levan's suggestion

  ex_expert_id <- beetles_raw$bet_parataxonomistID %>% 
    dplyr::group_by(individualID) %>% 
    dplyr::filter(dplyr::n_distinct(taxonID) > 1) %>% 
    dplyr::pull(individualID)
  
  # Add expert taxonomy info, where available
  data_expert <- dplyr::left_join(data_pin, 
                                  dplyr::select(beetles_raw$bet_parataxonomistID,
                                                individualID,taxonID,scientificName,
                                                taxonRank,identificationQualifier,
                                                identificationReferences) %>%
                                    dplyr::filter(!individualID %in% ex_expert_id), #exclude ID's that have unresolved expert taxonomy
                                  by = 'individualID', na_matches = "never") %>% 
    dplyr::distinct()
  
  
  
 
  # Replacement old taxon info with expert info, where available
  # NOTE - This is repetitive with the code snippet above, and if you want to do it this way you can just combine the calls into one chunk. BUT, you may
  #     want to do more than this, as it is *just* a replacement of IDs for individual beetles that an expert identified. If the expert identified
  #           a sample as COLSP6 instead of CARSP14, though, then all CARSP14 from that trap on that date should probably be updated to COLSP6…
  data_expert <- data_expert %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), identificationSource, "expert")) %>%
    dplyr::mutate(identificationReferences = ifelse(is.na(identificationReferences.y), 
                                                    identificationReferences.x, identificationReferences.y)) %>%
    dplyr::mutate (identificationQualifier = ifelse(is.na(taxonID.y), identificationQualifier.x, identificationQualifier.y)) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  
 
  
  #Get raw counts table
  beetles_counts <- data_expert %>%
    dplyr::select(-c(subsampleID, sampleType, individualID, 
                     identificationSource, identificationQualifier)) %>%
    dplyr::group_by_at(dplyr::vars(-individualCount)) %>%
    dplyr::summarise(count = sum(individualCount)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    as.data.frame()

  
  
  
  
  # making tables ----
  # Observation Tables
  # All individuals of the same species collected at the same time/same location are considered the same observation, regardless of how they were ID'd

  table_observation_raw <- beetles_counts %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation,
                  abundance = count)
  
  
  
  
  
  table_observation <- table_observation_raw %>%
    # tidyr::pivot_longer(c(abundance, trappingDays), names_to = "variable_name", values_to = "value") %>%
    # dplyr::mutate(unit = "count") %>%
    #create observation_id column
    # dplyr::group_by(sampleID) %>%
    # dplyr::mutate(observation_id = paste(sampleID, row_number(), sep = ".")) %>%
    dplyr::mutate(package_id = paste0(neon.data.product.id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    # tidyr::spread(variable_name,value) %>% 
    dplyr:: select(observation_id = uid, 
                   event_id = sampleID, 
                   package_id,
                   location_id, 
                   observation_datetime = collectDate, 
                   taxon_id = taxonID,
                   value = abundance/trappingDays) %>% 
    dplyr::mutate(variable_name = "abundance",
                  unit = "count per trap day") %>% 
    dplyr::select(observation_id,
                  event_id,
                  package_id,
                  location_id,
                  observation_datetime,
                  taxon_id,
                  variable_name,
                  value,
                  unit)
  # table_observation <- table_observation[stats::complete.cases(table_observation[,8]),]
  

  # no units, all text
  table_observation_ancillary_wide <- beetles_raw$bet_fielddata %>% 
    dplyr::select(eventID, sampleID,
                  trappingDays,
                  samplingProtocolVersion, remarks) %>% 
    dplyr::mutate_all(as.character) %>%
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
    
    # add units where appropriate
    dplyr::mutate(
      unit = dplyr::case_when(
        variable_name == "trappingDays" ~ "days",
        TRUE ~ NA_character_
      )
    )
  

  
  
  
  # location ----
  # get relevant location info from the data
  table_location_raw <- beetles_raw$bet_fielddata %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation,
                  plotType, nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- suppressMessages(
    beetles_raw$bet_fielddata %>% 
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
  
  table_location_ancillary <- table_location_raw %>% 
    dplyr::select(namedLocation,
                  plotType, nlcdClass, geodeticDatum) %>%
    tidyr::pivot_longer(
      cols = -namedLocation,
      names_to = "variable_name",
      values_to = "value") %>%
    dplyr::rename(location_id = namedLocation) %>%
    dplyr::mutate(
      location_ancillary_id = paste0(variable_name, "_", location_id)) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(., table_location_ancillary)
  
 
  # # load neon taxon list
  # neon_taxon_list <- neonUtilities::getTaxonTable(taxonType = "BEETLE",
  #                              token = my_neon_token)
  # 
  # browser()
  
  # # Taxon Tables 
  # table_taxon <- dplyr::bind_rows(beetles_raw$bet_sorting %>%
  #                            dplyr::select(taxonID, taxonRank, scientificName), 
  #                          beetles_raw$bet_parataxonomistID %>%
  #                            dplyr::select(taxonID, taxonRank, scientificName), 
  #                          beetles_raw$bet_parataxonomistID %>%
  #                            dplyr::select(taxonID, taxonRank, scientificName)) %>% 
  #   dplyr::distinct() %>%
  #   dplyr::filter(scientificName != "Carabidae spp.", taxonID != "") %>% #remove typo (entry with the apropriate sciName is already in df)
  #   dplyr::select(taxon_id = taxonID, taxon_rank = taxonRank, taxon_name = scientificName) %>%
  #   dplyr::mutate(authority_system = "itis", 
  #                 #create column on cleaned scientific names to get id's from 
  #                 taxon_name_clean = taxadb::clean_names(stringr::str_replace(taxon_name, " \\(.*\\)", ""), lowercase = FALSE),
  #                 authority_taxon_id = taxadb::get_ids(taxon_name_clean, "itis", "bare")) %>%
  #   dplyr::select(-taxon_name_clean)
  
  
  
  
  # create a taxon table, which describes each taxonID that appears in the data set
  # start with inv_taxonomyProcessed
  table_taxon <- beetles_counts %>%
    
    # keep only the coluns listed below
    dplyr::select(taxonID, taxonRank, scientificName, identificationReferences) %>%
    
    # remove rows with duplicate information
    dplyr::distinct() %>%
    
    # rename some columns
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = identificationReferences)
  
  
  
  
  
  # make dataset_summary -- required table
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

# my_result <- map_neon.ecocomDP.10022.001.001(site = c('ABBY','BARR'),
#                                               startdate = "2016-01", enddate = "2018-09",
#                                               check.size = FALSE,
#                                               token = Sys.getenv("NEON_TOKEN"))

  