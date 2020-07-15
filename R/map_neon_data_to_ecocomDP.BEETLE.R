##############################################################################################
##############################################################################################
#' @title Map BEETLE adundance data to ecocomDP format

#' @author
#' Ruvi Jaimes \email{jaimesr@battelleecology.org}
#' Kari Norman \email{kari.norman@berkeley.edu}

#' @description
#' Pull files from the NEON API by data product, merge data for each table, and convert to ecocomDP format. Please see neonUtilities::loadByProduct for more information. 
#'
#' @param site a list of the sites (ex. c('ABBY','BARR'))
#' @param startdate the start date of the data from the sites (ex."2019-06" )
#' @param enddate the end date of the data from the sites (ex."2020-02" )
#' 
#' @return Returns a named list, including: 
#' \item{table_location}{A table, which has the lat long for each NEON site included in the data set}
#' \item{table_taxon}{A table, which describes each taxonID that appears in the data set}
#' \item{table_observation}{A table, which describes each taxonID that appears in the data set}
#' 
#' 
#' @example 
#' \dontrun{
#' my_result <- map_neon_data_to_ecocomDP.BEETLE(site = c('ABBY','BARR'),
#'                                               startdate = "2019-06", 
#'                                               enddate = "2019-09")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for BEETLE from neon.data.product.id DP1.10022.001 from the NEON data portal and map to the ecocomDP 
#' @export
#' 
##############################################################################################


map_neon_data_to_ecocomDP.BEETLE <- function(
  neon.data.product.id = "DP1.10022.001",
  ...){
  
  # getting data ----
  #download data
  beetles_raw <- neonUtilities::loadByProduct(dpID = neon.data.product.id, 
                                              check.size = FALSE,
                                              ...)
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  data <- beetles_raw$bet_fielddata %>%
    dplyr::filter(sampleCollected == "Y") %>% #there's an entry for every trap, whether or not they got samples, only want ones with samples
    dplyr::select(sampleID, domainID, siteID, plotID, trapID, setDate, collectDate, eventID, trappingDays) %>%
    #eventID's are inconsistently separated by periods, so remove
    dplyr::mutate(eventID = stringr::str_remove_all(eventID, "[.]")) %>%
    dplyr::mutate(trappingDays = lubridate::interval(lubridate::ymd(setDate), lubridate::ymd(collectDate)) %/% lubridate::days(1))
  
  
  
  
  
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
                       dplyr::select(sampleID, subsampleID, sampleType, taxonID, scientificName, taxonRank, individualCount,identificationQualifier),
                     by = "sampleID") %>%
    dplyr::filter(!is.na(subsampleID)) #even though they were marked a sampled, some collection times don't acutally have any samples
  
  # Join taxonomic data from pinning with the sorting data
  # Replace sorting taxon info with pinning taxon info (people that pin specimens are more experienced with taxonomy), where available
  data_pin <- data %>%
    dplyr::left_join(beetles_raw$bet_parataxonomistID %>% select(subsampleID, individualID, taxonID, scientificName, taxonRank,identificationQualifier), by = "subsampleID") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), "sort", "pin")) %>%
    dplyr::mutate (identificationQualifier = ifelse(is.na(taxonID.y), identificationQualifier.x, identificationQualifier.y)) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  #some subsamples weren't fully ID'd by the pinners, so we have to recover the unpinned-individuals
  lost_indv <- data_pin %>% 
    dplyr::filter(!is.na(individualID)) %>%
    dplyr::group_by(subsampleID, individualCount) %>%
    dplyr::summarise(n_ided = n_distinct(individualID)) %>% 
    dplyr::filter(n_ided < individualCount) %>%
    dplyr::mutate(unidentifiedCount = individualCount - n_ided) %>%
    dplyr::select(subsampleID, individualCount = unidentifiedCount) %>%
    dplyr::left_join(data %>% select(-individualCount), by = "subsampleID") %>%
    dplyr::mutate(identificationSource = "sort")
  
  #add unpinned-individuals back to the pinned id's, adjust the individual counts so pinned individuals have a count of 1
  data_pin <- data_pin %>%
    dplyr::mutate(individualCount = ifelse(identificationSource == "sort", individualCount, 1)) %>%
    dplyr::bind_rows(lost_indv)
  #Join expert data to existing pinning and sorting data
  #There are ~10 individualID's for which experts ID'd more than one species (not all experts agreed), we want to exclude those expert ID's as per Katie Levan's suggestion
  ex_expert_id <- beetles_raw$bet_expertTaxonomistIDProcessed %>% 
    dplyr::group_by(individualID) %>% 
    dplyr::filter(n_distinct(taxonID) > 1) %>% 
    dplyr::pull(individualID)
  
  # Add expert taxonomy info, where available
  data_expert <- dplyr::left_join(data_pin, 
                                  dplyr::select(beetles_raw$bet_expertTaxonomistIDProcessed,
                                                individualID,taxonID,scientificName,taxonRank,identificationQualifier) %>%
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
    dplyr::mutate (identificationQualifier = ifelse(is.na(taxonID.y), identificationQualifier.x, identificationQualifier.y)) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  
  
  
  #Get raw counts table
  beetles_counts <- data_expert %>%
    dplyr::select(-c(subsampleID, sampleType, individualID, identificationSource, identificationQualifier)) %>%
    dplyr::group_by_at(vars(-individualCount)) %>%
    dplyr::summarise(count = sum(individualCount)) %>%
    dplyr::ungroup()
  
  
  # making tables ----
  # Observation Tables
  # All individuals of the same species collected at the same time/same location are considered the same observation, regardless of how they were ID'd
  table_observation <- beetles_counts %>%
    tidyr::unite(location_id, plotID, trapID) %>%
    dplyr::rename(abundance = count) %>%
    tidyr::pivot_longer(c(abundance, trappingDays), names_to = "variable_name", values_to = "value") %>%
    dplyr::mutate(unit = "count") %>%
    #create observation_id column
    dplyr::group_by(sampleID) %>%
    dplyr::mutate(observation_id = paste(sampleID, row_number(), sep = ".")) %>%
    dplyr::mutate(package_id = paste0(neon.data.product.id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    dplyr:: select(observation_id = uid, 
                   event_id = sampleID, 
                   package_id,
                   location_id, 
                   observation_datetime = collectDate, 
                   taxon_id = taxonID, 
                   variable_name, 
                   value,
                   unit)
  
  
  # Location Tables
  # *The location ID and the name are synonymous
  ################################not sure what parent_location_id is
  table_location_raw <- beetles_raw$bet_fielddata %>%
    dplyr::select(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude,elevation) %>% 
    dplyr::distinct() 
  
  table_location <- suppressMessages(
    beetles_raw$ bet_fielddata %>% 
      ecocomDP::make_location(cols = c("domainID", "siteID", "namedLocation")))
  
  # populate latitude
  table_location$latitude <- table_location_raw$decimalLatitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  #populate longitude
  table_location$longitude <- table_location_raw$decimalLongitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # populate elevation
  table_location$elevation <- table_location_raw$elevation[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  
  # Taxon Tables 
  table_taxon <- bind_rows(beetles_raw$bet_sorting %>%
                             dplyr::select(taxonID, taxonRank, scientificName), 
                           beetles_raw$bet_parataxonomistID %>%
                             dplyr::select(taxonID, taxonRank, scientificName), 
                           beetles_raw$bet_expertTaxonomistIDProcessed %>%
                             dplyr::select(taxonID, taxonRank, scientificName)) %>% 
    dplyr::distinct() %>%
    dplyr::filter(scientificName != "Carabidae spp.", taxonID != "") %>% #remove typo (entry with the apropriate sciName is already in df)
    dplyr::select(taxon_id = taxonID, taxon_rank = taxonRank, taxon_name = scientificName) %>%
    dplyr::mutate(authority_system = "itis", 
                  #create column on cleaned scientific names to get id's from 
                  taxon_name_clean = taxadb::clean_names(stringr::str_replace(taxon_name, " \\(.*\\)", ""), lowercase = FALSE),
                  authority_taxon_id = taxadb::get_ids(taxon_name_clean, "itis", "bare")) %>%
    dplyr::select(-taxon_name_clean)
  
  # return tables ----
  
  out_list <- list(
    observation = table_observation,
    location = table_location,
    taxon = table_taxon
  )
  
  return(out_list)
} # end of function
