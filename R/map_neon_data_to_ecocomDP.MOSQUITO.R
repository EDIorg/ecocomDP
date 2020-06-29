##############################################################################################
##############################################################################################
#' @title Map MOSQUITO density data to ecocomDP format

#' @author
#' Natalie Robinson \email{nrobinson@battelleecology.org}
#' Ruvi Jaimes \email{jaimesr@battelleecology.org}

#' @description
#' Pull files from the NEON API by data product, merge data for each table, and convert to ecocomDP format. Please see neonUtilities::loadByProduct for more information. 
#'
#' @param site a list of the sites (c("NIWO","DSNY"))
#' @param startdate the start date of the data from the sites ("2019-06" )
#' @param enddate the end date of the data from the sites ("2020-02" )
#' 
#' @return Returns a named list, including: 
#' \item{table_location}{A table, which has the lat long for each NEON site included in the data set}
#' \item{table_taxon}{A table, which describes each taxonID that appears in the data set}
#' \item{table_observation}{A table, which describes each taxonID that appears in the data set}
#' 
#' 
#' @example 
#' \dontrun{
#' my_result <- map_neon_data_to_ecocomDP.MOSQUITOES(site = c("NIWO","DSNY"), 
#'                                                   startdate = "2016-01", 
#'                                                   enddate = "2018-11")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve density data for MOSQUITO from neon.data.product.id DP1.10043.001 from the NEON data portal and map to the ecocomDP 
#' @export

##############################################################################################
# mosquitoes function
#library(dplyr)
#library(tidyr)
#library(neonUtilities)


# example <- map_neon_data_to_ecocomDP.MOSQUITOES(site = c("ABBY", "BARR"), startdate = "2017-01", enddate = "2017=12")

map_neon_data_to_ecocomDP.MOSQUITO <- function(
  neon.data.product.id ="DP1.10043.001",
  ...){
  
  mos_allTabs <- neonUtilities::loadByProduct(
    dpID = neon.data.product.id, package = "expanded",
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
    dplyr::mutate_all(list(~na_if(.,""))) %>%
    tidyr::drop_na(all_of(cols_oi_trap)) %>%
    dplyr::mutate_if(is.factor, as.character)
  
  
  ### problem here
  mos_sorting <- mos_allTabs$mos_sorting %>% 
    dplyr::mutate (
      
      setDate = as.character(setDate),
      collectDate = as.character(collectDate), 
      sortDate = as.character(sortDate)) %>% 
    
    dplyr::mutate_all(list(~na_if(.,""))) %>% 
    tidyr::drop_na(all_of(cols_oi_sort)) %>%
    dplyr::mutate_if(is.factor, as.character)
  
  
  
  mos_archivepooling <- mos_allTabs$mos_archivepooling %>% 
    dplyr::mutate(
      startCollectDate = as.character(startCollectDate),
      endCollectDate = as.character(endCollectDate)) %>% 
    dplyr::mutate_all(list(~na_if(.,""))) %>% 
    tidyr::drop_na(all_of(cols_oi_arch)) %>%
    dplyr::mutate_if(is.factor, as.character)
  
  
  mos_expertTaxonomistIDProcessed <- mos_allTabs$mos_expertTaxonomistIDProcessed %>% 
    dplyr::mutate(
      setDate= as.character(setDate),
      collectDate = as.character(collectDate), 
      identifiedDate = as.character(identifiedDate)) %>% 
    dplyr::mutate_all(list(~na_if(.,""))) %>% 
    tidyr::drop_na(all_of(cols_oi_exp_proc)) %>%
    dplyr::mutate_if(is.factor, as.character)
  
  
  
  # Look for duplicate sampleIDs #
  length(which(duplicated(mos_trapping$sampleID)))  # 0
  length(which(duplicated(mos_sorting$subsampleID))) # 0
  length(which(duplicated(mos_archivepooling$archiveID))) # 0
  
  
  
  # Make sure no "upstream" records are missing primary "downstream" reference
  length(which(!mos_sorting$sampleID %in% mos_trapping$sampleID))  # 0
  length(which(!mos_expertTaxonomistIDProcessed$subsampleID %in% mos_sorting$subsampleID))  # 0
  
  
  # Clear expertTaxonomist:individualCount if it is 0 and there is no taxonID. These aren't ID'ed samples
  mos_expertTaxonomistIDProcessed$individualCount[mos_expertTaxonomistIDProcessed$individualCount == 0 & is.na(mos_expertTaxonomistIDProcessed$taxonID)] <- NA
  
  
  ####
  # Join data
  
  # Add trapping info to sorting table 
  # Note - 59 trapping records have no associated sorting record and don't come through the left_join (even though targetTaxaPresent was set to Y or U)
  mos_dat <- mos_sorting %>%
    dplyr::select(-c(collectDate, domainID, namedLocation, plotID, setDate, siteID)) %>%
    dplyr::left_join(select(mos_trapping,-uid),by = 'sampleID') %>%
    dplyr::rename(sampCondition_sorting = sampleCondition.x,
                  sampCondition_trapping = sampleCondition.y,
                  remarks_sorting = remarks,
                  dataQF_trapping = dataQF)
  
  # Verify sample barcode consistency before removing one column
  which(mos_dat$sampleCode.x != mos_dat$sampleCode.y)
  
  # Consistency is fine with barcodes
  mos_dat <- dplyr::rename(mos_dat,sampleCode = sampleCode.x) %>% 
    dplyr::select(-sampleCode.y)
  
  
  # Join expert ID data --
  mos_dat <- mos_dat %>%
    dplyr::left_join(select(mos_expertTaxonomistIDProcessed,
                            -c(uid,collectDate,domainID,namedLocation,plotID,setDate,siteID,targetTaxaPresent)),
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
    dplyr::left_join(select(mos_archivepooling,-c(domainID,uid,namedLocation,siteID)),by = 'archiveID')
  
  # Verify sample barcode consistency before removing one column
  which(mos_dat$archiveIDCode.x != mos_dat$archiveIDCode.y)
  
  # Consistency is fine with barcodes
  mos_dat <- dplyr::rename(mos_dat,archiveIDCode = archiveIDCode.x) %>% 
    dplyr::select(-archiveIDCode.y)
  
  
  #location ----
  table_location_raw <- mos_dat %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
    dplyr::distinct() 
  
  table_location <- suppressMessages(
    mos_dat %>%  
      ecocomDP::make_location(cols = c("domainID", "siteID", "namedLocation")))
  
  # populate latitude
  table_location$latitude <- table_location_raw$decimalLatitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  #populate longitude
  table_location$longitude <- table_location_raw$decimalLongitude[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  # populate elevation
  table_location$elevation <- table_location_raw$elevation[match(table_location$location_name, table_location_raw$namedLocation)] 
  
  
  # taxon ----
  table_taxon <- mos_dat %>%
    dplyr::select(taxonID, taxonRank, scientificName,identificationReferences) %>%
    
    dplyr::distinct() %>% 
    
    
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_taxon_id = identificationReferences) %>%
    
    dplyr::mutate(authority_system = "NEON_external_lab") %>% 
    
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system,
                  authority_taxon_id)
  
  
  # observation ----
  table_observation <- mos_dat %>% 
    dplyr::select(uid, 
                  eventID, 
                  namedLocation, 
                  startCollectDate, 
                  taxonID,
                  estimated_totIndividuals,
                  subsampleWeight) %>% 
    
    dplyr::mutate(variable_name = "individuals",
                  value = round(estimated_totIndividuals / subsampleWeight),
                  unit = "count per gram") %>% 
    
    dplyr::rename(observation_id = uid,
                  event_id = eventID,
                  location_id = namedLocation,
                  observation_datetime = startCollectDate,
                  taxon_id = taxonID) %>%
    
    dplyr::mutate(package_id = NA) %>%
    
    dplyr::select(observation_id,
                  event_id,
                  package_id,
                  location_id,
                  observation_datetime,
                  taxon_id,
                  variable_name,
                  value,
                  unit)
  
  # return ----
  # list of tables to be returned, with standardized names for elements
  out_list <- list(
    location = table_location,
    taxon = table_taxon,
    observation = table_observation)
  
  # return out_list -- this is output from this function
  return(out_list)
  
} # end of function
