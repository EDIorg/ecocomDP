##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for HERPTILE taxa from neon.data.product.id DP1.10022.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for HERPS
map_neon.ecocomdp.10022.001.002 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10022.001",
  ...){
  
  #NEON target taxon group is HERPS
  neon_method_id <- "neon.ecocomdp.10022.001.002"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  ### Get the Data
  bycatch_raw <- neon.data.list
  
  
  ### Wrangle Field Data
  
  # 1. Get the fielddata table that contains metadata for all sampling events.
  # 1. Clean up the trappingDays, which is a metric of catch per unit effort
  # 1. Clean eventID that provides the unique key for the events.
  # 1. Select the important variables and toss the rest.
  # 1. Calculate trappingDays as the length of time a pitfall trap was set
  
  tidy_fielddata <- tidyr::as_tibble(bycatch_raw$bet_fielddata) # get field data
  
  
  tidy_fielddata <- tidy_fielddata %>%
    dplyr::select(namedLocation,  # select needed variables
                  domainID,  siteID, plotID, plotType, trapID, nlcdClass,
                  decimalLatitude, decimalLongitude,
                  geodeticDatum, coordinateUncertainty,
                  elevation, elevationUncertainty,
                  setDate, collectDate, eventID, trappingDays,
                  sampleCollected, sampleID, sampleCondition,
                  samplingImpractical, remarksFielddata = remarks,
                  release, publicationDate) %>%
    dplyr::mutate(eventID_raw = eventID,
                  eventID = stringr::str_remove_all(eventID, "[.]")) %>% # remove periods from eventID's (cleans an inconsistency)
    dplyr::mutate(trappingDays = # add sampling effort in days (trappingdays)
                    lubridate::interval(lubridate::ymd(setDate),
                                        lubridate::ymd(collectDate)) %/%
                    lubridate::days(1))
  
  
  
  # 1. Make object with cleaned up serial bouts
  # There are some traps that have multiple bouts for the same setDate. This is
  # indicated by having the same setDate but different collectDates for a trap.
  # This indicates that the traps were reset to being open and sampling after a bout.
  # This serial sampling can also be seen by having unique eventIDs for each of these
  # serial bouts. To remedy this situation, trapping days should be calculated from
  # the previous collectDate, not the recorded setDate. To do this:
  
  adjTrappingDays <- tidy_fielddata %>%
    dplyr::select(namedLocation, trapID, setDate, collectDate, trappingDays, eventID) %>%
    dplyr::group_by(namedLocation, trapID, setDate) %>%
    dplyr::filter(dplyr::n_distinct(collectDate) > 1) %>% # filter those with more than one collectDate
    dplyr::mutate(totalSerialBouts = dplyr::n_distinct(collectDate))
  
  
  
  if(nrow(adjTrappingDays) > 0){
    #1. take the difference and use this as the new trapping days
    adjTrappingDays <- adjTrappingDays %>%
      dplyr::mutate(diffTrappingDays = # as long as there are only 2 bouts this works
               trappingDays - min(trappingDays)) %>%
      dplyr::mutate(adjTrappingDays = # if the difference is 0 then trappingDays
                      dplyr::case_when(diffTrappingDays == 0 ~ trappingDays,
                         TRUE ~ diffTrappingDays)) # otherwise use the difference
    
    
    
    # 1. Drop some columns from adjTrappingDays
    adjTrappingDays <- adjTrappingDays %>% # drop some columns
      dplyr::select(-c(trappingDays, diffTrappingDays, totalSerialBouts))
    
    # 1. Join trapping days to adjTrappingDays
    tidy_fielddata <- dplyr::left_join(tidy_fielddata, adjTrappingDays)
  }else{
    tidy_fielddata$adjTrappingDays <- NA_real_
  }
  
  
  
  
  #1. Adjust the trapping days
  tidy_fielddata <- tidy_fielddata %>%
    dplyr::mutate(trappingDays =
             dplyr::case_when(!is.na(adjTrappingDays) ~ adjTrappingDays, # use adjTrappingDays
                       TRUE ~ trappingDays # otherwise just use trappingDays
             )) %>% dplyr::select(-adjTrappingDays) # clean up the variables
  
  
  
  # 1. Change collectDate to the date that majority of traps were collected on
  Mode <- function(x) { # calculate mode of a column/vector
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  tidy_fielddata <- tidy_fielddata %>%
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup()
  
  
  # 1. Create a boutID that identifies all trap collection events at a site 
  # in the same bout (replacing eventID).
  # there are some sites for which all traps were set and collect on the 
  # same date, but have multiple eventID's
  # we want to consider that as all one bout so we create a new ID based on 
  # the site and collectDate
  
  tidy_fielddata <-  tidyr::unite(tidy_fielddata, boutID, siteID, 
                                  collectDate, remove = FALSE) #%>%
  #dplyr::select(-eventID) # beetles dropped the eventID, here I keep it
  
  
  ### Wrangle the Sorting data ###
  
  # 1. join with bet_sorting that has the bycatch in each sample
  
  tidy_sorting <- bycatch_raw$bet_sorting %>% # select the variables that make the most sense to keep
    dplyr::select(uid, sampleID, subsampleID, sampleType, taxonID, scientificName,
                  taxonRank, identificationReferences, individualCount,
                  nativeStatusCode, remarksSorting  = remarks) %>%
    tidyr::as_tibble()
  
  
  
  ### Produce the Full Data Set ###
  
  # 1. perform full_join on fielddata and sorting to make sure to get all the sampleIDs and remove missing sampleIDs
  
  data_herp_bycatch <- tidy_fielddata %>%
    dplyr::full_join(tidy_sorting) %>%
    dplyr::filter(!is.na(sampleID)) # remove all of the sampleIDs that are NAs
  
  
  # 1. Clean up the data_herp_bycatch so that it is focused on vert bycatch herp
  
  # sampleType provides the categories
  
  data_herp_bycatch <- data_herp_bycatch %>%
    dplyr::mutate(sampleType =
                    dplyr::case_when(is.na(sampleType) ~ "no data collected", # add a level for the missing sorting data
                                     TRUE ~ sampleType)) %>%
    dplyr::mutate(sampleCondition =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ sampleCondition)) %>%
    dplyr::mutate(samplingImpractical =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ samplingImpractical)) %>%
    dplyr::mutate(remarksFielddata =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ remarksFielddata)) %>%
    dplyr::mutate(subsampleID =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ subsampleID)) %>%
    dplyr::mutate(taxonID =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ taxonID)) %>%
    dplyr::mutate(taxonRank =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ taxonRank)) %>%
    dplyr::mutate(scientificName =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ scientificName)) %>%
    # mutate(identificationQualifier =
    #          case_when(sampleType!="vert bycatch herp" ~ NA_character_,
    #                    TRUE ~ identificationQualifier)) %>%
    # mutate(morphospeciesID =
    #          case_when(sampleType!="vert bycatch herp" ~ NA_character_,
    #                    TRUE ~ morphospeciesID)) %>%
    dplyr::mutate(identificationReferences =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ identificationReferences)) %>%
    dplyr::mutate(nativeStatusCode =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ nativeStatusCode)) %>%
    dplyr::mutate(remarksSorting =
                    dplyr::case_when(sampleType!="vert bycatch herp" ~ NA_character_,
                                     TRUE ~ remarksSorting))
  
  # 1. Decide if you don't care about any of the other sampleTypes
  
  

  # browser()
  
  
  # if(group_nonherps) { # group all nonherps
  data_herp_bycatch <-  data_herp_bycatch  %>%
    dplyr::mutate(sampleType =
                    dplyr::case_when(sampleType != "vert bycatch herp" ~ "not herp", # add a level for the missing sorting data
                       TRUE ~ sampleType)) %>%
    dplyr::group_by(sampleID, sampleType, scientificName) %>%
    dplyr::mutate(individualCount_sum = sum(individualCount, na.rm = TRUE)) %>% # sum up all of the not herps
    dplyr::mutate(individualCount_sum = dplyr::na_if(individualCount_sum, 0)) %>% # put NA back in the fielddata with missing sorting data
    dplyr::mutate(individualCount = individualCount_sum) %>% # replace with the new cout
    dplyr::select(-individualCount_sum) %>% # clean up
    dplyr::ungroup() # clean up
  # }
  
  # 1. Remove the redundant rows
  data_herp_bycatch <- dplyr::distinct(data_herp_bycatch)
  
  
  
  # #1. Only want samples with herps
  data_herp_bycatch <- dplyr::filter(data_herp_bycatch, sampleType == "vert bycatch herp",
                                     (is.na(sampleCondition) | sampleCondition == "OK")) %>%
    dplyr::select(
      # -boutID, # just siteID and colectDate
      -sampleCollected, # all Y
      # -sampleID,  
      -sampleCondition, -samplingImpractical,
      -eventID_raw, -subsampleID,
      -sampleType # all "vert bycatch herp"
    )
  
  
 
  
  #location ----
  table_location_raw <- data_herp_bycatch %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  plotID, plotType,
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
  
  # get HERP taxon table from NEON
  neon_taxon_table <- neonUtilities::getTaxonTable(
    taxonType = "HERPETOLOGY", token = my_token) %>%
    dplyr::filter(taxonID %in% data_herp_bycatch$taxonID)
  
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
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  table_observation_wide_all <- data_herp_bycatch %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(
      package_id = my_package_id,
      # neon_trap_id = trapID
      ) %>%
    dplyr:: rename(
      observation_id = uid, 
      datetime = setDate, 
      taxon_id = taxonID,
      value = individualCount/trappingDays) %>%
    dplyr::mutate(
      # event_id = observation_id,
      variable_name = "abundance",
      unit = "count per trap day") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      event_id = sampleID
      # neon_event_id = paste0(location_id, "_", neon_trap_id, "_", gsub("^(?i)[a-z]{4}_","",boutID))
      ) %>%
    dplyr::ungroup()
  
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
      "trapID",
      "sampleID",
      "trappingDays",
      "identificationReferences",
      "nativeStatusCode",
      "remarksFielddata",
      "remarksSorting",
      "release", 
      "publicationDate"))
  
  
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