##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve percent cover data for PLANT taxa from neon.data.product.id DP1.10058.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################

# mapping function for PLANT taxa
map_neon.ecocomdp.10058.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10058.001",
  ...){
  #NEON target taxon group is PLANT
  neon_method_id <- "neon.ecocomdp.10058.001.001"
  
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  
  ### Get the Data
  allTabs_plant <- neon.data.list
  
  
  
  
  table(allTabs_plant$div_1m2Data$divDataType)
  div_1m2_pla <- dplyr::filter(allTabs_plant$div_1m2Data, divDataType == 'plantSpecies')
  # Remove 1m2 data with targetTaxaPresent = N
  table(div_1m2_pla$targetTaxaPresent)
  div_1m2_pla <- dplyr::filter(div_1m2_pla, targetTaxaPresent != 'N')
  
  div_1m2_oVar <- dplyr::filter(allTabs_plant$div_1m2Data, divDataType == 'otherVariables')
  table(div_1m2_oVar$otherVariables)
  
  div_10_100_m2 <- allTabs_plant$div_10m2Data100m2Data
  
  # Remove rows without plotID, subplotID, boutNumber, endDate, and/or taxonID
  div_1m2_pla <- tidyr::drop_na(div_1m2_pla, plotID, subplotID, boutNumber, endDate, taxonID)
  div_10_100_m2 <- tidyr::drop_na(div_10_100_m2, plotID, subplotID, boutNumber, endDate, taxonID)
  
  # Remove duplicate taxa between nested subplots (each taxon should be represented once for the bout/plotID/year).
  # 1) If a taxon/date/bout/plot combo is present in 1m2 data, remove from 10/100
  div_1m2_pla <- dplyr::mutate(div_1m2_pla, primaryKey = paste(plotID, boutNumber, substr(endDate, 1, 4),
                                                               taxonID, subplotID, sep = '_'),
                               key2 = gsub("[.][0-9]$", "", primaryKey),
                               key3 = gsub("[.][0-9]$", "", key2))
  div_10_100_m2 <- dplyr::mutate(div_10_100_m2, primaryKey = paste(plotID, boutNumber, substr(endDate, 1, 4),
                                                                   taxonID, subplotID, sep = '_'),
                                 key2 = gsub("[.][0-9]{2}$", "", primaryKey),
                                 key3 = gsub("[.][0-9]$", "", key2))
  # additional species in 10m2 and 100m2 but not in 1m2
  div_10_100_m2_2 <- dplyr::filter(div_10_100_m2, !key2 %in% unique(div_1m2_pla$key2))
  # species in 10m2 only
  div_10_100_m2_3 <- dplyr::filter(div_10_100_m2_2, key2 != key3)
  # species in 100m2 only (remove species already in 1m2 or 10m2)
  div_10_100_m2_4 <- dplyr::filter(div_10_100_m2_2, key2 == key3,
                                   !key3 %in% unique(c(div_1m2_pla$key3, div_10_100_m2_3$key3)))
  div_10_100_m2_5 = dplyr::bind_rows(dplyr::mutate(div_10_100_m2_3, sample_area_m2 = 10),
                                     dplyr::mutate(div_10_100_m2_4, sample_area_m2 = 100))
  
  # stack data
  data_plant = dplyr::bind_rows(
    dplyr::select(
      div_1m2_pla, 
      uid, domainID, namedLocation, siteID, plotType,
      decimalLatitude, decimalLongitude,
      geodeticDatum, coordinateUncertainty, 
      elevation, elevationUncertainty, nlcdClass,
      plotID, subplotID, boutNumber, endDate, 
      taxonID, scientificName, taxonRank, identificationReferences,
      family, nativeStatusCode, percentCover, 
      heightPlantOver300cm, heightPlantSpecies,
      release, publicationDate) %>%
      dplyr::mutate(sample_area_m2 = 1), # 1m2
    dplyr::select(
      div_10_100_m2_5, 
      uid, domainID, namedLocation, 
      siteID, plotType, decimalLatitude, decimalLongitude,
      geodeticDatum, coordinateUncertainty, 
      elevation, elevationUncertainty, nlcdClass,
      plotID, subplotID, boutNumber, endDate, 
      taxonID, scientificName, taxonRank,  identificationReferences,
      family, nativeStatusCode, sample_area_m2,
      release, publicationDate)
  ) %>%
    dplyr::mutate(subplot_id = substr(subplotID, 1, 2),
                  subsubplot_id = substr(subplotID, 4, 4),
                  year = lubridate::year(lubridate::ymd(endDate))) %>%
    unique() %>%
    tidyr::as_tibble()
  
  
  data_plant <- dplyr::distinct(data_plant)
  
  # family or order level data are pretty much not useful.
  data_plant <- dplyr::filter(
    data_plant, 
    taxonRank %in% c("variety", "subspecies", "species", "speciesGroup", "genus"))
  # NOTE: we have not cleaned species names; users need to do it
  
 
  
  #location ----
  table_location_raw <- data_plant %>%
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
    ancillary_var_names = c("namedLocation", "plotType", "nlcdClass", "geodeticDatum"))
  
  
  # taxon ----
  table_taxon <- data_plant %>%
    dplyr::select(taxonID, taxonRank, scientificName, identificationReferences) %>%
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName) %>%
    dplyr::mutate(authority_system = "USDA, NRCS. The PLANTS Database (http://plants.usda.gov)") %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) %>%
    dplyr::distinct()
    
   
  
  # observation ----
  
  my_package_id <- paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  table_observation_wide_all <- data_plant %>%
    dplyr::rename(location_id = namedLocation) %>%
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr:: rename(
      observation_id = uid, 
      datetime = endDate, 
      taxon_id = taxonID,
      value = percentCover) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # neon_event_id = paste0(location_id,"_",subplot_id,"_",year,"-",boutNumber),
      event_id = paste0(location_id,"_",subplot_id,"_",year,"-",boutNumber),
      variable_name = "percent cover",
      unit = "percent of plot area covered by taxon",
      presence_absence = 1) %>%
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
      # "neon_event_id",
      "subplotID", 
      "boutNumber",
      # "subplot_id",
      # "subsubplot_id",
      "nativeStatusCode",
      "identificationReferences",
      "nativeStatusCode",
      "heightPlantOver300cm",
      "heightPlantSpecies",
      "sample_area_m2",
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