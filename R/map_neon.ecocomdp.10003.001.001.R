##############################################################################################
##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon.ecocomdp.10003.001.001(site = c("NIWO","DSNY"), 
#'                                                   startdate = "2016-01", 
#'                                                   enddate = "2018-11")
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for BIRD taxa from neon.data.product.id DP1.10003.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for BIRD
map_neon.ecocomdp.10003.001.001 <- function(
  neon.data.product.id = "DP1.10003.001",
  ...){
  # Authors: Daijiang Li, Eric Sokol
  
  #NEON target taxon group is BIRD
  neon_method_id <- "neon.ecocomdp.10003.001.001"
  
  
  # check arguments passed via dots for neonUtilities
  dots_updated <- list(..., dpID = neon.data.product.id)
  
  #Error handling if user provides a value for "package" other than "expanded"
  if("package" %in% names(dots_updated) && dots_updated$package != "expanded") message(
    paste0("WARNING: expanded package for ", neon.data.product.id, 
           " is required to execute this request. Downloading the expanded package"))
  
  dots_updated[["package"]] <- "expanded"
  
  allTabs_bird <- rlang::exec( 
    neonUtilities::loadByProduct,
    !!!dots_updated)
  
  
  # allTabs_bird = neonUtilities::loadByProduct(dpID = neon.data.product.id, package = "expanded", ...)
  # saveRDS(allTabs_bird, file = "~/Documents/allTabs_bird.rds")
  # allTabs_bird = readRDS("~/Documents/allTabs_bird.rds")
  allTabs_bird$brd_countdata <- tibble::as_tibble(allTabs_bird$brd_countdata)
  allTabs_bird$brd_perpoint <- tibble::as_tibble(allTabs_bird$brd_perpoint)
  data_bird <- dplyr::left_join(allTabs_bird$brd_countdata,
                               dplyr::select(allTabs_bird$brd_perpoint, -uid,
                                             -startDate, -publicationDate))
  
  # table(data_bird$samplingImpractical) # all NA
  # table(data_bird$samplingImpracticalRemarks)
  
  data_bird <- dplyr::select(data_bird, 
                             # -uid, 
                             -identifiedBy, -publicationDate,
                            # -eventID, # it is just plotID, pointID, startDate
                            -laboratoryName, -measuredBy,
                            -samplingImpractical, -samplingImpracticalRemarks)
  
  


  
  #location ----
  table_location_raw <- data_bird %>%
    dplyr::select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- ecocomDP:::make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- ecocomDP:::make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", "geodeticDatum"))
  
  
  
  
  # taxon ----
  
  if("token" %in% names(dots_updated)){
    my_token <- dots_updated$token
  }else{
    my_token <- NA
  }
  
  # get bird taxon table from NEON
  neon_bird_taxon_table <- neonUtilities::getTaxonTable(
    taxonType = "BIRD", token = my_token) %>%
    dplyr::filter(taxonID %in% data_bird$taxonID)
  
  table_taxon <- neon_bird_taxon_table %>%
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

  
  


  # observation ----
  table_observation_wide_all <- data_bird %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(package_id = paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    dplyr:: rename(
      observation_id = uid, 
      neon_event_id = eventID,
      observation_datetime = startDate, 
      taxon_id = taxonID,
      value = clusterSize) %>%
    dplyr::mutate(
      event_id = observation_id,
      variable_name = "cluster size",
      unit = "count of individuals") 
  
  table_observation <- table_observation_wide_all %>%
    dplyr::select(
      observation_id,
      event_id,
      package_id,
      location_id,
      observation_datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>%
    dplyr::filter(!is.na(taxon_id))
  
  

  
  table_observation_ancillary <- ecocomDP:::make_neon_ancillary_observation_table(
    obs_wide = table_observation_wide_all,
    ancillary_var_names = c(
      "event_id",
      "neon_event_id",
      "plotID",
      "plotType",
      "pointID",
      "pointCountMinute",
      "targetTaxaPresent",
      "observerDistance",
      "detectionMethod",
      "visualConfirmation",
      "sexOrAge",
      "clusterCode",
      "release",
      "endCloudCoverPercentage",
      "observedHabitat",
      "observedAirTemp",
      "kmPerHourObservedWindSpeed",
      "samplingProtocolVersion",
      "remarks"))

  # data summary ----
  # make dataset_summary -- required table
  
  years_in_data <- table_observation$observation_datetime %>% lubridate::year()
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