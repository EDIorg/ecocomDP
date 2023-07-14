##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for SMALL_MAMMAL taxa from neon.data.product.id DP1.10072.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################

rm(list=ls())
sites<-c("HARV", "SCBI", "UNDE")
devtools::load_all()
require(neonUtilities)
# Load portal data
smammals <- loadByProduct(
  startdate = "2022-06", 
  enddate = "2022-08",
  dpID= 'DP1.10072.001',
  check.size=F, 
  site = sites,
  package='basic',
  token = Sys.getenv('NEON_PAT')) # remove this line if you don't have a token, though everyone is encouraged to get one
neon.data.list = smammals
neon.data.product.id = "DP1.10072.001"
#neon.data.list = list of dataframes returned by neonUtilities.
#keep list of things changed for Eric so that he can deprecate the old one and release an update.
#No pull request needed just an example script

# mapping function for SMALL_MAMMAL taxa
map_neon.ecocomdp.10072.001.001 <- function(
    neon.data.list,
    neon.data.product.id = "DP1.10072.001",
    ...){
  
  ### This code downloads, unzips and restructures small mammal data (raw abundances) from NEON
  ### for each NEON site, we provide mammal raw abundance aggregated per day (across all years),
  ### per month (bout) (across all years), and per year
  
  #NEON target taxon group is SMALL_MAMMALS
  neon_method_id <- "neon.ecocomdp.10072.001.001"
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  ### Get the Data
  d <- neon.data.list
  ##########SP ADDITIONS TO USE EVENTID AS BOUT:
  #Note this uses the neonOS packge joining function so may need to load that somewhere if it's a new package
  #Join tables with 
  mamjn<-neonOS::joinTableNEON(d$mam_perplotnight, d$mam_pertrapnight, name1 = "mam_perplotnight", name2 = "mam_pertrapnight")

   #Get rid of the .x in column names that are duplicated in both tables:
    dat.mam<-dplyr::mutate(mamjn,
                           collectDate = lubridate::ymd(collectDate.y), 
                           year = lubridate::year(collectDate),
                           month = lubridate::month(collectDate),
                           day = lubridate::day(collectDate),
                           #bouts are designated by eventID in perplotnight table.  If that entry is missing
                           #bout is estimated as the year_month
                           bout = ifelse(is.na(eventID), paste(year, month, sep = "_"), eventID)) %>%
    tidyr::as_tibble() %>%
    dplyr::group_by(nightuid) %>%
    dplyr::mutate(n_trap_nights_per_night_uid = length(unique(trapCoordinate))) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(bout) %>%
    # dplyr::mutate(n_trap_nights_per_bout = sum(unique(trapCoordinate))) %>%
    dplyr::ungroup() 
    
  ### Here we provide the code that summarizes raw abundances per bout and year
  ### We keep scientificName of NA or blank (no captures) at this point
  # dat.mam <- filter(dat.mam, scientificName != "") #, !is.na(scientificName))
  dat.mam <- dplyr::mutate(dat.mam, scientificName = ifelse(scientificName == "", NA, scientificName))
  
  #########SP ADDITIONS FILTERING TO TARGET TAXA:
  #Next filter the captures down to the target taxa.  The raw table includes 
  #numerous records for opportunistic taxa that are not specifically targeted by 
  #our sampling methods. The abundance estimates for these non-target taxa 
  #(e.g., diurnal species) may be unreliable since these taxa are not explicitly 
  ##targeted by our method of overnight sampling.  The small mammal taxonomy table 
  #lists each taxonID as being target or not and can be used to filter to only 
  #target species.
  
  #Download the taxon table using neonOS:
  mam.tax <- neonOS::getTaxonList(taxonType="SMALL_MAMMAL", 
                                  recordReturnLimit=1000, verbose=T)
  #Get a list of target taxa:
  targetTaxa <- mam.tax %>% 
    filter(taxonProtocolCategory == "target") %>% 
    select(taxonID, scientificName)
  
  #######SP QUESTION: WHY REMOVING DEAD AND ESCAPED?  THEY'RE STILL PRESENT IN COMMUNITY?
  ### Remove species that are bycatch (non-target), dead, or escapted while processing
  ### remove all where the fate of the individual, unless marked and released, is
  ### 'dead' = dead, 'escaped' = escaped while handling, 'nontarget' = released, non-target species,
  ### should 'released' (= target or opportunistic species released without full processing) be also removed?
  ## D Li: probably not, released have 64,400 records and processed only have 7,365
  
  # table(dat.mam$fate)
  # dat.mam <- dplyr::filter(dat.mam, !fate %in% c("dead", "escaped", "nontarget"))
  # dat.mam <- dplyr::filter(dat.mam, fate != "released")
  dat.mam <- dplyr::filter(dat.mam, trapStatus %in% c("5 - capture","4 - more than 1 capture in one trap"))
  
  # unique(dat.mam$scientificName)
  # group_by(dat.mam, taxonRank) %>% tally() # mostly are sp and genus level
  #SP NOTE/QUESTION FOR ERIC: I added speciesGroup to capture the species complexes that are
    #impossible to distinguish in some regions (e.g., PELEPEMA)
  #SP The first filtering step gets rid of unprocessed individuals since they don't have taxonIDs at a useful level
  dat.mam <- dplyr::filter(dat.mam, taxonRank %in% c("genus", "species", "subspecies", "speciesGroup",NA)) %>%
    filter(taxonID %in% targetTaxa$taxonID)
  ### Remove recaptures -- Y and U (unknown); only retain N
  # table(dat.mam$recapture)
  # sum(is.na(dat.mam$recapture))
  
  # dat.mam <- dplyr::filter(dat.mam, recapture == "N" | is.na(recapture))
  
  # filter out recaptures within a bout, but not all recaptures
  dat.mam <- dat.mam %>%
    dplyr::group_by(bout) %>%
    dplyr::filter(!duplicated(tagID)) %>%
    dplyr::ungroup()
#SP NOTE: because of table join I had to add .y to some of the column names below:  
  ### Get raw abundances per day
  data_small_mammal <- dat.mam %>%
    dplyr::select(uid.y, nightuid, bout,
                  n_trap_nights_per_night_uid,
                  domainID, siteID, plotID, namedLocation, plotType,
                  collectDate,
                  nlcdClass, decimalLatitude,
                  decimalLongitude, geodeticDatum, coordinateUncertainty,
                  elevation, elevationUncertainty, 
                  trapCoordinate, trapStatus, 
                  year, month,
                  day, taxonID, scientificName, taxonRank, identificationReferences,
                  nativeStatusCode, sex, lifeStage,
                  pregnancyStatus, tailLength, totalLength, weight,
                  release.y, publicationDate.y) %>%
    dplyr::rename(uid = uid.y, release = release.y, publicationDate = publicationDate.y) %>%
    dplyr::filter(!is.na(taxonID)) %>%
    dplyr::distinct()
  
  

  #location ----
  table_location_raw <- data_small_mammal %>%
    dplyr::select(domainID, siteID, plotID, plotType, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  # table_location <- make_neon_location_table(
  #   loc_info = table_location_raw,
  #   loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  # 
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


  # get SMALL_MAMMAL taxon table from NEON
  #SP: I commented this section out and changed neon_taxon_table to mam.tax since I load it up higher
 # neon_taxon_table <- neonOS::getTaxonList( #Note - this function is deprecated in neonUtilities (in neonOS)
    #NOTE OUTPUT MAY HAVE CHANGED FOR TAXON TABLE
  #  taxonType = "SMALL_MAMMAL", token = my_token) %>%
   # dplyr::filter(taxonID %in% unique(stats::na.omit(data_small_mammal$taxonID)))
  
  table_taxon <- mam.tax %>%
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
  
  table_observation_all <- data_small_mammal %>%
    dplyr::rename(location_id = namedLocation) %>%
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr::rowwise() %>%
    dplyr:: rename(
      datetime = collectDate, 
      taxon_id = taxonID) %>%
    dplyr::mutate(
      event_id = paste(location_id, year, month, sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      occurrence = 1)
  
  table_event <- table_observation_all %>%
    dplyr::select(package_id, 
                  plotID, 
                  plotType,
                  location_id,
                  event_id, 
                  nightuid, n_trap_nights_per_night_uid,
                  datetime, 
                  year, month) %>%
    dplyr::distinct() %>%
    dplyr::group_by(
      package_id, event_id, location_id, plotID, year, month) %>%
    dplyr::summarize(
      datetime = dplyr::first(datetime),
      n_trap_nights_per_bout_per_plot = sum(n_trap_nights_per_night_uid),
      n_nights_per_bout = length(unique(nightuid))) %>%
    dplyr::ungroup()
  
  #DSNY_004 2016 has 6 nights?
  # table_event %>% dplyr::filter(plotID == "DSNY_004", year == 2016, month == 11) %>%
  #   as.data.frame()
 
  table_raw_counts <- table_observation_all %>%
    dplyr::select(
      event_id,
      taxon_id,
      nativeStatusCode, 
      release, publicationDate,
      occurrence) %>%
    dplyr::group_by_at(dplyr::vars(
      event_id, taxon_id)) %>%
    dplyr::summarize(
      raw_count = sum(occurrence),
      nativeStatusCode = paste(unique(nativeStatusCode), collapse = "|"),
      release = paste(unique(release), collapse = "|"), 
      publicationDate= paste(unique(publicationDate), collapse = "|")) 
  
  table_event_counts <- table_event %>%
    dplyr::left_join(table_raw_counts, by = "event_id") %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)),
      value = 100 * raw_count / n_trap_nights_per_bout_per_plot,
      variable_name = "count",
      unit = "unique individuals per 100 trap nights per plot per month")
  
  table_observation <- table_event_counts %>%
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
    obs_wide = table_event_counts,
    ancillary_var_names = c(
      "observation_id",
      "year",
      "month",
      "n_trap_nights_per_bout_per_plot",
      "n_nights_per_bout",
      "nativeStatusCode",
      "release", 
      "publicationDate")) %>% 
    dplyr::distinct()
  
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
    #location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  return(out_list)
  
}
test<-map_neon.ecocomdp.10072.001.001(neon.data.list = smammals, neon.data.product.id = "DP1.10072.001")
