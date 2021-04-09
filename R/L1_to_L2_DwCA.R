#' Creates Darwin Core Archive package from an ecocomDP formatted level-1
#'
#' @param path 
#'     (character) Path to the where the DwC-A data objects and EML will be 
#'     written.
#' @param core.name
#'     (character) The Darwin Core central table of the package. Can be: 
#'     "occurence" (occurrence core) or "event" (event core).
#' @param parent.package.id
#'     (character) ID of an ecocomDP data package. Only 
#'     EDI Data Repository package IDs are currently supported.
#' @param child.package.id
#'     (character) ID of DWcA occurrence data package being created.
#' @param user.id
#'     (character; optional) Repository user identifier. If more than one, 
#'     then enter as a vector of character strings (e.g. 
#'     \code{c("user_id_1", "user_id_2")}). \code{user.id} sets the 
#'     /eml/access/principal element for all \code{user.domain} except
#'     "KNB", "ADC", and if \code{user.domain = NULL}.
#' @param user.domain
#'     (character; optional) Repository domain associated with 
#'     \code{user.id}. Currently supported values are "EDI" 
#'     (Environmental Data Initiative), "LTER" (Long-Term Ecological Research 
#'     Network), "KNB" (The Knowledge Network for Biocomplexity), "ADC" (The 
#'     Arctic Data Center). If you'd like your system supported please contact
#'     maintainers of the EMLassemblyline R package. If using more than one 
#'     \code{user.domain}, then enter as a vector of character strings (e.g. 
#'     \code{c("user_domain_1", "user_domain_2")}) in the same order as 
#'     corresponding \code{user.id}. If \code{user.domain} is missing then a 
#'     default value "unknown" is assigned. \code{user.domain} sets the EML 
#'     header "system" attribute and for all \code{user.domain}, except "KNB" 
#'     and "ADC", sets the /eml/access/principal element attributes and values.
#' @details
#'     TODO: Add details
#' @return
#'     Data entities in either the occurrence or event core formats with a
#'     corresponding EML metadata record and a meta.xml file.
#' @export
#'
#' @examples
#' 
L1_to_L2_DwCA <- function(path, 
                          core.name, 
                          parent.package.id, 
                          child.package.id,
                          data.table.url = NULL,
                          user.id,
                          user.domain) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("environment", envir = .GlobalEnv)) {
    environment <- get("environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Parameterize --------------------------------------------------------------
  
  # readr progress bar and messaging is not useful in this function
  
  dflt_prog <- getOption("readr.show_progress")
  options(readr.show_progress = FALSE)
  on.exit(options(readr.show_progress = dflt_prog))
  
  # Eventually will support 2 DwC types: choose occurence-core or event-core. 
  # occurence is simpler (sightings), event is a better fit for most of our data
  # TODO: Support event-core
  
  dwca_config_file <- suppressMessages(
    readr::read_csv(
      system.file(
        "/dwca_occurrence_core/dwca_occurrence_core_config.csv", 
        package = "ecocomDP")))
  dwca_mappings_file <- suppressMessages(
    readr::read_csv(
      system.file("/dwca_occurrence_core/dwca_occurrence_core_mappings.csv", 
                  package = "ecocomDP")))
  
  # Load data -----------------------------------------------------------------
  
  # FIXME: Add environment argument to ecocomDP::read_data()
  d <- ecocomDP::read_data(parent.package.id)
  d <- d[[1]]$tables
  
  # Convert tables ------------------------------------------------------------
  
  if (core.name == 'occurrence') {
    # call a function to create occurrence core. inputs: data objects, 
    # dwca_mappings, dwca_config
    r <- create_table_dwca_occurrence_core(
      dwca_occurrence_core_config = dwca_config_file,
      dwca_occurrence_core_mapping = dwca_mappings_file,
      dt_obs = d$observation,
      dt_loc = d$location,
      dt_tax = d$taxon,
      dt_loc_ancil = d$location_ancillary,
      parent.package.id = parent.package.id)
  } else if (core.name == 'event') {
    # call a function to create event core. inputs: data objects, dwca_mappings, dwca_config  
    # print('calling function to create DwC-A, event core')
    r <- create_tables_dwca_event_core(
      dwca_occurrence_core_config = dwca_config_file,
      dwca_occurrence_core_mapping = dwca_mappings_file,
      dt_obs = d$observation,
      dt_loc = d$location,
      dt_tax = d$taxon,
      dt_loc_ancil = d$location_ancillary,
      dt_obs_ancil = d$observation_ancillary,
      parent.package.id = parent.package.id,
      child.package.id = child.package.id)
  }
  
  # Write tables to file ------------------------------------------------------
  
  # Set physical attributes here so they will match what is listed in meta.xml
  for (i in names(r)) {
    readr::write_delim(
      x = r[[i]],
      file = paste0(path, "/", i, ".csv"),
      delim = ",",
      quote_escape = "double",
      eol = "\r\n")
  }
  
  # Write meta.xml ------------------------------------------------------------
  
  if (core.name == "event") {
    file.copy(
      from = system.file("/dwca_event_core/meta.xml", package = "ecocomDP"),
      to = path)
  } else if (core.name == "occurrence") {
    file.copy(
      from = system.file("/dwca_occurrence_core/meta.xml", package = "ecocomDP"),
      to = path)
  }

  # Write EML to file ---------------------------------------------------------
  
  eml <- make_eml_dwca(
    path = path,
    core.name = core.name,
    parent.package.id = parent.package.id, 
    child.package.id = child.package.id, 
    user.id = user.id, 
    user.domain = user.domain,
    url = data.table.url)
  
}







#' Function creates DwC-A tables in occurrence core 
#'
#' @param dwca_occurrence_core_config 
#' @param dwca_occurrence_core_mapping 
#' @param dt_obs 
#' @param dt_loc 
#' @param dt_tax 
#' @param dt_loc_ancil 
#' @param parent.package.id
#'     (character) ID of an ecocomDP data package. Only 
#'     EDI Data Repository package IDs are currently supported.
#'
#' @return
#' @export
create_table_dwca_occurrence_core <- function(
  dwca_occurrence_core_config,
  dwca_occurrence_core_mapping,
  dt_obs,
  dt_loc,
  dt_tax,
  dt_loc_ancil = NULL,
  parent.package.id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Validate function inputs --------------------------------------------------
  # Do you have what you need to get these vars? confirm fields used by mapping 
  # table are present. Anything that is required by the ecocomDP model already, 
  # you can assume is present. location_ancillary.value, or 
  # observation_anncillary.value ?? what does this note mean? do I need these?
  
  # If dt_loc_ancil exists, then the occurence table can have sampleValueUnit
  if (!is.null(dt_loc_ancil)) {
    include_sampleSizeUnit_sampleSizeValue <- FALSE
    # TODO: include_sampleSizeUnit_sampleSizeValue <- TRUE
  } else {
    include_sampleSizeUnit_sampleSizeValue <- FALSE
  }
  
  # Join the tables -----------------------------------------------------------
  
  # TODO: Don't forget locationn ancillary (MOB add issue #)
  obs_loc_tax <- join_obs_loc_tax(
    dt_obs = dt_obs, 
    dt_loc = dt_loc, 
    dt_tax = dt_tax)
  
  # Add column
  # Create computed vectors:
  
  # Define new cols specifically needed for DwC-A
  obs_loc_tax$lsid <- NA_character_ # TODO: This will be an LSID
  obs_loc_tax$dc_occurrence_id <- NA_character_ # TODO: Globally unique and persistentt
  obs_loc_tax$dc_basisofrecord <- NA_character_ # TODO: Will be "MachineObservation" or "MumanObservation" depending on sample type
  obs_loc_tax$dc_occurrencestatus <-NA_character_ # TODO: Presence (1) or absence (0) and depends on obundance column. Optional.
  obs_loc_tax$dc_samplingprotocol <-NA_character_ # TODO: The name of, reference to, or description of the method or protocol used during an Event. A string that will be derived from the L1 metadata. Optional.
  obs_loc_tax$dc_samplesizeunit <-NA_character_ 
  obs_loc_tax$dc_samplesizevalue <-NA_character_ 
  # obs_loc_tax$dc_organismquantitytype <-NA_character_ # TODO: The type of quantification system used for the quantity of organisms. Required.
  
  # comb_id 
  # TODO: Form globally unique IDs. See notes.
  obs_loc_tax$dc_occurrence_id <- paste(
    child.package.id,
    seq(nrow(obs_loc_tax)),
    sep = '.')
  
  # occurrence status: choice of present|absent, based on value.
  obs_loc_tax$dc_occurrencestatus[obs_loc_tax$value %in% 0] <- 'absent'
  obs_loc_tax$dc_occurrencestatus[
    obs_loc_tax$value != 0 |
      !is.na(obs_loc_tax$value)] <-'present'
  
  # Use the DOI of the L1 and construct as: "See methods in DOI"
  obs_loc_tax$dc_samplingprotocol <- paste0(
    "See methods in ",
    suppressMessages(
      EDIutils::api_read_data_package_doi(parent.package.id, environment)))
  
  # TODO: Determine if observation was made by an instrument or human. This 
  # info is stored in the L1 EML keywordSet
  
  keywords <- xml2::xml_text(
      xml2::xml_find_all(
        suppressMessages(
          EDIutils::api_read_metadata(parent.package.id, environment)), ".//keyword"))
  
  basis_of_record <- trimws(
    stringr::str_remove(
      keywords[stringr::str_detect(keywords, "basisOfRecord:")],
      "basisOfRecord:"))
  
  if (length(basis_of_record) == 1) {
    obs_loc_tax$dc_basisofrecord <- basis_of_record
  }
  
  # TODO: Determine if these have to be here
  # obs_loc_tax$dc_samplesizevalue
  
  # TODO: Determine if these have to be here
  # obs_loc_tax$dc_samplesizeunit
  
  # Create DF for export 
  occurrence_core <- data.frame(
    occurrenceId = obs_loc_tax$dc_occurrence_id,
    basisOfRecord = obs_loc_tax$dc_basisofrecord,
    locationID = obs_loc_tax$location_id,
    decimalLatitude = obs_loc_tax$latitude,
    decimalLongitude = obs_loc_tax$longitude,
    eventDate = obs_loc_tax$observation_datetime,
    samplingProtocol = obs_loc_tax$dc_samplingprotocol,
    sampleSizeValue = obs_loc_tax$dc_samplesizevalue,
    sampleSizeUnit = obs_loc_tax$dc_samplesizeunit,
    scientificName = obs_loc_tax$taxon_name,
    taxonID = obs_loc_tax$authority_taxon_id,
    nameAccordingTo = obs_loc_tax$authority_system,
    scientificNameID = obs_loc_tax$lsid,
    occurrenceStatus = obs_loc_tax$dc_occurrencestatus,
    organismQuantityType = obs_loc_tax$value,
    measurementUnit = obs_loc_tax$unit,
    stringsAsFactors = FALSE)
  
  return(
    list(occurrence = occurrence_core))
  
}





#' Creates DwC-A tables in event core 
#'
#' @param dwca_occurrence_core_config 
#' @param dwca_occurrence_core_mapping 
#' @param dt_obs 
#' @param dt_obs_ancil 
#' @param dt_loc_ancil 
#' @param dt_loc 
#' @param dt_tax 
#' @param parent.package.id
#'     (character) ID of an ecocomDP data package. Only 
#'     EDI Data Repository package IDs are currently supported.
#' @param child.package.id
#'
#' @return
#'     three tables, event, occurrence, measurementOrFact
#' @export
#'
#' @examples
#' 
create_tables_dwca_event_core <- function(
  dwca_occurrence_core_config,
  dwca_occurrence_core_mapping,
  dt_obs,
  dt_obs_ancil,
  dt_loc_ancil,
  dt_loc,
  dt_tax,
  parent.package.id,
  child.package.id) {
  
  message('Creating DwC-A Event Core tables')
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Validate function inputs --------------------------------------------------
  # Do you have what you need to get these vars? confirm fields used by mapping 
  # table are present. Anything that is required by the ecocomDP model already, 
  # you can assume is present. location_ancillary.value, or 
  # observation_anncillary.value ?? what does this note mean? do I need these?
  
  # Join the tables -----------------------------------------------------------
  
  # TODO: Don't forget locationn ancillary (MOB add issue #). An enhancement.
  obs_loc_tax <- join_obs_loc_tax(
    dt_obs = dt_obs, 
    dt_loc = dt_loc, 
    dt_tax = dt_tax)
  
  # Add column
  # Create computed vectors:
  
  # Define new cols specifically needed for DwC-A
  obs_loc_tax$lsid <- NA_character_ # TODO: This will be an LSID
  obs_loc_tax$dc_occurrence_id <- NA_character_ # TODO: Globally unique and persistentt
  obs_loc_tax$dc_samplingprotocol <-NA_character_ # TODO: The name of, reference to, or description of the method or protocol used during an Event. A string that will be derived from the L1 metadata. Optional.
  obs_loc_tax$dc_event_id <-NA_character_
  obs_loc_tax$dc_dataset_name <-NA_character_
  
  # comb_id 
  # TODO: Form globally unique IDs. See notes.
  obs_loc_tax$dc_occurrence_id <- paste(
    "occ",
    child.package.id,
    seq(nrow(obs_loc_tax)),
    sep = '.')
  
  # TODO: Form globally unique IDs. See notes.
  obs_loc_tax$dc_event_id <- paste(
    child.package.id,
    obs_loc_tax$event_id,
    sep = '.')
  
  # Use the DOI of the L1 and construct as: "See methods in DOI"
  obs_loc_tax$dc_samplingprotocol <- paste0(
    "See methods in ",
    suppressMessages(
      EDIutils::api_read_data_package_doi(parent.package.id, environment)))
  
  # TODO: Determine if observation was made by an instrument or human. This 
  # info is stored in the L1 EML keywordSet
  
  keywords <- xml2::xml_text(
    xml2::xml_find_all(
      suppressMessages(
        EDIutils::api_read_metadata(parent.package.id, environment)), ".//keyword"))
  
  basis_of_record <- trimws(
    stringr::str_remove(
      keywords[stringr::str_detect(keywords, "basisOfRecord:")],
      "basisOfRecord:"))
  
  if (length(basis_of_record) == 1) {
    obs_loc_tax$dc_basisofrecord <- basis_of_record
  } else {
    obs_loc_tax$dc_basisofrecord <- NA_character_
  }

  # datasetName 
  # TODO: Determine method. Maybe use title of L0, because we add note about 
  # ecocomDP in level 1. Could shortname if exists and title if not.
  dc_dataset_name <- "Dataset name"
  
  # Create event --------------------------------------------------------------
  
  event_table <- data.frame(
    id = seq(nrow(obs_loc_tax)),
    eventID = obs_loc_tax$dc_event_id,
    datasetName = dc_dataset_name,
    samplingProtocol = obs_loc_tax$dc_samplingprotocol,
    eventDate = obs_loc_tax$observation_datetime,
    decimalLatitude = obs_loc_tax$latitude,
    decimalLongitude = obs_loc_tax$longitude,
    georeferenceRemarks = obs_loc_tax$location_name,
    stringsAsFactors = FALSE)
  
  # Unique event_table based on all columns
  event_table <- unique.data.frame(
    event_table)
  
  # Create occurrence ---------------------------------------------------------
  
  occurrence_table <- data.frame(
    id = seq(nrow(obs_loc_tax)),
    eventID = obs_loc_tax$dc_event_id,
    occurrenceID = obs_loc_tax$dc_occurrence_id,
    basisOfRecord = "humanObservation",#obs_loc_tax$dc_basisofrecord,
    scientificName = obs_loc_tax$taxon_name,
    taxonID = obs_loc_tax$authority_taxon_id,
    nameAccordingTo = obs_loc_tax$authority_system,
    scientificNameID = obs_loc_tax$lsid,
    stringsAsFactors = FALSE)
  
  # Unique occurrence_table based on all columns except occurrenceID to reduce
  # redundancy
  occurrence_table <- dplyr::distinct_at(
    occurrence_table, 
    .vars = c("eventID", "basisOfRecord", "scientificName", "taxonID",
              "nameAccordingTo", "scientificNameID"),
    .keep_all = TRUE)
  
  # Create extendedmeasurementorfact ------------------------------------------
  
  extendedmeasurementorfact_table <- data.frame(
    id = seq(nrow(obs_loc_tax)),
    eventID = obs_loc_tax$dc_event_id,
    occurrenceID = obs_loc_tax$dc_occurrence_id,
    measurementType = obs_loc_tax$variable_name,
    measurementTypeID = NA_character_, # TODO: the variable mapping annotation (obs_loc_tax$variable_mapping)
    measurementValue = obs_loc_tax$value,
    measurementUnit = obs_loc_tax$unit,
    stringsAsFactors = FALSE)
  
  return(
    list(
      event = event_table,
      occurrence = occurrence_table,
      extendedmeasurementorfact = extendedmeasurementorfact_table))
  
}

