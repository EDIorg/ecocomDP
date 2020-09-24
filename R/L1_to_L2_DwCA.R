#' Creates Darwin Core Archive package from an ecocomDP formatted level-1
#'
#' @param core.name
#'     (character) The Darwin Core central table of the package. Can be: 
#'     "occurence" (occurrence core) or "event" (event core).
#' @param L1.id 
#'     (character) Package ID of an ecocomDP dataset from PASTA. Other level-1 
#'     types TBD.
#' @details
#'     TODO: Add details
#' @return
#'     Data entities in either the occurrence or event core formats with a
#'     corresponding EML metadata record and a meta.xml file.
#' @export
#'
#' @examples
#' 
L1_to_L2_DwCA <- function(core.name, L1.id) {
  
  # Config script -------------------------------------------------------------
  # Eventually will support 2 DwC types: choose occurence-core or event-core. 
  # occurence is simpler (sightings), event is a better fit for most of our data
  # TODO: Support event-core
  
  parent_path <- "."
  dwca_config_file <- readr::read_csv(system.file("dwca_occurrence_core_config.csv", package = "ecocomDP"))
  dwca_mappings_file <- readr::read_csv(system.file("dwca_occurrence_core_mappings.csv", package = "ecocomDP"))

  # msg the user
  message(
    'Using config files at: ', 
    dirname(system.file("dwca_occurrence_core_config.csv", package = "ecocomDP")))
  message('Making ', core.name)
  
  # Load data -----------------------------------------------------------------
  # TODO: write a function that reads pkg metadata and dataTables. copy 
  # something from one of the L0 conversion scripts.
  
  source(system.file("/edi_193_3.R", package = "ecocomDP"))
  
  
  
  # assign tables to a var for that type (read pkg function will use their 
  # actual names, assign based on known str frag)
  # TODO: Use the entity name to determine which of the ecocomDP tables that 
  # entity actually is.
  
  dt_obs <- dt1
  dt_obs_ancil <- dt2
  dt_loc_ancil <- dt3
  dt_tax_ancil <- dt4
  dt_summary <- dt5
  dt_loc <- dt6
  dt_tax <- dt7
  # dt_var_mapping <- ___
  
  # Convert tables ------------------------------------------------------------
  
  if (core.name == 'occurrence') {
    
    # call a function to create occurrence core. inputs: data objects, dwca_mappings, dwca_config
    message('calling function to create DwC-A, occurrence core')
    
    if (exists("dt_loc_ancil")) {
      occurrence_table <- create_table_dwca_occurrence_core(
        dwca_occurrence_core_config = dwca_config_file,
        dwca_occurrence_core_mapping = dwca_mappings_file,
        dt_obs = dt_obs,
        dt_loc = dt_loc,
        dt_tax = dt_tax,
        dt_loc_ancil = dt_loc_ancil)
    } else {
      occurrence_table <- create_table_dwca_occurrence_core(
        dwca_occurrence_core_config = dwca_config_file,
        dwca_occurrence_core_mapping = dwca_mappings_file,
        dt_obs = dt_obs,
        dt_loc = dt_loc,
        dt_tax = dt_tax)
    }
    
  } else if (core.name == 'event') {
    
    # call a function to create event core. inputs: data objects, dwca_mappings, dwca_config  
    # print('calling function to create DwC-A, event core')
    event_table_list <- create_table_dwca_occurrence_core(
      dwca_occurrence_core_config = dwca_config_file,
      dwca_occurrence_core_mapping = dwca_mappings_file,
      dt_obs = dt_obs,
      dt_loc = dt_loc,
      dt_tax = dt_tax,
      dt_loc_ancil = dt_loc_ancil,
      dt_obs_ancil = dt_obs_ancil)
    
  }
  
  # Export tables -------------------------------------------------------------
  
  # 5. construct EML 
  # again, reuse functions from L1 build
  
  
}







#' Function creates DwC-A tables in occurrence core 
#'
#' @param dwca_occurrence_core_config 
#' @param dwca_occurrence_core_mapping 
#' @param dt_obs 
#' @param dt_loc 
#' @param dt_tax 
#' @param dt_loc_ancil 
#'
#' @return
#' @export
#'
#' @examples
create_table_dwca_occurrence_core <- function(
  dwca_occurrence_core_config,
  dwca_occurrence_core_mapping,
  dt_obs,
  dt_loc,
  dt_tax,
  dt_loc_ancil = NULL) {
  
  # Validate function inputs --------------------------------------------------
  # Do you have what you need to get these vars? confirm fields used by mapping table are present.
  # anything that is required by the ecocomDP model already, you can assume is present.
  # location_ancillary.value, or observation_anncillary.value ?? what does this note mean? do I need these?
  
  if (!is.null(dt_loc_ancil)) {
    # If dt_loc_ancil exists, then the occurence table can have sampleValueUnit
  }
  
  # Join the tables -----------------------------------------------------------
  # TODO: Don't forget locationn ancillary
  
  obs_loc_tax <- long2wide_obs_loc_tax(
    dt_obs = dt_obs, 
    dt_loc = dt_loc, 
    dt_tax = dt_tax)
  
  # Add column
  # Create computed vectors:
  
  # Define new cols specifically needed for DwC-A
  obs_loc_tax$lsid <- NA_character_
  obs_loc_tax$comb_id <- NA_character_
  obs_loc_tax$dc_basisofrecord <- NA_character_
  obs_loc_tax$dc_occurrencestatus <-NA_character_
  obs_loc_tax$dc_samplingprotocol <-NA_character_
  obs_loc_tax$dc_quantitytype <-NA_character_
  
  
  # comb_id 
  # TODO: globally unique, see notes.
  obs_loc_tax$comb_id <- paste(sep='.', obs_loc_tax$package_id,
                               obs_loc_tax$observation_id,
                               obs_loc_tax$location_id,
                               obs_loc_tax$event_id,
                               obs_loc_tax$taxon_id,
                               obs_loc_tax$observation_datetime)
  
  # occurrence status: choice of present|absent, based on value.
  if (obs_loc_tax$value == 0) {
    obs_loc_tax$dc_occurrencestatus <- 'absent'
  } else {
    obs_loc_tax$dc_occurrencestatus <-'present'
  }
  
  # sampling protocol: string description of the method. ouch.
  obs_loc_tax$dc_samplingprotocol <- 'ad hoc observation'
  
  
  # TODO: not all will be human obs.  need logic to determine if this is an instrument or not
  obs_loc_tax$dc_basisofrecord <- 'HumanObservation'
  
  # TODO: not all will be counts of individuals.
  obs_loc_tax$dc_quantitytype <-'individuals'
  
  # resume here:
  # browser()
  
  
  # Create DF for export 
  # TODO: proper headers. probably want the mapping table for this.
  occurrence_core <- dplyr::select(obs_loc_tax, comb_id,
                                   dc_basisofrecord,
                                   dc_occurrencestatus,
                                   location_id,
                                   latitude,
                                   longitude,
                                   dc_samplingprotocol,
                                   observation_datetime,
                                   dc_samplingprotocol,
                                   taxon_name,
                                   authority_system,
                                   authority_taxon_id,
                                   taxon_id,
                                   lsid,
                                   dc_quantitytype,
                                   value
  )
  
  
  # send it back.
  return(occurrence_core)
  
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
  dt_tax) {
  
  # validate ecocomDP. do you have what you need to get these vars? confirm fields used by mapping table are present.
  
  # create 3 tables: fields in the config are the column headers. 
  
  string1<-'hello world'
  return(string1)
  
  # return(event)
  # return(occurrence)
  # return(measurementOrFact)
  
  # return(
  #   list(
  #     event = event_df,
  #     occurrence = occurrence_df,
  #     measurementOrFact = measurementOrFact_df))
  
}

