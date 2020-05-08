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
  
  # browser()
  # Resume dev here ...
  
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
  # ID should be globally unique. so create a composite ID from other IDs and date,
  # presuming that this string won't collide with something else. 
  # TODO: find out how GBIF uses the ID. if it has a role in replacing records, you will need to rethink this
  # e.g., not use the internal id at all (which might change) and instead use the static part of package id and sample date.
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
#' (dwca_event_table, dwca_occurrence_table, dwca_measurementOrFact_table) <- create_table_dwca_event_core(
#'  a bunch of inputs here.)    
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

