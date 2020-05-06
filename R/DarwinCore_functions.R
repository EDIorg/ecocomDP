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
  
  # Assemble vectors ----------------------------------------------------------
  # fields in the config are the column headers. number of rows will = that in the L1 obs table.
  # ideally, you would drive this from the mapping tables

  # TODO: Move this function into a new file, perhaps "manipulate_tables.R"
  long2wide_obs_loc_tax <- function(dt_obs, dt_loc, dt_tax) {
    
    # TODO: Unnest location
    
    for (i in 1:length(dt_loc$location_id)) {
      
      if (!is.na(dt_loc$parent_location_id[i])) {
        tid <- dt_loc$parent_location_id[i]
        # TODO: Resume here!
        # browser()
        dt_loc$location_id == tid
        
      }
      
      # cont <- TRUE
      # while (isTRUE(cont)) {
      #   if (i == 2){
      #     browser()
      #   }
      # }
      
    }
    
    
    # Left join observation, location, and taxon tables
    output <- dplyr::left_join(
      dplyr::left_join(
        dt_obs, dt_loc, 
        by = "location_id"), 
      dt_tax, by = "taxon_id")
  }
  
  test <- long2wide_obs_loc_tax(
    dt_obs = dt_obs, 
    dt_loc = dt_loc, 
    dt_tax = dt_tax)
  
    
  # Direct pulls: from obs table. their length will set nrows of final table.  
  occ_eventDate            <- dt_obs$observation_datetime	
  occ_OrganismQuantityType <- dt_obs$variable_name	
  occ_organismQuantity     <- dt_obs$value	
  
  # computed vectors:
  
  
  # order the cols in a data frame
  occurrence_core <- data.frame(occ_eventDate, occ_OrganismQuantityType, occ_organismQuantity)
  
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

