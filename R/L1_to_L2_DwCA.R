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