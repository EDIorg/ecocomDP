# this is a test! BP for functions.
# probably you will want to put all the functions into one file. name it functions_ecocomDP_to_DwCA.R

# Function creates DwC-A tables in occurrence core 
# inputs:
# dwca_mappings, dwca_config, ecocomDP data objects, 
# usage: dwca_occurrence_table <- create_table_dwca_occurrence_core(
#  a bunch of inputs here.
#    
#)

func_create_table_dwca_occurrence_core <- function(
  dwca_occurrence_core_config,
  dwca_occurrence_core_mapping,
  dt_obs,
  dt_loc,
  dt_tax
#  dt_obs_ancil,
#  dt_loc_ancil,
#  dt_tax_ancil,
#  dt_summary,

)
{
  # 1. validate ecocomDP. do you have what you need to get these vars? confirm fields used by mapping table are present.
  # anything that is required by the ecocomDP model already, you can assume is present.
  # location_ancillary.value, or observation_anncillary.value ?? what does this note mean? do I need these?
  
  # 2. Assemble vectors 
  # fields in the config are the column headers. number of rows will = that in the L1 obs table.
  # ideally, you would drive this from the mapping tables
  
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

