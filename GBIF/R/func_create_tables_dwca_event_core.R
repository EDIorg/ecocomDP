# this is a test! BP for functions.

# FUnction creates DwC-A tables in event core 
# inputs:
# dwca_mappings, dwca_config, ecocomDP data objects, 
# usage: (dwca_event_table, dwca_occurrence_table, dwca_measurementOrFact_table) <- create_table_dwca_event_core(
#  a bunch of inputs here.
# )    
# OUtputs: three tables, event, occurrence, measurementOrFact

func_create_table_dwca_occurrence_core <- function(
  dwca_occurrence_core_config,
  dwca_occurrence_core_mapping
 # dt_obs,
#  dt_obs_ancil,
#  dt_loc_ancil,
#  dt_tax_ancil,
#  dt_summary,
#  dt_loc,
#  dt_tax
)
{
  
  # validate ecocomDP. do you have what you need to get these vars? confirm fields used by mapping table are present.
  
  # create 3 tables: fields in the config are the column headers. 
  
  
  
  string1<-'hello world'
  return(string1)
  
  # return(event)
  # return(occurrence)
  # return(measurementOrFact)
  
 
  
  
}