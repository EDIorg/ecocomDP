library(ecocomDP)
library(tidyverse)






# read in data using ecocomDP
all_data <- ecocomDP::read_data(
    id = "DP1.20120.001",
    site= c('COMO','LECO'), 
    startdate = "2019-06",
    enddate = "2019-09",
    token = Sys.getenv("NEON_TOKEN"))






# flatten_tables <- reactive({
  
  # all_data <- pulled_data()
  
# helper function to determine if a col is all NAs
# returns boolean
not_all_NAs <- function(x)!all(is.na(x))

# merge observation table with taxon table
all_merged <- all_data[[1]]$tables$observation %>%
  dplyr::select_if(not_all_NAs) %>%
  dplyr::left_join(
    all_data[[1]]$tables$taxon %>% dplyr::select_if(not_all_NAs), 
    by = "taxon_id")

# merge all_merged with observation_ancillary
if("observation_ancillary" %in% names(all_data[[1]]$tables)){
  observation_ancillary <- all_data[[1]]$tables$observation_ancillary %>%
    dplyr::select(-observation_ancillary_id) %>%
    tidyr::pivot_wider(names_from = variable_name, values_from = value) %>%
    dplyr::select_if(not_all_NAs)
  
  all_merged <- all_merged %>%
    dplyr::left_join(observation_ancillary, 
                     by = "event_id",
                     suffix = c("", "_observation_ancillary"))
}


# merge location data and using "NEON location type" from location ancillary data
if("location_ancillary" %in% names(all_data[[1]]$tables) && 
   "NEON location type" %in% all_data[[1]]$tables$location_ancillary$variable_name){
  
  # merge location and location_ancillary
  location <- all_data[[1]]$tables$location %>%
    dplyr::select_if(not_all_NAs) %>%
    dplyr::left_join(
      all_data[[1]]$tables$location_ancillary %>%
        dplyr::filter(variable_name == "NEON location type") %>%
        dplyr::select(location_id, value) %>%
        rename(location_type = value))
  
  # extract a list of the location types
  location_type_list <- location$location_type %>% unique()
  
  # identify ultimate parents... i.e., records with no parents
  location_parent <- location %>% filter(is.na(parent_location_id)) %>%
    select(location_id, location_type)
  
  # identify all records that have parents
  location_child <- location %>% filter(!is.na(parent_location_id))
  
  # loop through from ultimate parents down, joining with child records, until there are no more children
  while(nrow(location_child) > 0){
    
    # inner join parents to children
    location_new <- location_child %>% inner_join(
      location_parent %>% rename(location_type_parent = location_type),
      by = c("parent_location_id" = "location_id"))
    
    # rename parent location id column by location type
    new_parent_location_id_name <- location_new$location_type_parent[1]
    names(location_new)[names(location_new)=="parent_location_id"] <- new_parent_location_id_name
    location_new <- location_new %>% select(-location_type_parent)
    
    # see if there are any children left that have not been joined to parents
    location_child <- location_child %>%
      filter(!location_id %in% location_new$location_id)
    
    # if there are still children, re-assign parent table
    if (nrow(location_child) > 0){
      location_parent <- location_new[,names(location_new) %in% 
                                        c("location_id", "location_type", location_type_list)]
    }else{
      
      # otherwise, clean up new location table and return as output
      location_new <- location_new %>% select(-location_type)
    }
  }
  
  all_merged <- all_merged %>%
    left_join(
      location_new, 
      by = "location_id",
      suffix = c("", "_location"))
  
}else{
  #otherwise, just merge location table with all_merged
  all_merged <- all_merged %>%
    dplyr::left_join(
      all_data[[1]]$tables$location %>%
        dplyr::select_if(not_all_NAs),
      by = "location_id",
      suffix = c("", "_location"))
}
  

# merge taxon_ancillary
if("taxon_ancillary" %in% names(all_data[[1]]$tables)){
  taxon_ancillary <- all_data[[1]]$tables$taxon_ancillary %>% 
    tidyr::pivot_wider(names_from = variable_name, values_from = value)
  all_merged <- all_merged %>%
    left_join(
      taxon_ancillary %>% 
        dplyr::select(-taxon_ancillary_id) %>% 
        dplyr::select_if(not_all_NAs),
      by = "taxon_id",
      suffix = c("", "_taxon_ancillary"))
}


# merge other location ancillary data
if("location_ancillary" %in% names(all_data[[1]]$tables) &&
   length(
     dplyr::setdiff("NEON location type", 
                    unique(all_data[[1]]$tables$location_ancillary$variable_name))) > 0){
  location_ancillary <- all_data[[1]]$tables$location_ancillary %>%
    # dplyr::filter(variable_name != "NEON location type") %>%
    dplyr::select(-location_ancillary_id) %>%
    tidyr::pivot_wider(names_from = variable_name, values_from = value)
    
  all_merged <- all_merged %>%
    dplyr::left_join(
      location_ancillary %>% 
        dplyr::select_if(not_all_NAs),
      by = "location_id",
      suffix = c("", "_location_ancillary"))
}

return(all_merged)
  
}) # end of flatten_tables