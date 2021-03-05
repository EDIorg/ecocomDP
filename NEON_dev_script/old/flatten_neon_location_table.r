library(tidyverse)
library(ecocomDP)

# read macroinvert data
my_result <- ecocomDP::read_data(
  id = "DP1.20120.001", 
  site= c('COMO','LECO'),
  token = Sys.getenv("NEON_TOKEN"), #code to use API token if you have it saved as sys env var
  check.size = FALSE)


# function to flatten neon location table in ecocomDP
flatten_neon_location_table <- function(location, location_ancillary){
  
  # join with location type info from ancillary table
  location <- location %>% 
    dplyr::left_join(
      location_ancillary %>%
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
      location_parent <- location_new[,names(location_new) %in% c("location_id", "location_type", location_type_list)]
    }else{
      
      # otherwise, clean up new location table and return as output
      location_new <- location_new %>% select(-location_type)
    }
  }
  
  return(location_new)
}


# example usage
flat_location_table <- flatten_neon_location_table(
  location = my_result[[1]]$tables$location,
  location_ancillary = my_result[[1]]$tables$location_ancillary)
