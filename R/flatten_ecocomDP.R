#' Merge tables in ecocomDP dataset
#'
#' @description  
#'     This function joins a list of data.frames following the ecocomDP data pattern
#' 

#' Check for valid date and time formats
#' 
#' @param data.list
#'     (list of data frames) A named list of data frames, each of which is an 
#'     ecocomDP table.
#'
#' @return 
#'     (data.frame) a single data.frame of merged tables for an ecocomDP data package
#'
#' @examples 
#' \dontrun{
#' # Flatten NEON data
#' all_data <- ecocomDP::read_data(
#' id = "DP1.20120.001",
#' site= c('COMO','LECO'),
#' startdate = "2019-06",
#' enddate = "2019-09",
#' token = Sys.getenv("NEON_TOKEN"))
#' 
#' my_list_of_tables <- all_data[[1]]$tables
#' flat_data <- ecocomDP::flatten_ecocomDP(my_list_of_tables)
#' 
#' # Flatten EDI data
#' all_data <- ecocomDP::read_data("edi.290.1")
#' my_list_of_tables <- all_data[[1]]$tables
#' flat_data <- ecocomDP::flatten_ecocomDP(my_list_of_tables)
#' }
#' 
#' @export

flatten_ecocomDP <- function(data.list){
  
  # helper function to determine if a col is all NAs
  # returns boolean
  not_all_NAs <- function(x)!all(is.na(x))
  
  dup_fxn <- function(x){
    x <- unlist(x)
    if(class(x) == "numeric"){
      message(paste0("WARNING: duplicate values observed for during pivot_wider"))
      return(mean(x, na.rm = TRUE))
    }else{
      return(paste(x, collapse = " | "))
    }
  }
  
  
  # merge observation table with taxon table
  all_merged <- data.list$observation %>%
    dplyr::select_if(not_all_NAs) %>%
    dplyr::left_join(
      data.list$taxon %>% dplyr::select_if(not_all_NAs), 
      by = "taxon_id")
  
  
  # merge all_merged with observation_ancillary
  if("observation_ancillary" %in% names(data.list)){
    observation_ancillary_long <- data.list$observation_ancillary 
    
    observation_ancillary_long_wide <- observation_ancillary_long[
      ,!names(observation_ancillary_long) %in% c("observation_ancillary_id","unit")] %>%
      tidyr::pivot_wider(
        names_from = variable_name, 
        values_from = value,
        values_fn = dup_fxn) %>%
      dplyr::select_if(not_all_NAs)
    
    all_merged <- all_merged %>%
      dplyr::left_join(observation_ancillary_long_wide, 
                       by = "event_id",
                       suffix = c("", "_observation_ancillary"))
  }
  
  
  # merge location data and using "NEON location type" from location ancillary data
  if("location_ancillary" %in% names(data.list) && 
     "NEON location type" %in% data.list$location_ancillary$variable_name){
    
    # merge location and location_ancillary
    location <- data.list$location %>%
      dplyr::select_if(not_all_NAs) %>%
      dplyr::left_join(
        data.list$location_ancillary %>%
          dplyr::filter(variable_name == "NEON location type") %>%
          dplyr::select(location_id, value) %>%
          dplyr::rename(location_type = value))
    
    # extract a list of the location types
    location_type_list <- location$location_type %>% unique()
    
    # identify ultimate parents... i.e., records with no parents
    location_parent <- location %>% 
      dplyr::filter(is.na(parent_location_id)) %>%
      dplyr::select(location_id, location_type)
    
    # identify all records that have parents
    location_child <- location %>% 
      dplyr::filter(!is.na(parent_location_id))
    
    

    
    # loop through from ultimate parents down, joining with child records, until there are no more children
    while(nrow(location_child) > 0){
      
      # inner join parents to children
      location_new <- location_child %>% 
        dplyr::inner_join(
        location_parent %>% 
          dplyr::rename(location_type_parent = location_type),
        by = c("parent_location_id" = "location_id"))
      
      # rename parent location id column by location type
      new_parent_location_id_name <- location_new$location_type_parent[1]
      names(location_new)[names(location_new)=="parent_location_id"] <- new_parent_location_id_name
      location_new <- location_new %>% 
        dplyr::select(-location_type_parent)
      
      # see if there are any children left that have not been joined to parents
      location_child <- location_child %>%
        dplyr::filter(!location_id %in% location_new$location_id)
      
      # if there are still children, re-assign parent table
      if (nrow(location_child) > 0){
        location_parent <- location_new[,names(location_new) %in% 
                                          c("location_id", "location_type", location_type_list)]
      }else{
        
        # otherwise, clean up new location table and return as output
        location_new <- location_new %>% 
          dplyr::select(-location_type)
      }
    }
    

    all_merged <- all_merged %>%
      dplyr::left_join(
        location_new, 
        by = "location_id",
        suffix = c("", "_location"))
    
  }else{
    #otherwise, just merge location table with all_merged
    all_merged <- all_merged %>%
      dplyr::left_join(
        data.list$location %>%
          dplyr::select_if(not_all_NAs),
        by = "location_id",
        suffix = c("", "_location"))
  }
  

  
  
  # merge taxon_ancillary
  if("taxon_ancillary" %in% names(data.list)){
    taxon_ancillary <- data.list$taxon_ancillary %>% 
      tidyr::pivot_wider(names_from = variable_name, values_from = value)
    all_merged <- all_merged %>%
      dplyr::left_join(
        taxon_ancillary %>% 
          dplyr::select(-taxon_ancillary_id) %>% 
          dplyr::select_if(not_all_NAs),
        by = "taxon_id",
        suffix = c("", "_taxon_ancillary"))
  }
  

  
  # merge other location ancillary data
  if("location_ancillary" %in% names(data.list) &&
     length(
       dplyr::setdiff("NEON location type", 
                      unique(data.list$location_ancillary$variable_name))) > 0){
    location_ancillary <- data.list$location_ancillary %>%
      # dplyr::filter(variable_name != "NEON location type") %>%
      dplyr::select(-location_ancillary_id) %>%
      tidyr::pivot_wider(
        names_from = variable_name, 
        values_from = value)
    
    all_merged <- all_merged %>%
      dplyr::left_join(
        location_ancillary %>% 
          dplyr::select_if(not_all_NAs),
        by = "location_id",
        suffix = c("", "_location_ancillary"))
  }
  
  return(all_merged)
  
} # end of flatten_ecocomDP



# # read in data using ecocomDP
# all_data <- ecocomDP::read_data(
#   id = "DP1.20120.001",
#   site= c('COMO','LECO'), 
#   startdate = "2019-06",
#   enddate = "2019-09",
#   token = Sys.getenv("NEON_TOKEN"))
# 
# all_data <- ecocomDP::read_data("edi.290.1")
# 
# data.list <- all_data[[1]]$tables