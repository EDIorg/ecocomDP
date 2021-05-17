#' Join all tables of an ecocomDP dataset and spread into wide format
#' 
#' @param tables (list) A named list of ecocomDP tables (as data.frames).
#'
#' @return (data.frame) A single wide table created by joining and spreading all \code{tables}.
#' 
#' @export
#'
#' @examples 
#' \dontrun{
#' # Flatten NEON data
#' all_data <- read_data(
#' id = "DP1.20120.001",
#' site= c('COMO','LECO'),
#' startdate = "2019-06",
#' enddate = "2019-09",
#' token = Sys.getenv("NEON_TOKEN"))
#' 
#' my_list_of_tables <- all_data[[1]]$tables
#' flat_data <- flatten_data(my_list_of_tables)
#' 
#' # Flatten EDI data
#' all_data <- read_data("edi.290.1")
#' my_list_of_tables <- all_data[[1]]$tables
#' flat_data <- flatten_data(my_list_of_tables)
#' }
#' 
flatten_data <- function(tables){
  
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
  all_merged <- tables$observation %>%
    dplyr::select_if(not_all_NAs) %>%
    dplyr::left_join(
      tables$taxon %>% dplyr::select_if(not_all_NAs), 
      by = "taxon_id")
  
  
  # merge all_merged with observation_ancillary
  if("observation_ancillary" %in% names(tables)){
    
    #handle missing observation_id or case where all observation_id is na
    if(!"observation_id" %in% names(tables$observation_ancillary) || 
       all(is.na(tables$observation_ancillary$observation_id)) ){
      warning("'observation_id' is missing from the observation_ancillary table")
    
    #the expected case:
    }else{
      
      observation_ancillary_long <- tables$observation_ancillary 
      
      observation_ancillary_long_wide <- observation_ancillary_long[
        ,!names(observation_ancillary_long) %in% c("observation_ancillary_id","unit")] %>%
        tidyr::pivot_wider(
          names_from = variable_name, 
          values_from = value,
          values_fn = dup_fxn) %>%
        dplyr::select_if(not_all_NAs)
      
      all_merged <- all_merged %>%
        dplyr::left_join(observation_ancillary_long_wide, 
                         by = "observation_id",
                         suffix = c("", "_observation_ancillary"))
    }
  }
  
  
  # merge location data and using "NEON location type" from location ancillary data
  if("location_ancillary" %in% names(tables) && 
     "NEON location type" %in% tables$location_ancillary$variable_name){
    
    # merge location and location_ancillary
    location <- tables$location %>%
      dplyr::select_if(not_all_NAs) %>%
      dplyr::left_join(
        tables$location_ancillary %>%
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
        tables$location %>%
          dplyr::select_if(not_all_NAs),
        by = "location_id",
        suffix = c("", "_location"))
  }
  

  # merge additional location_ancillary data
  # only for bottom level when locations are nested
  if("location_ancillary" %in% names(tables)){
    location_ancillary <- tables$location_ancillary %>%
      dplyr::filter(variable_name!="NEON location type",
                    location_id %in% all_merged$location_id)
    
    if(nrow(location_ancillary) > 0){
      location_ancillary_wide <- location_ancillary %>%
        dplyr::select(location_id, variable_name, value) %>%
        tidyr::pivot_wider(names_from = variable_name, values_from = value)
      all_merged <- all_merged %>%
        dplyr::left_join(
          location_ancillary_wide %>% 
            dplyr::select_if(not_all_NAs),
          by = "location_id",
          suffix = c("", "_location_ancillary"))
    }
  }
  
  
  
  # merge taxon_ancillary
  
  

  if("taxon_ancillary" %in% names(tables)){
    taxon_ancillary <- tables$taxon_ancillary %>% 
      dplyr::select(taxon_id, variable_name, value) %>%
      tidyr::pivot_wider(names_from = variable_name, values_from = value)
    all_merged <- all_merged %>%
      dplyr::left_join(
        taxon_ancillary %>% 
          dplyr::select_if(not_all_NAs),
        by = "taxon_id",
        suffix = c("", "_taxon_ancillary"))
  }
  
  
  

  # merge other location ancillary data
  if("location_ancillary" %in% names(tables) &&
     length(
       dplyr::setdiff("NEON location type", 
                      unique(tables$location_ancillary$variable_name))) > 0){
    location_ancillary <- tables$location_ancillary %>%
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
  
} # end of flatten



# # read in data using ecocomDP
# all_data <- read_data(
#   id = "DP1.20120.001",
#   site= c('COMO','LECO'), 
#   startdate = "2019-06",
#   enddate = "2019-09",
#   token = Sys.getenv("NEON_TOKEN"))
# 
# all_data <- read_data("edi.290.1")
# 
# tables <- all_data[[1]]$tables