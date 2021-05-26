#' Join all tables of an ecocomDP dataset and spread into flat format
#' 
#' @param tables (list) A named list of ecocomDP tables (as data.frames).
#'
#' @return (data.frame) A single flat table created by joining and spreading all \code{tables}.
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
flatten_data <- function(tables) {
  
  # Merge observation table with taxon table
  all_merged <- tables$observation %>%
    dplyr::select_if(not_all_NAs) %>%
    dplyr::left_join(
      tables$taxon %>% dplyr::select_if(not_all_NAs),
      by = "taxon_id")
  
  # Merge observation_ancillary
  if("observation_ancillary" %in% names(tables)){
    
    #handle missing observation_id or case where all observation_id is na
    if(!"observation_id" %in% names(tables$observation_ancillary) || 
       all(is.na(tables$observation_ancillary$observation_id)) ){
      warning("'observation_id' is missing from the observation_ancillary table")
    
    #the expected case:
    }else{
      observation_ancillary_wide <- flatten_ancillary(tables$observation_ancillary)
      all_merged <- all_merged %>%
        dplyr::left_join(observation_ancillary_wide, 
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
    
    if (nrow(location_ancillary) > 0) {
      location_ancillary_wide <- flatten_ancillary(location_ancillary)
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
    taxon_ancillary_wide <- flatten_ancillary(tables$taxon_ancillary)
    all_merged <- all_merged %>%
      dplyr::left_join(
        taxon_ancillary_wide %>%
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
  
  # Merge dataset_summary
  if ("dataset_summary" %in% names(tables)) {
    dataset_summary <- tables$dataset_summary
    all_merged <- all_merged %>%
      dplyr::left_join(dataset_summary, by = "package_id")
  }
  
  # TODO Coerce column classes. Often a numeric type has been stored as character in the long L1 tables. But some columns (primarily ids) should remain character
  
  return(all_merged)
  
}








#' Flatten ancillary table
#'
#' @param ancillary_table (data.frame) Ancillary table to flatten. Can be: observation_ancillary, location_ancillary, or taxon_ancillary.
#'
#' @return (data.frame) A flattened version of \code{ancillary_table} with units added following the unit_<variable_name> convention, and sorted so the unit immediately follows its corresponding variable_name.
#' 
flatten_ancillary <- function(ancillary_table) {
  # Spread on variable_name and value, then add units later
  fkey <- stringr::str_subset(colnames(ancillary_table), "(?<!ancillary_)id") # Use regexpr to select id column to later join on
  auth <- ifelse("author" %in% colnames(ancillary_table), "author", list(NULL))
  vars <- c(fkey, "variable_name", "value", unlist(auth))
  df <- ancillary_table %>% 
    dplyr::select(vars) %>%
    tidyr::pivot_wider(names_from = variable_name, 
                       values_from = value)
  # Add units
  unts <- ancillary_table$unit
  if (!is.null(unts) & not_all_NAs(unts)) {
    unts_df <- ancillary_table %>% # Get units
      dplyr::select(variable_name, unit) %>%
      dplyr::distinct() %>%
      na.omit() %>% 
      tidyr::pivot_wider(names_from = variable_name, values_from = unit)
    # Rename for sorting
    colnames(unts_df) <- paste0(colnames(unts_df), "_unit")
    # Append to flattened df
    df <- dplyr::bind_cols(df, unts_df)
    colnames(df)
    # Sort so units are adjacent their variable
    df <- df %>%  
      dplyr::select(order(colnames(df)))
    # Rename following convention
    cols <- colnames(df)
    i <- stringr::str_detect(colnames(df), "_unit")
    varnames <- stringr::str_remove_all(cols[i], "_unit")
    cols[i] <- paste0("unit_", varnames)
    colnames(df) <- cols
  }
  df <- df %>% dplyr::select_if(not_all_NAs)
  return(df)
}








#' Determine if a col is all NAs
#'
#' @param x A vector of values
#'
#' @return (logical) TRUE if not all NAs, otherwise FALSE
#' 
not_all_NAs <- function(x) {
  !all(is.na(x))
}








#' Helper to handle duplicates
#'
#' @param x A vector of values
#'
#' @return A vector of values, averaged or concatenated
#' 
#' @details Helper function to handle duplicates within the observation_ancillary table. Modifications to the ecocomDP model and corresponding validation checks prohibit duplicates within ancillary tables, but this function handles instances where a user flattens data that could not otherwise be flattened.
#'
dup_fxn <- function(x){
  x <- unlist(x)
  if(class(x) == "numeric"){
    message(paste0("WARNING: duplicate values observed for during pivot_wider. Duplicates will be averaged."))
    return(mean(x, na.rm = TRUE))
  }else{
    message(paste0("WARNING: duplicate values observed for during pivot_wider. Duplicates will be concatenated and pipe delimited."))
    return(paste(x, collapse = " | "))
  }
}
