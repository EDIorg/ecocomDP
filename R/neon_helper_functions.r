# loc_info <- table_location_raw
# loc_col_names <- c("domainID", "siteID", "namedLocation")
# ancillary_var_names <- c("namedLocation",
#   "plotType", "nlcdClass", "geodeticDatum")

#####################################################################################
# helper function to make neon location tables with appropriate nexted references
# not to be exported
#########################################
make_neon_location_table <- function(loc_info, loc_col_names){
  neon_site_list <- data.table::fread(
    system.file("extdata", "neon-field-sites.csv", package = "ecocomDP"))
  
   
  #populate location_id
  if("namedLocation" %in% names(loc_info)) loc_info <- loc_info %>%
      dplyr::mutate(location_id = namedLocation)
  
  
  # # make location table
  table_location <- suppressMessages(
    create_location_deprecated(
      L0_flat = loc_info,
      location_id = "location_id",
      location_name = loc_col_names)) %>%
    dplyr::select(-c(
       .data$latitude,
       .data$longitude,
       .data$elevation))
  
  # get spatial data for lowest resolution location_id
  spatial_data <- loc_info %>%
    dplyr::select(
      .data$location_id,
      .data$decimalLatitude,
      .data$decimalLongitude,
      .data$elevation) %>% 
    dplyr::rename(
      latitude = .data$decimalLatitude,
      longitude = .data$decimalLongitude)
  
  # merge spatial data into table_location
  table_location <- table_location %>%
    dplyr::left_join(spatial_data, by = "location_id")
  

  # code to handle updated create_location (updated 18 Sep 2020 in create_location branch)
  # this code no longer necessary as of 23 July 2021, but leaving in just in case
  if("list" %in% class(table_location) &&
     "location" %in% names(table_location)){
    
    table_location <- table_location$location %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        location_name = location_name %>% 
          strsplit("=") %>%
          unlist() %>%
          dplyr::last()) 
  }
  

    
  # replace parent_id with parent_name
  table_location$parent_location_id <- table_location$location_name[
    match(table_location$parent_location_id, table_location$location_id)]
  
  # replace loc_id with meaningful names
  table_location$location_id <- table_location$location_name
  
  # get neon location info lookup tables
  neon_domain_list <- neon_site_list %>%
    dplyr::select(`Domain Number`, `Domain Name`) %>%
    dplyr::distinct()
  
  neon_site_info_list <- neon_site_list %>%
    dplyr::select(-c(`Domain Number`,`Domain Name`)) %>%
    dplyr::distinct()
  
  
  # update location_names and lat longs where possible
  for(location_id in table_location$location_id){
    if(location_id %in% neon_domain_list$`Domain Number`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_domain_list$`Domain Name`[neon_domain_list$`Domain Number`==location_id]
    }else if(location_id %in% neon_site_info_list$`Site ID`){
      table_location$location_name[table_location$location_id == location_id] <- 
        neon_site_info_list$`Site Name`[neon_site_info_list$`Site ID`==location_id]
      
      table_location$latitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Latitude[neon_site_info_list$`Site ID`==location_id]
      
      table_location$longitude[table_location$location_id == location_id] <- 
        neon_site_info_list$Longitude[neon_site_info_list$`Site ID`==location_id]
      
    }
  }
  

  return(table_location)
}


#####################################################################################
# helper function to make neon ancillary location tables
#########################################
make_neon_ancillary_location_table <- function(loc_info, loc_col_names, ancillary_var_names = NULL){
  # make ancillary table that indicates the location type 
  table_location_ancillary <- loc_info %>% 
    dplyr::select_at(dplyr::vars(loc_col_names)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "value",
      values_to = "location_id") %>%
    dplyr::mutate(
      variable_name = "NEON location type",
      location_ancillary_id = paste0("NEON_location_type_",location_id)) %>%
    dplyr::distinct()
  
  # add other ancillary variables to the long-format table
  if(!is.null(ancillary_var_names)){
    table_location_ancillary <- loc_info %>% 
      dplyr::select_at(dplyr::vars(ancillary_var_names)) %>%
      tidyr::pivot_longer(
        cols = -namedLocation,
        names_to = "variable_name",
        values_to = "value") %>%
      dplyr::rename(location_id = namedLocation) %>%
      dplyr::mutate(
        location_ancillary_id = paste0(variable_name, "_", location_id)) %>%
      dplyr::distinct() %>%
      dplyr::bind_rows(., table_location_ancillary)
  }
  
  return(table_location_ancillary)
}


#####################################################################################
# helper function to make neon ancillary observation tables
#########################################

make_neon_ancillary_observation_table <- function(
  obs_wide, #must have "observation_id" field
  ancillary_var_names = NULL){
  # convert cols that are not char or numeric to char
  obs_wide <- as.data.frame(obs_wide)
  cols_2_change <- which(
    !lapply(X=as.list(obs_wide), FUN=class) %in% c("character", "numeric"))
  if(length(cols_2_change)>0){
    for(i in cols_2_change){
      obs_wide[,i] <- as.character(obs_wide[,i])
    }
  }
  
  col_names_to_keep <- c(
    "observation_id", ancillary_var_names)
  
  
  table_observation_ancillary <- obs_wide[
    ,names(obs_wide) %in% col_names_to_keep
    ] %>%
    dplyr::distinct() 
  
  table_observation_ancillary <- table_observation_ancillary %>% 
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(
      cols = -observation_id,
      names_to = "variable_name", 
      values_to = "value") %>% 
    dplyr::mutate(
      observation_ancillary_id = paste0(variable_name, "_for_", observation_id)) %>%
    dplyr::filter(!is.na(value))
  
  # table_observation_ancillary$observation_ancillary_id <- 
  #   paste0("obsan_", seq(nrow(table_observation_ancillary)))
  
  table_observation_ancillary$unit <- NA_character_
  table_observation_ancillary <- dplyr::select(
    table_observation_ancillary,
    observation_ancillary_id,
    observation_id,
    variable_name,
    value,
    unit) %>% 
    dplyr::distinct()
  
  return(table_observation_ancillary)
}


