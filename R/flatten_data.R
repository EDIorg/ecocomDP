#' Flatten an ecocomDP dataset
#' 
#' @param tables (list of tbl_df, tbl, data.frame) A named list of ecocomDP tables.
#'
#' @return (tbl_df, tbl, data.frame) A single flat table created by joining and spreading all \code{tables}, except the observation table. See details for more information on this "flat" format.
#' 
#' @details "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#' 
#' @note Ancillary identifiers are dropped from the returned object.
#' 
#' @export
#'
#' @examples 
#' dataset <- ants_L1
#' 
#' flat <- flatten_data(dataset[[1]]$tables)
#' 
#' flat
#' 
flatten_data <- function(tables) {
  
  validate_arguments(fun.name = "flatten_data", fun.args = as.list(environment()))
  
  # Start with observation
  all_merged <- tables$observation %>%
    dplyr::select_if(not_all_NAs)
  
  # Merge observation_ancillary
  if ("observation_ancillary" %in% names(tables)) {
    #handle missing observation_id or case where all observation_id is na
    if (!"observation_id" %in% names(tables$observation_ancillary) || 
        all(is.na(tables$observation_ancillary$observation_id)) ) {
      warning("'observation_id' is missing from the observation_ancillary table")
    }else{
      #the expected case:
      observation_ancillary_wide <- flatten_ancillary(tables$observation_ancillary)
      all_merged <- all_merged %>%
        dplyr::left_join(observation_ancillary_wide, 
                         by = "observation_id",
                         suffix = c("", "_observation_ancillary"))
    }
  }
  
  # Merge location data and using "NEON location type" from location ancillary data
  if("location" %in% names(tables)) {
    last_col <- ncol(all_merged) # Index of last column for sorting
    if (("location_ancillary" %in% names(tables)) & 
        ("NEON location type" %in% tables$location_ancillary$variable_name)) {
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
    } else {
      #otherwise, just merge location table with all_merged
      all_merged <- all_merged %>%
        dplyr::left_join(
          tables$location %>%
            dplyr::select_if(not_all_NAs),
          by = "location_id",
          suffix = c("", "_location"))
    }
    # Sort, move location_id to beginning of location cols
    last_col <- colnames(all_merged)[last_col]
    all_merged <- all_merged %>% 
      relocate(location_id, .after = last_col)
  }
  
  # Merge location_ancillary data only for bottom level when locations are nested
  if ("location_ancillary" %in% names(tables)) {
    location_ancillary <- tables$location_ancillary %>%
      dplyr::filter(variable_name != "NEON location type",
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
  
  # Merge taxon table
  if ("taxon" %in% names(tables)) {
    last_col <- ncol(all_merged) # Index of last column for sorting
    all_merged <- all_merged %>%
      dplyr::left_join(
        tables$taxon %>%
          dplyr::select_if(not_all_NAs),
        by = "taxon_id")
    # Sort, move taxon_id to beginning of taxon cols
    last_col <- colnames(all_merged)[last_col]
    all_merged <- all_merged %>% 
      relocate(taxon_id, .after = last_col)
  }
  
  # Merge taxon_ancillary
  if ("taxon_ancillary" %in% names(tables)) {
    taxon_ancillary_wide <- flatten_ancillary(tables$taxon_ancillary)
    all_merged <- all_merged %>%
      dplyr::left_join(
        taxon_ancillary_wide %>%
          dplyr::select_if(not_all_NAs),
        by = "taxon_id",
        suffix = c("", "_taxon_ancillary"))
  }
  
  # Merge dataset_summary
  if ("dataset_summary" %in% names(tables)) {
    last_col <- ncol(all_merged) # Index of last column for sorting
    all_merged <- all_merged %>%
      dplyr::left_join(
        tables$dataset_summary %>% dplyr::select_if(not_all_NAs), 
        by = "package_id")
    # Sort, move package_id to beginning of dataset_summary cols
    last_col <- colnames(all_merged)[last_col]
    all_merged <- all_merged %>% 
      relocate(package_id, .after = last_col)
  }
  
  # Coerce columns to correct classes
  all_merged <- coerce_all_merged(all_merged)
  
  # Coerce table class
  cls <- class(tables$observation)
  if (all(c("tbl_df", "tbl", "data.frame") %in% cls)) {
    all_merged <- tidyr::as_tibble(all_merged)
  } else {
    all_merged <- as.data.frame(all_merged)
  }
  
  # Remove any unwanted columns
  vars <- "parent_location_id"
  all_merged <- all_merged %>% dplyr::select(-dplyr::any_of(vars))
  
  # Return
  return(all_merged)
  
}









# Flatten ancillary table
#
# @param ancillary_table (data.frame) Ancillary table to flatten. Can be: observation_ancillary, location_ancillary, or taxon_ancillary.
#
# @return (data.frame) A flattened version of \code{ancillary_table} with units added following the unit_<variable_name> 
# convention, and sorted so the unit immediately follows its corresponding variable_name.
# 
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
      stats::na.omit() %>% 
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








# Determine if a col is all NAs
#
# @param x A vector of values
#
# @return (logical) TRUE if not all NAs, otherwise FALSE
# 
not_all_NAs <- function(x) {
  !all(is.na(x))
}








# Helper to handle duplicates
#
# @param x A vector of values
#
# @return A vector of values, averaged or concatenated
# 
# @details Helper function to handle duplicates within the observation_ancillary table. Modifications to the 
# ecocomDP model and corresponding validation checks prohibit duplicates within ancillary tables, but this function handles 
# instances where a user flattens data that could not otherwise be flattened.
#
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







# Coerce columns of flatten_data()'s "all_merged" to correct classes
#
# @param x (data.frame) The all_merged object
#
# @return (data.frame) \code{x} with column classes coerced to ecocomDP specifications or 
# numeric if not an ecocomDP column and if no NAs are generated.
# 
coerce_all_merged <- function(x) {
  # Coerce column classes. Often a numeric type has been stored as character in the long L1 tables. But some columns (primarily ids) should remain character
  crit <- read_criteria() %>% 
    dplyr::select(column, class) %>% 
    dplyr::filter(column != "datetime", column != "value") %>%
    dplyr::distinct()
  for (col in colnames(x)) {
    # Coerce cols to ecocomDP criteria and coerce anything outside this set to numeric as long as no NAs are introduced (i.e. cases that are likely character types)
    if (col %in% crit$column) {                                         # is ecocomDP col
      expctd <- crit$class[crit$column %in% col]
      if (expctd == "character" & class(x[[col]]) != expctd) {
        x[[col]] <- as.character(x[[col]])
      }
      if (expctd == "numeric" & class(x[[col]]) != expctd) {
        x[[col]] <- as.numeric(x[[col]])
      }
    } else if (col %in% "datetime") {                                   # is ecocomDP datetime col
      # Do nothing, because datetimes are unambiguous
    } else {                                                            # is likely a variable
      x[[col]] <- tryCatch(
        as.numeric(x[[col]]), 
        warning = function(w) x[[col]])
    }
  }
  return(x)
}
