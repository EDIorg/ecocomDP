#' Flatten a dataset
#' 
#' @param data (list) The dataset object returned by \code{read_data()}, or a named list of ecocoomDP tables.
#'
#' @return (tbl_df, tbl, data.frame) A single flat table created by joining and spreading all \code{tables}, except the observation table. See details for more information on this "flat" format.
#' 
#' @details The "flat" format refers to the fully joined source L0 dataset in "wide" form with the exception of the core observation variables, which are in "long" form (i.e. using the variable_name, value, unit columns of the observation table). This "flat" format is the "widest" an L1 ecocomDP dataset can be consistently spread due to the frequent occurrence of L0 source datasets with > 1 core observation variable.
#' 
#' @note 
#' Warnings/Errors from \code{flatten_data()} can most often be fixed by addressing any validation issues reported by \code{read_data()} (e.g. non-unique composite keys).
#' 
#' Ancillary identifiers are dropped from the returned object.
#' 
#' @export
#'
#' @examples 
#' # Flatten a dataset object
#' flat <- flatten_data(ants_L1)
#' flat
#' 
#' # Flatten a list of tables
#' tables <- ants_L1$tables
#' flat <- flatten_data(tables)
#' flat
#' 
flatten_data <- function(data) {

  # TODO Refactor this validation check
  # validate_arguments(fun.name = "flatten_data", fun.args = as.list(environment()))

  if (detect_data_type(data) == "dataset") {
    res <- flatten_tables(data$tables)
  } else if (detect_data_type(data) == "list_of_tables") {
    res <- flatten_tables(data)
  } else if (detect_data_type(data) == "dataset_old") {
    res <- flatten_tables(data[[1]]$tables)
  } else {
    stop('Input to "data" is not one of the supported types.', call. = FALSE)
  }
  return(res)
}








#' Flatten ecocomDP tables
#' 
#' @param tables list of ecocomDP tables
#' 
#' @noRd
#' 
flatten_tables <- function(tables){

  # Start w/observation -------------------------------------------------------
  
  all_merged <- tables$observation %>%
    dplyr::select_if(not_all_NAs)
  
  # Merge observation_ancillary -----------------------------------------------
  
  if ("observation_ancillary" %in% names(tables)) {
    #handle missing observation_id or case where all observation_id is na
    if (!"observation_id" %in% names(tables$observation_ancillary) || 
        all(is.na(tables$observation_ancillary$observation_id))) {
      warning("'observation_id' is missing from the observation_ancillary ",
              "table. Cannot join observation_ancillary to observation.")
    }else{
      #the expected case:
      observation_ancillary_wide <- flatten_ancillary(tables$observation_ancillary)
      all_merged <- all_merged %>%
        dplyr::left_join(observation_ancillary_wide, 
                         by = "observation_id",
                         suffix = c("", "_observation_ancillary"))
    }
  }
  
  # Merge location ------------------------------------------------------------
  
  # Merge location data using one of two approaches:
  # 1.) "NEON location type" from location ancillary data
  # 2.) Find unique marker injected by create_location() to parse column names from values
  if("location" %in% names(tables)) {
    last_col <- ncol(all_merged) # Index of last column for sorting
    if (("location_ancillary" %in% names(tables)) & 
        ("NEON location type" %in% tables$location_ancillary$variable_name)) {
      # Approach 1: Used by NEON
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
      # Approach 2: Used by everyone else (i.e. non-NEON)
      res_flatloc <- flatten_location(location = tables$location)
      tables$location <- res_flatloc$location_flat
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
  
  # Merge location_ancillary --------------------------------------------------
  
  # ... data only for bottom level when locations are nested
  if ("location_ancillary" %in% names(tables)) {
    location_ancillary <- tables$location_ancillary %>%
      dplyr::filter(variable_name != "NEON location type",
                    location_id %in% all_merged$location_id)
    if (nrow(location_ancillary) > 0) {
      location_ancillary_wide <- flatten_ancillary(location_ancillary)
      if ("datetime" %in% colnames(location_ancillary_wide)) { # Join with datetime if present
        all_merged <- all_merged %>%
          dplyr::left_join(
            location_ancillary_wide %>%
              dplyr::select_if(not_all_NAs),
            by = c("location_id", "datetime"),
            suffix = c("", "_location_ancillary"))
      } else {
        all_merged <- all_merged %>%
          dplyr::left_join(
            location_ancillary_wide %>%
              dplyr::select_if(not_all_NAs),
            by = "location_id",
            suffix = c("", "_location_ancillary"))
      }
    }
  }
  
  # Merge taxon ---------------------------------------------------------------
  
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
  
  # Merge taxon_ancillary -----------------------------------------------------
  
  if ("taxon_ancillary" %in% names(tables)) {
    taxon_ancillary_wide <- flatten_ancillary(tables$taxon_ancillary)
    all_merged <- all_merged %>%
      dplyr::left_join(
        taxon_ancillary_wide %>%
          dplyr::select_if(not_all_NAs),
        by = "taxon_id",
        suffix = c("", "_taxon_ancillary"))
  }
  
  # Merge dataset_summary -----------------------------------------------------
  
  if ("dataset_summary" %in% names(tables)) {
    last_col <- ncol(all_merged) # Index of last column for sorting
    all_merged <- all_merged %>%
      dplyr::left_join(
        tables$dataset_summary %>% dplyr::select_if(not_all_NAs), 
        by = "package_id")
    # Sort, move package_id to beginning of dataset_summary cols
    last_col <- colnames(all_merged)[last_col]
    colnames(all_merged)
    all_merged <- all_merged %>%
      relocate(package_id, .after = last_col)
  }
  
  # Coerce columns to correct classes -----------------------------------------
  
  if (exists("res_flatloc")) {
    all_merged <- coerce_all_merged(x = all_merged, 
                                    locnames = res_flatloc$locnames)
  }
  
  # Coerce table class --------------------------------------------------------
  
  cls <- class(tables$observation)
  if (all(c("tbl_df", "tbl", "data.frame") %in% cls)) {
    all_merged <- tidyr::as_tibble(all_merged)
  } else {
    all_merged <- as.data.frame(all_merged)
  }
  
  # Remove any unwanted columns -----------------------------------------------
  
  vars <- "parent_location_id"
  all_merged <- all_merged %>% dplyr::select(-dplyr::any_of(vars))
  
  # Return
  return(all_merged)
  
}









#' Flatten ancillary table
#'
#' @param ancillary_table (data.frame) Ancillary table to flatten. Can be: observation_ancillary, location_ancillary, or taxon_ancillary.
#'
#' @return (data.frame) A flattened version of \code{ancillary_table} with units added following the unit_<variable_name> 
#' convention, and sorted so the unit immediately follows its corresponding variable_name.
#'
#' @noRd
#' 
flatten_ancillary <- function(ancillary_table) {
  # Spread on variable_name and value, then add units later
  fkey <- stringr::str_subset(colnames(ancillary_table), "(?<!ancillary_)id") # Use regexpr to select id column to later join on
  auth <- ifelse("author" %in% colnames(ancillary_table), "author", list(NULL))
  vars <- c(fkey, "variable_name", "value", unlist(auth))
  # Add datetime to list of vars if present
  if (!is.null(ancillary_table[["datetime"]]) & not_all_NAs(ancillary_table[["datetime"]])) {
    vars <- c(vars, "datetime")
  }
  df <- ancillary_table %>% 
    dplyr::select(any_of(vars)) %>%
    tidyr::pivot_wider(names_from = variable_name, 
                       values_from = value)
  # Add units
  unts <- suppressWarnings(ancillary_table$unit)
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
  if (methods::is(x, "numeric")){
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
# @param locnames (character) Names of location columns
#
# @return (data.frame) \code{x} with column classes coerced to ecocomDP specifications, location names coerced to character, or numeric if not an ecocomDP column and if no NAs are generated. 
# 
coerce_all_merged <- function(x, locnames) {
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
    } else if (col %in% locnames) {                                     # is a location name col
      x[[col]] <- as.character(x[[col]])
    } else {                                                            # is likely a variable
      x[[col]] <- tryCatch(
        as.numeric(x[[col]]), 
        warning = function(w) x[[col]])
    }
  }
  return(x)
}








# Flatten the location table
#
# @param location (tbl_df, tbl, data.frame) The location table
#
# @return A list of:
# location_flat: (tbl_df, tbl, data.frame) A flattened version of \code{location} with nested levels unpacked and latitude, longitude, and elevation only returned for the level of observation.
# locnames: (character) So coerce_all_merged() knows which to coerce to "character" class
#'
#' @noRd
flatten_location <- function(location) {
  
  # An empty object for collecting the order of nested locations
  nesting_order <- c()
  
  # If no location_name, then remove location_ids of parents and return location_new
  if (!"location_name" %in% colnames(location)) {
    res <- list(location_flat = location,
                locnames = nesting_order)
    return(res)
  }
  
  # FIXME Exception for legacy data - Remove this once all L1 have been updated
  if (!any(stringr::str_detect(location$location_name, "__.*"))) {
    last_child <- location$location_id[nrow(location)]
    ultimate_child_id <- stringr::str_extract(last_child, "lo_[:digit:]*(?=_)")
    is_ultimate_child <- stringr::str_detect(location$location_id, ultimate_child_id)
    location <- location[is_ultimate_child, ]
    location <- dplyr::select(location, -parent_location_id)
    res <- list(location_flat = location,
                locnames = nesting_order)
    return(res)
  }
  
  # Add "location_type"
  location_ancillary <- location %>% 
    dplyr::mutate(location_type = stringr::str_remove_all(location_name, "__.*")) %>% 
    dplyr::select(location_id, location_type)
  location <- location %>% dplyr::left_join(location_ancillary, by = "location_id")
  
  # extract a list of the location types
  location_type_list <- location$location_type %>% unique()
  # location_type_list <- location$location_type %>% unique()
  
  # identify ultimate parents... i.e., records with no parents
  location_parent <- location %>% 
    dplyr::filter(is.na(parent_location_id)) %>%
    dplyr::select(location_id, location_type)
  nesting_order <- c(nesting_order, unique(location_parent$location_type))
  
  # identify all records that have parents
  location_child <- location %>% 
    dplyr::filter(!is.na(parent_location_id))
  
  # loop through from ultimate parents down, joining with child records, until there are no more children
  if (nrow(location_child) > 0) { # Has children
    while(nrow(location_child) > 0){
      # inner join parents to children
      location_new <- location_child %>% 
        dplyr::inner_join(
          location_parent %>% 
            dplyr::rename(location_type_parent = location_type),
          by = c("parent_location_id" = "location_id"))
      nesting_order <- c(nesting_order, unique(location_new$location_type))
      # rename parent location id column by location type ... location name
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
  } else {                       # Has no children
    location_new <- location %>% 
      dplyr::select(-c(parent_location_id, location_type))
  }
  
  # Get original column name containing observation level locations and then use it to rename the location_name column, otherwise value matching can't be done in the next sequence of steps (below).
  
  new_name <- unique(stringr::str_remove(location_new$location_name, "__.*"))
  L1_location_name <- location_new$location_name
  location_new$location_name <- stringr::str_extract(location_new$location_name, "(?<=__).*")
  cnames <- colnames(location_new)
  cnames[cnames == "location_name"] <- new_name
  colnames(location_new) <- cnames
  
  # Return values, not IDs
  # Iterate across unique location_id of input location table
  for (i in seq_along(location$location_id)) {
    # Extract value (i.e. get content in location_name after "__")
    val <- stringr::str_extract(location$location_name[i], "(?<=__).*")
    # Get location_type (which is the column name containing the location_id value to replace)
    loctype <- location$location_type[i]
    # Use location_type to index column and use location_id to index value, then replace with value 
    use_i <- location_new[[loctype]] == location$location_id[i]
    location_new[[loctype]][use_i] <- val
  }
  
  # Add location_name back in, because it represents the spatial level of observation, because
  # it is returned for flattened NEON datasets, and because it may facilitate future interoperability.
  location_new$location_name <- L1_location_name
  
  # Reorder location columns in terms of nesting (from high to low) for user understanding
  col_order <- c("location_id", "location_name", nesting_order, "latitude", "longitude", "elevation")
  location_new <- dplyr::relocate(location_new, any_of(col_order))
  
  
  # Return location_colnames so coerce_all_merged() knows which to coerce to "character" class
  res <- list(location_flat = location_new,
              locnames = nesting_order)
  return(res)
}