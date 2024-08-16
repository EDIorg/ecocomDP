# Add units to create_*() functions
#
# @param L0_flat (tbl_df, tbl, data.frame) The fully joined source L0 dataset, in "flat" format (see details).
# @param res (tbl_df, tbl, data.frame) Intermediary result in create_*()
# @param unit (character) Unit column input to create_*()
#
# @return (tbl_df, tbl, data.frame) \code{res} with an added unit column for the variables listed under variable_name
#
add_units <- function(L0_flat, res, unit) {
  if (!is.null(unit)) {
    unit_map <- L0_flat %>% dplyr::select(dplyr::all_of(unit)) %>% unique() # Unique unit values
    unit_map <- as.data.frame(lapply(unit_map, stats::na.omit))                    # Remove NA
    
    unit_map <- data.frame(variable_name = colnames(unit_map),
                           unit = as.character(unit_map[1, ]),              # Should only be one
                           stringsAsFactors = FALSE)
    unit_map$variable_name <- stringr::str_remove_all(unit_map$variable_name, 
                                                      "unit_")
    res <- dplyr::left_join(res, unit_map, by = "variable_name")
  } else if (is.null(unit)) {
    res$unit <- NA_character_
  }
  return(res)
}








# Get provenance metadata
#
# @description
#     Add Provenance Metadata from Level-1 metadata in PASTA to an XML 
#     document containing a single methods element in the request message 
#     body.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     ("xml_document" "xml_node") EML metadata.
#     
#
api_get_provenance_metadata <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving provenance metadata for ', package.id))
  
  ping_edi()
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/provenance/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )
  
  output <- httr::content(
    r,
    as = 'parsed',
    encoding = 'UTF-8'
  )
  
  output
  
}








# List data package revisions
#
# @description
#     List Data Package Revisions operation, specifying the scope and 
#     identifier values to match in the URI. The request may be filtered by 
#     applying the modifiers “oldest” or “newest” to the “filter” query 
#     parameter.
#
# @param scope
#     (character) Data package scope (e.g. 'edi', 'knb-lter-bnz').
# @param identifier
#     (character) Data package identifier (e.g. '100', '275').
# @param filter
#     (character) Filter to data packages, can be 'oldest' or 'newest'.
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     (character) Vector of revisions, if more than one exists, otherwise a 
#     single revision number.
#
api_list_data_package_revisions <- function(scope, identifier, filter = NULL, environment = 'production'){
  
  message(paste('Retrieving data package revisions for', 
                paste(scope, '.', identifier)))
  
  ping_edi()
  
  if (is.null(filter)){
    
    r <- httr::GET(
      url = paste0(
        url_env(environment),
        '.lternet.edu/package/eml/',
        scope,
        '/',
        identifier
      )
    )
    
    r <- httr::content(
      r,
      as = 'text',
      encoding = 'UTF-8'
    )
    
    output <- utils::read.csv(
      text = c(
        'revision',
        r
      ),
      as.is = T
    )
    
    output <- as.character(output$revision)
    
  } else if (filter == 'newest'){
    
    r <- httr::GET(
      url = paste0(
        url_env(environment),
        '.lternet.edu/package/eml/',
        scope,
        '/',
        identifier,
        '?filter=newest'
      )
    )
    
    output <- httr::content(
      r,
      as = 'text',
      encoding = 'UTF-8'
    )
    
  } else if (filter == 'oldest'){
    
    r <- httr::GET(
      url = paste0(
        url_env(environment),
        '.lternet.edu/package/eml/',
        scope,
        '/',
        identifier,
        '?filter=oldest'
      )
    )
    
    output <- httr::content(
      r,
      as = 'text',
      encoding = 'UTF-8'
    )
    
  }
  
  output
  
}








# Read data entity name
#
# @description
#     Read Data Entity Name operation, specifying the scope, identifier, 
#     revision, and entity identifier of the data entity whose name is 
#     to be read in the URI.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param identifier
#     (character) Data entity identifier (e.g. 
#     5c224a0e74547b14006272064dc869b1)
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     (character) Data entity name
#
api_read_data_entity_name <- function(package.id, identifier, environment = 'production'){
  
  message(paste('Retrieving name of', package.id, identifier))
  
  ping_edi()
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/name/eml/',
      stringr::str_replace_all(package.id, '\\.', '/'),
      '/',
      identifier
    )
  )
  
  r <- httr::content(
    r,
    as = 'text',
    encoding = 'UTF-8'
  )
  
  output <- as.character(
    utils::read.csv(
      text = c(
        'identifier',
        r
      ),
      as.is = T
    )$identifier
  )
  
  output
  
}








# Read data package
#
# @description
#     Read Data Package operation, specifying the scope, identifier, and 
#     revision of the data package to be read in the URI, returning a resource 
#     map with reference URLs to each of the metadata, data, and quality 
#     report resources that comprise the data package.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     (character) Reference URLs.
#     
api_read_data_package <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving resource map for', package.id))
  
  ping_edi()
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )
  
  r <- httr::content(
    r,
    as = 'text',
    encoding = 'UTF-8'
  )
  
  output <- as.character(
    utils::read.csv(
      text = c(
        'identifier',
        r
      ),
      as.is = T
    )$identifier
  )
  
  output
  
}








# Read data package DOI
#
# @description
#     Read Data Package DOI operation, specifying the scope, identifier, 
#     and revision of the data package DOI to be read in the URI.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     (character) The canonical Digital Object Identifier.
#
api_read_data_package_doi <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving DOI for', package.id))
  
  ping_edi()
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/doi/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )
  
  output <- httr::content(
    r,
    as = 'text',
    encoding = 'UTF-8'
  )
  
  output
  
}








# Read metadata
#
# @description
#     Read Metadata (EML) operation, specifying the scope, identifier, and 
#     revision of the EML document to be read in the URI.
#
# @param package.id
#     (character) Package identifier composed of scope, identifier, and
#     revision (e.g. 'edi.101.1').
# @param environment
#     (character) Data repository environment to create the package in.
#     Can be: 'development', 'staging', 'production'.
#
# @return
#     ('xml_document' 'xml_node') EML metadata.
#     
#
api_read_metadata <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving EML for data package', package.id))
  
  ping_edi()
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/metadata/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )
  
  eml <- httr::content(
    r,
    as = 'parsed',
    encoding = 'UTF-8'
  )
  
  eml
  
}









# Convert \code{character(0)} to \code{""}
#
# @param txt (character) Character string
#
# @return (character) Return \code{txt} if not empty else ""
#
cnvmt <- function(txt) {
  if (length(txt) == 0) {
    return("")
  } else {
    return(txt)
  }
}








# Coerce table classes to ecocomDP specifications
#
# @param tbl (data.frame) Table to coerce
# @param name (character) Table name
# @param cls (character) Class of L0_flat input.
#
# @return \code{tbl} with column classes coerced to ecocomDP model specifications and of the input type specified by \code{cls}.
# 
# @details Datetime columns are not coerced. These are unchanged from the input class.
# 
coerce_table_classes <- function(tbl, name, cls) {
  
  crit <- read_criteria() %>% 
    dplyr::filter(table == name) %>% 
    dplyr::select(column, class) %>% 
    stats::na.omit()
  for (col in colnames(tbl)) {
    colclass <- crit$class[crit$column == col]
    if (colclass == "character") {
      tbl[[col]] <- as.character(tbl[[col]])
    } else if (colclass == "numeric") {
      tbl[[col]] <- as.numeric(tbl[[col]])
    }
  }
  if (all(c("tbl_df", "tbl", "data.frame") %in% cls)) {
    tbl <- tidyr::as_tibble(tbl)
  } else {
    tbl <- as.data.frame(tbl)
  }
  return(tbl)
}







#' Detect the type of object input to the \code{data} parameter
#' 
#' @description The \code{data} parameter, used by a few functions in the ecocomDP package, can accept different object types. \code{detect_data_type()} identifies the object type, which the calling function typically uses in some flow control logic.
#' 
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' 
#' @return (character) The type of \code{data}, which is one of:
#' \itemize{
#'   \item "dataset": The default return of \code{read_data()}
#'   \item "list_of_datasets": > 1 "dataset"
#'   \item "table": A flat table
#'   \item "list_of_tables": The named list of L1 tables (i.e. read_data()$tables)
#'   \item "dataset_old": The old, and since deprecated, return of \code{read_data()}
#'   \item "list_of_datasets_old": > 1 "dataset_old"
#' }
#' 
#' Unrecognized types will thorw an error.
#' 
#' @noRd
#' 
detect_data_type <- function(data){
  table_names <- unique(read_criteria()$table)
  # dataset
  if (("list" %in% class(data)) & ("tables" %in% names(data))) {
    if (sum(names(data) == "tables") == 1) {
      return("dataset")
    }
  }
  # list_of_datasets
  if (("list" %in% class(data)) & (length(data) > 1)) {
    res <- lapply(data, function(x) {"tables" %in% names(x)})
    if (all(unlist(res))) {
      return("list_of_datasets")
    }
  }
  # table
  if(all(class(data) %in% c("data.frame", "tbl_df", "tbl"))){
    return("table")
  }
  # list_of_tables
  if(("list" %in% class(data)) & any(table_names %in% names(data))) {
    return("list_of_tables")
  }
  # dataset_old
  is_dataset_old <- function(x) {
    if ("list" %in% class(x)) {
      if ("tables" %in% names(x)) {
        return(FALSE)
      }
      if (length(x) == 1) {
        if ("tables" %in% names(x[[1]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  if (is_dataset_old(data)) {
    warning('Input to "data" is an old and deprecated format. Please use the ',
            'new format instead. See ?read_data for more info.', call. = FALSE)
    return("dataset_old")
  }
  # list_of_dataset_old
  if (!is.null(data) & all(unlist(lapply(data, is_dataset_old)))) {
    return("list_of_datasets_old")
  }
  # unrecognized
  stop('Input to "data" is not one of the supported types.', call. = FALSE)
}








# Detect field delimiter of file
#
# @param path (character) Path in which \code{data.files} are found
# @param data.files (character) File names
# @param os (character) Return from \code{detect_os()}.
#
# @details Parses the verbose return from \code{data.table::fread()} to 
# get the delimiter value. If this fails, then a secondary function is called utilizing the suggested \code{reader} package. If this secondary approach fails, then a default "," is returned.
#
# @return (character) Field delimiter of \code{data.files}
#
detect_delimiter <- function(path, data.files, os) {
  f <- paste0(path, "/", data.files)
  msg <- utils::capture.output(data.table::fread(f, verbose = TRUE) %>% {NULL}) # primary method
  seps <- stringr::str_extract_all(msg, "(?<=(sep=')).+(?='[:blank:])")
  sep <- unique(unlist(seps))
  if (length(sep) == 1) {
    return(sep)
  } else {
    warning("Could not detect field delimiter for ", f, ". Trying alternate ",
            "method.", call. = FALSE)
    if (!requireNamespace("reader", quietly = TRUE)) {                   # default value
      warning("Package 'reader' is required for the alternate field delimiter",
              " detection method but is not installed.", call. = FALSE)
      warning("Could not detect field delimiter for ", f, ". Defaulting to ",
              "','.", call. = FALSE)
      return(",")
    } else {                                                             # secondary method
      res <- detect_delimiter_method_2(path, data.files, detect_os())
      return(res)
    }
  }
}








# Get field delimiters of input files (method 2)
#
# @description  
#     Detect and return field delimiters of input files (tables).
# 
# @param path 
#     (character) Path to files.
# @param data.files
#     (character) File names.
# @param os
#     (character) Operating system. Valid options are returned from  
#     \code{EDIutils::detect_os}.
# 
# @return 
#     (character) Field delimiters of input files.
#     \item{"\\t"}{tab}
#     \item{","}{comma}
#     \item{";"}{semi-colon}
#     \item{"|"}{pipe}
#
detect_delimiter_method_2 <- function(path, data.files, os){
  
  # Check for suggested package
  if (!requireNamespace("reader", quietly = TRUE)) {
    warning("Package 'reader' is required for delimiter detection but is not installed", call. = FALSE)
  }
  
  # Validate data tables
  
  data_files <- validate_file_names(path, data.files)
  
  # Detect field delimiters
  # Loop through each table using reader::get.delim() to return the field
  # delimiter. Note: reader::get.delim() performance seems to be operating 
  # system specific.
  
  delim_guess <- c()
  data_path <- c()
  
  for (i in seq_along(data_files)){
    
    # Initialize output vector
    
    data_path[i] <- paste0(path, '/', data_files[i])
    
    if (os == "mac"){
      
      # Detect delimiter for table in Mac OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    } else if (os == "win"){
      
      # Detect delimiter for table in Windows OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    } else if (os == 'lin'){
      
      # Detect delimiter for table in Linux OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    }
    
  }
  
  # Return
  
  delim_guess
  
}







# Detect operating system
#
# @description  
#     This function uses \code{Sys.info} to detect the user's operating system 
#     and outputs an abbreviated character string to be used as inputs to OS
#     specific function calls.
# 
# @return 
#     \item{win}{Windows OS}
#     \item{mac}{Mac OS}
#
detect_os <- function(){
  sysinfo <- Sys.info()['sysname']
  if (sysinfo == 'Darwin'){
    os <- 'mac'
  } else if (sysinfo == 'Windows'){
    os <- 'win'
  } else {
    os <- 'lin'
  }
  os
}








# Unnest nested sites in the location table
#
# @param dt_loc 
#     (data.frame) The location table
# @return
#     (data.frame) With columns location_id, site_name, latitude, longitude, 
#     elevation.
# @details 
#     Dereferences each of the parent_id, creates a site name that includes
#     the site names of each parent, and adds the lowest latitude, longitude 
#     and elevation available in the table.
# 
flatten_location_dwca <- function(dt_loc) {
  
  # Expand location table
  
  # Rewrite names of locations to indicate their nested arrangements and expand
  # corresponding latitude, longitude, and elevation
  loc_name_combined <- rep(NA_character_, nrow(dt_loc))
  for (i in 1:length(dt_loc$location_id)) {
    if (!is.na(dt_loc$parent_location_id[i])) {
      id_out <- dt_loc$location_id[i]
      id <- dt_loc$location_id[i]
      cont <- TRUE
      while (isTRUE(cont)) {
        if (!is.na(dt_loc$parent_location_id[id == dt_loc$location_id])) {
          id_out[length(id_out) + 1] <- dt_loc$parent_location_id[id == dt_loc$location_id]
          id <- dt_loc$parent_location_id[id == dt_loc$location_id]
        } else {
          cont <- FALSE
        }
      }
      loc_name_combined[i] <- paste(
        dt_loc$location_name[
          dt_loc$location_id %in% rev(id_out)], 
        collapse = ".")
      # Supress warnings for cases when no latitude is available
      lat_i <- suppressWarnings(
        max(
          which(
            !is.na(
              dt_loc$latitude[
                dt_loc$location_id %in% rev(id_out)]))))
      # Supress warnings for cases when no longitude is available
      lon_i <- suppressWarnings(
        max(
          which(
            !is.na(
              dt_loc$longitude[
                dt_loc$location_id %in% rev(id_out)]))))
      # Supress warnings for cases when no elevation is available
      elv_i <- suppressWarnings(
        max(
          which(
            !is.na(
              dt_loc$elevation[
                dt_loc$location_id %in% rev(id_out)]))))
      if (!is.na(lat_i) & !is.na(lon_i)) {
        if (lat_i == lon_i) {
          fill <- dt_loc$latitude[dt_loc$location_id %in% rev(id_out)]
          fill[is.na(fill)] <- dt_loc$latitude[dt_loc$location_id %in% rev(id_out)][lat_i]
          dt_loc$latitude[dt_loc$location_id %in% rev(id_out)] <- fill
          fill <- dt_loc$longitude[dt_loc$location_id %in% rev(id_out)]
          fill[is.na(fill)] <- dt_loc$longitude[dt_loc$location_id %in% rev(id_out)][lat_i]
          dt_loc$longitude[dt_loc$location_id %in% rev(id_out)] <- fill
          if (elv_i == lat_i) {
            fill <- dt_loc$elevation[dt_loc$location_id %in% rev(id_out)]
            fill[is.na(fill)] <- dt_loc$elevation[dt_loc$location_id %in% rev(id_out)][lat_i]
            dt_loc$elevation[dt_loc$location_id %in% rev(id_out)] <- fill
          }
        }
      }
    }
  }
  
  # Create the expanded location table
  loc_name_combined[is.na(dt_loc$parent_location_id)] <- 
    dt_loc$location_name[is.na(dt_loc$parent_location_id)]
  dt_loc_expanded <- dt_loc
  dt_loc_expanded$location_name <- loc_name_combined
  dt_loc_expanded$parent_location_id <- NULL
  
  # Return
  dt_loc_expanded
  
}








# Get attribute definitions from EML
#
# @param eml (xml_document, xml_node) EML metadata
#
# @return (named list) Definitions
# 
# @note Duplicate names are dropped.
# 
get_attr_defs <- function(eml) {
  nodes <- attrs <- xml2::xml_find_all(eml, ".//dataTable")
  nmes <- xml2::xml_text(xml2::xml_find_all(nodes, ".//attribute/attributeName"))
  defs <- xml2::xml_text(xml2::xml_find_all(nodes, ".//attribute/attributeDefinition"))
  dups <- duplicated(nmes)
  if (any(dups)) {
    # warning("Duplicate attribute names were found in the parent EML when looking up definitions for attributes of this dataset. Dropping these attributes: ", paste(nmes[dups], collapse = ", "), call. = FALSE)
    nmes <- nmes[!dups]
    defs <- defs[!dups]
  }
  res <- defs
  names(res) <- nmes
  return(res)
}





# Get end of line (EOL) character
#
# @description
#     Get EOL character of input file(s).
#
# @param path
#     (character) A path to the target file directory.
# @param file.name
#     (character) The target file name.
#
# @return
#     A character string representation of the EOL character.
#
get_eol <- function(path, file.name){
  file_name <- validate_file_names(path, file.name)
  output <- readChar(paste0(path, '/', file.name), nchars = 10000)
  eol <- parse_delim(output)
  return(eol)
}






#' Get the dataset \code{id} from a \code{data} argument
#' 
#' @description Several functions use the \code{id} parameter. This helper function gets the \code{id} if it can be found in \code{data}.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#'
#' @return (character) The dataset identifier
#' 
#' @noRd
#'
get_id <- function(data) {
  # Get the id from allowed object types and return NA if missing
  if (detect_data_type(data) == "dataset") {
    res <- data$id
    if (is.null(res)) {
      res <- NA_character_
    }
  } else if (detect_data_type(data) == "dataset_old") {
    res <- names(data)
    if (is.null(res)) {
      res <- NA_character_
    }
  } else {
    res <- NA_character_
  }
  return(res)
}








#' Get the observation table from a \code{data} argument
#' 
#' @description Several functions operate on the observation table but accept a broader set of inputs via the \code{data} parameter. This helper function does that.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#'
#' @return (tbl_df, tbl, data.frame) The observation table.
#' 
#' @noRd
#'
get_observation_table <- function(data) {
  # Get the observation table from allowed object types and error if missing
  if (detect_data_type(data) == "dataset") {
    table <- data$tables[["observation"]]
  } else if (detect_data_type(data) == "list_of_tables") {
    table <- data[["observation"]]
  } else if (detect_data_type(data) == "dataset_old") {
    table <- data[[1]]$tables[["observation"]]
  }
  if (is.null(table)) {
    stop('Input argument "data" does not contain the observation table', 
         call. = FALSE)
  }
  if (detect_data_type(data) == "table") {
    table <- data %>% dplyr::distinct()
  }
  # Extract all columns of the observation table and error if missing
  criteria <- dplyr::filter(read_criteria(), table == "observation")
  obs_cols <- stats::na.omit(criteria$column)
  res <- dplyr::select(table, dplyr::all_of(obs_cols))
  return(res)
}








# Does dataset ID belong to EDI?
#
# @param id (character) Dataset identifier 
#
# @return (logical) TRUE if is an EDI identifier, otherwise FALSE
#
is_edi <- function(id) {
  res <- stringr::str_detect(
    string = id, 
    pattern = paste0("(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|",
                     "(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)"))
  return(res)
}








# Does dataset ID belong to NEON?
#
# @param id (character) Dataset identifier 
#
# @return (logical) TRUE if is a NEON identifier, otherwise FALSE
#
is_neon <- function(id) {
  res <- stringr::str_detect(string = id, pattern = "^neon\\.")
  return(res)
}








# Is provenance node?
#
# @param nodeset (xml_nodeset) methods nodeset at \code{/eml:eml/dataset/methods/methodStep}
# 
# @details Looks for provenance in \code{./dataSource}
# 
# @return (logical) TRUE if nodeset has provenance
# 
is_prov <- function(nodeset) {  
  dasource <- xml2::xml_find_all(nodeset, "./dataSource")
  res <- !is_empty_nodeset(dasource)
  return(res)
}








# Parse datetime format from values
#
# @param vals (character) Vector of datetimes
# 
# @details Only works for \code{vals} of the format "YYYY-MM-DD hh:mm:ss" and subsets thereof. Values in other formats will return errant formats (e.g. "07/20/2021" returns "YYYY-MM-DD hh").
# 
# @return (character) Datetime format string of \code{vals}
# 
parse_datetime_frmt_from_vals <- function(vals) {
  # Modify inputs for processing
  vals <- as.character(vals)
  # Best match has the fewest coercions
  na_start <- sum(is.na(vals))
  na_end <- suppressWarnings(
    c(sum(is.na(lubridate::parse_date_time(vals, "ymdHMS"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymdHM"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymdH"))),
      sum(is.na(lubridate::parse_date_time(vals, "ymd"))),
      sum(is.na(lubridate::parse_date_time(vals, "y")))))
  na_coerced <- na_end - na_start
  if (stats::var(na_coerced) == 0) {    # When format of vals are NA or unsupported
    frmt <- NULL
  } else {                       # When format of vals are supported
    best_match <- which(na_coerced == min(na_coerced))[1]
    frmt <- c("YYYY-MM-DD hh:mm:ss",
              "YYYY-MM-DD hh:mm",
              "YYYY-MM-DD hh",
              "YYYY-MM-DD",
              "YYYY")[best_match]
    if (min(na_coerced) != 0) {  # When the best match doesn't represent all vals
      warning("The best match '", frmt, "' may not describe all datetimes")
    }
  }
  return(frmt)
}








# Parse datetime when format string is known
#
# @param tbl (character) Table name \code{vals} come from. This is used in warning messages.
# @param vals (character) Vector of datetimes
# @param frmt (character) Datetime format string
# 
# @details A wrapper to to \code{lubridate::ymd()}, \code{lubridate::ymd_h()}, \code{lubridate::ymd_hm()}, and \code{lubridate::ymd_hms()}.
# 
# @note No attempt is made at using the time zone component of \code{frmt}. Years (\code{frmt = "YYYY"}) are parsed to dates (\code{frmt = "YYYY-MM-DD"}) and a warning is raised.
# 
# @return (POSIXct POSIXt) Datetimes parsed
# 
parse_datetime_from_frmt <- function(tbl, vals, frmt) {
  # Modify inputs for processing below
  vals <- as.character(vals)
  na_i <- sum(is.na(vals))
  vals <- stringr::str_remove_all(vals, "(Z|z).+$")
  # Parse datetimes using lubridate
  if (frmt == "YYYY") {
    res <- "y"
    parsed <- lubridate::parse_date_time(vals, res)
    warning("Input datetimes have format 'YYYY' but are being returned as 'YYYY-MM-DD'.", call. = FALSE)
  } else {
    res <- "ymd"
    t <- unlist(stringr::str_split(frmt, "T|[:blank:]"))[2] # time format
    if (!is.na(t)) {                                        # build time component of lubridate func call
      if (stringr::str_detect(tolower(t), "hh")) {
        res <- paste0(res, "_h")
      }
      if (stringr::str_detect(tolower(t), "mm")) {
        res <- paste0(res, "m")
      }
      if (stringr::str_detect(tolower(t), "ss")) {
        res <- paste0(res, "s")
      }
    }
    parsed <- suppressWarnings(
      eval(parse(text = paste0("lubridate::", res, "(vals)"))))
  }
  # Warn user of parsing issues
  na_f <- sum(is.na(parsed))
  if (na_f > na_i) {
    warning((na_f - na_i), " ", tbl, " datetime strings failed to parse. Use parse_datetime = 'FALSE' to get datetimes as character strings for manual processing.")
  }
  return(parsed)
}







# Parse delimiter from string
parse_delim <- function(x){
  
  use_i <- stringr::str_detect(
    x,
    '\\r\\n'
  )
  
  if (sum(use_i) > 0){
    eol <- '\\r\\n'
  } else {
    use_i <- stringr::str_detect(
      x,
      '\\n'
    )
    if (sum(use_i) > 0){
      eol <- '\\n'
    } else {
      eol <- '\\r'
    }
  }
  
  eol
  
}








# Is the EDI Data Repository accessible?
#
ping_edi <- function() {
  r <- tryCatch(
    httr::RETRY("GET", url = "https://pasta.lternet.edu/package/eml/edi/759", quiet = TRUE), # Warn if EDI is down
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (is.logical(r)) {
    stop(
      "The EDI Repository is down for regular maintenance (Wednesday 01:00",
      " - 03:00 UTC). If you have reached this message outside maintenance",
      " hours, then there is an unexpected issue that will be resolved ",
      "shortly. Our apologies for the inconvenience. Please try again ",
      "later."
    )
  }
}








# Get previous data package version
#
# @param package.id (character) Data package identifier
#
# @return (character) Previous data package version
# 
# @details Supports repository specific methods.
# 
get_previous_version <- function(package.id) {
  
  # Load Global Environment config
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Repository specific methods
  
  if (repository == "EDI") {
    
    parts <- unlist(stringr::str_split(package.id, "\\."))
    
    vers <- suppressMessages(
      api_list_data_package_revisions(
        scope = parts[1],
        identifier = parts[2],
        environment = environment))
    
    if (length(vers) == 1) {
      return("0")
    }
    
    parts[3] <- vers[length(vers) - 1]
    parts <- paste(parts, collapse = ".")
    return(parts)
    
  }
}








# Increment data package version number
#
# @param package.id (character) Data package identifier
#
# @return (character) Package identifier with version number incremented by 1.
# 
# @details Supports repository specific methods.
#
# 
increment_package_version <- function(package.id) {
  
  # Load Global Environment config
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  # Repository specific methods
  
  if (repository == "EDI") {
    
    parts <- unlist(stringr::str_split(package.id, "\\."))
    parts[3] <- as.character(as.numeric(parts[3]) + 1)
    parts <- paste(parts, collapse = ".")
    return(parts)
    
  }
}








# Is empty nodeset?
#
# @param nodeset (xml_nodeset) Any nodeset returned by the xml2 library
# 
# @return (logical) TRUE if nodeset length = 0
# 
is_empty_nodeset <- function(nodeset) {
  res <- length(nodeset) == 0
  return(res)
}








# Join observation, location, and taxon tables in a wide format
#
# @param dt_obs
#     (data.frame) The observation table
# @param dt_loc 
#     (data.frame) The location table
# @param dt_tax 
#     (data.frame) The taxon table
# @return
#     (data.frame) Joined observation, location, and taxon tables in a wide 
#     format, where the location table has been unnested for latitude, 
#     longitude, and site names.
# 
join_obs_loc_tax <- function(dt_obs, dt_loc, dt_tax) {
  
  # Flatten location
  
  dt_loc_expanded <- flatten_location_dwca(dt_loc)
  
  # Left join
  # Left join observation, location (expanded), and taxon tables
  # TO DO: ask Colin for his naming convention for temporary objects
  
  temp <- dplyr::left_join(
    dplyr::left_join(
      dt_obs, dt_loc_expanded, 
      by = "location_id"), 
    dt_tax, by = "taxon_id")
  
  return(temp)
  
}








# Read ecocomDP criteria
#
# @return (data.frame) ecocomDP criteria
# 
read_criteria <- function() {
  res <- data.table::fread(
    system.file('extdata', 'validation_criteria.txt', package = 'ecocomDP'))
  return(res)
}









# Read EML metadata from a data repository
# 
# @description A wrapper function to repository specific read methods (the repository arg drives the logic).
# 
# @param package.id (character) Data package identifier
#
# @return (xml_document, xml_node) EML metadata
# 
read_eml <- function(package.id) {
  
  # Load Global Environment config
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Get EML
  
  if (repository == "EDI") {
    eml <- api_read_metadata(package.id, environment)
  }
  
  return(eml)
  
}








# Read EML into an emld list object
#
# @description  
#     This function wraps \code{EML::read_eml()} with a layer of quality 
#     control ensuring the returned emld list object has the same structure 
#     as output by \code{EMLassemblyline::make_eml()} and can be used in 
#     EMLassemblyline workflows.
#
# @param path 
#     (character) Path to the metadata template directory and where 
#     annotations.txt will be written.
# @param eml
#     (file) An EML .xml file located at \code{path}.
#
# @return 
#     An emld list object as similarly created by \code{EML::read_eml()}.
#     
# @details
#     When representing EML in the emld list structure, nodes that have 
#     1 or more children are structured as a list of unnamed lists 
#     (e.g. \code{list(dataTable = list("dataTable1", "dataTable2"))}) and
#     this is the structure output by EMLassemblyline::make_eml(). However,
#     EML::read_eml() removes the unnamed list when the node has only 1 child
#     (e.g. \code{list(dataTable = "dataTable1")}), thereby breaking 
#     EMLassemblyline code using \code{lapply()} to parse such nodes. 
#     Currently this QC is selectively applied to annotatable nodes targeted
#     by \code{template_annotations()}:
#     
#     \itemize{
#       \item eml/dataset/dataTable
#       \item eml/dataset/dataTable/attributeList/attribute
#       \item eml/dataset/otherEntity
#       \item eml/dataset/creator
#       \item eml/dataset/contact
#       \item eml/dataset/associatedParty
#       \item eml/dataset/project/personnel
#       \item eml/dataset/project/relatedProject
#       \item eml/dataset/project/relatedProject/personnel
#     }
#     
#     Extending support to all nodes capable of having 1 or more children is
#     welcomed.
# 
# 
EAL_read_eml <- function(path, eml) {
  
  # Create the emld list object
  eml <- EML::read_eml(paste0(path, "/", eml))
  
  # A helper function to wrap target nodes in list(). There are two 
  # implementations, one for target nodes and a second for nested target nodes.
  #
  # Arguments:
  # eml = emld list object
  # path.parent = path of the parent node using the "$" subsetting character
  # path.child = path of child node
  list_it <- function(eml, path.parent, path.child = NULL) {
    if (is.null(path.child)) {
      e <- eval(parse(text = path.parent))
      if ((length(e) > 1) & (!is.null(names(e)))) {
        eval(parse(text = paste0(path.parent, " <- list(e)")))
      }
    } else if (!is.null(path.child)) {
      for (k in seq_along(eval(parse(text = path.parent)))) {
        if ((length(eval(parse(text = paste0(path.parent, "[[", k, "]]", path.child)))) > 1) &
            (!is.null(names(eval(parse(text = paste0(path.parent, "[[", k, "]]", path.child))))))) {
          strng <- paste0(path.parent, "[[", k, "]]",
                          path.child, " <- list(",
                          paste0(path.parent, "[[", k, "]]", path.child), ")")
          eval(parse(text = strng))
        }
      }
    }
    eml
  }
  
  # Fix the emld list object
  eml <- list_it(eml, "eml$dataset$dataTable")
  eml <- list_it(eml, "eml$dataset$otherEntity")
  eml <- list_it(eml, "eml$dataset$creator")
  eml <- list_it(eml, "eml$dataset$contact")
  eml <- list_it(eml, "eml$dataset$associatedParty")
  eml <- list_it(eml, "eml$dataset$project$personnel")
  eml <- list_it(eml, "eml$dataset$project$relatedProject")
  eml <- list_it(eml, "eml$dataset$project$relatedProject", "$personnel")
  eml <- list_it(eml, "eml$dataset$dataTable", "$attributeList$attribute")
  
  eml
  
}








# Read the ecocomDP dataset_summary table (from EDI)
#
# @description  
#     Use this function to read the dataset_summary table of an ecocomDP data 
#     package from the EDI Data Repository.
# 
# @param package.id
#     (character) Data package identifier of an ecocomDP dataset in the EDI
#     data repository.
#     
# @return 
#     (tibble) The dataset_summary table
#         
read_table_dataset_summary <- function(package.id){
  
  message("Reading dataset_summary table for ", package.id)
  eml <- suppressMessages(api_read_metadata(package.id))
  entity_name <- xml2::xml_text(
    xml2::xml_find_all(eml, './/dataset/dataTable/physical/objectName'))
  if (any(stringr::str_detect(entity_name, 'dataset_summary'))) {
    entity_delimiter <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter'))[
          stringr::str_detect(entity_name, 'dataset_summary')]
    entity_url <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/distribution/online/url'))[
          stringr::str_detect(entity_name, 'dataset_summary')]
    if (entity_delimiter == ',') {
      output <- data.table::fread(entity_url)
    } else if (entity_delimiter == '\\t') {
      output <- data.table::fread(entity_url)
    }
  } else {
    output <- NULL
  }
  output
  
}








remove_empty_templates <- function(x) {
  # Removes empty templates (NULL, data frames with 0 rows, or TextType of 0 
  # characters) from the list object created by template_arguments().
  # x = template_arguments()$x
  attr_tmp <- read_template_attributes()
  use_i <- rep(F, length(x$template))
  for (i in 1:length(x$template)) {
    if (is.null(x$template[[i]]$content)) {
      use_i[i] <- T
    } else {
      if (any(attr_tmp$template_name == 
              tools::file_path_sans_ext(names(x$template[i])))) {
        if ((attr_tmp$type[
          attr_tmp$template_name == 
          tools::file_path_sans_ext(names(x$template[i]))]) == "text") {
          if (sum(nchar(unlist(x$template[[i]]))) == 0) {
            use_i[i] <- T
          }
        } else if ((attr_tmp$type[
          attr_tmp$template_name == 
          tools::file_path_sans_ext(names(x$template[i]))]) == "xml") {
          if (length(x$template[[i]]$content$taxonomicClassification) == 0) {
            use_i[i] <- T
          }
        } else {
          if (nrow(x$template[[i]]$content) == 0) {
            use_i[i] <- T
          }
        }
      }
    }
  }
  if (all(use_i)) {
    x["template"] <-list(NULL)
  } else {
    x$template[use_i] <- NULL
  }
  x
}









# Make URL for PASTA+ environment
#
# @description
#     Create the URL suffix to the PASTA+ environment specified by the
#     environment argument.
#
# @param environment
#     (character) Data repository environment to perform the evaluation in.
#     Can be: 'development', 'staging', 'production'.
#
url_env <- function(environment){
  
  environment <- tolower(environment)
  if (environment == 'development'){
    url_env <- 'https://pasta-d'
  } else if (environment == 'staging'){
    url_env <- 'https://pasta-s'
  } else if (environment == 'production'){
    url_env <- 'https://pasta'
  }
  
  url_env
  
}








# Trim data object for faster tests
#
# @param dataset (list) Data list object returned by \code{read_data()}
#
# @return (list of data frames) \code{data.list} with some content removed. 
#
trim_data_for_test <- function(dataset) {
  # Remove variable_mapping so validate_mapped_id() doesn't overload target 
  # APIs
  dataset$tables$variable_mapping <- NULL
  return(dataset)
}








# Validate file names
#
# @description  
#     Identify whether input data file names exist in the specified directory.
#
# @param path 
#     (character) A character string specifying a path to the dataset working 
#     directory.
# @param data.files
#     A list of character strings specifying the names of the data files of 
#     your dataset.
# 
# @return 
#     A warning message if the data files don't exist at path, and which of
#     the input data files are missing.
#     
#     The full names of files listed in the data.files argument.
#
validate_file_names <- function(path, data.files){
  
  # Validate file presence
  
  # Index data.files in path
  files <- list.files(path)
  use_i <- data.files %in% files
  
  # Throw an error if any data.files are missing
  if (sum(use_i) != length(data.files)){
    stop(
      paste0(
        "\nThese files don't exist in the specified directory:\n", 
        paste(data.files[!use_i], collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  # Check file naming convention
  
  # Index file names that are not composed of alphanumerics and underscores
  use_i <- stringr::str_detect(
    string = tools::file_path_sans_ext(data.files), 
    pattern = "([:blank:]|([:punct:]^_))"
  )
  
  # Issue warning if this best practice is not followed
  if (any(isTRUE(use_i))) {
    warning(
      paste0(
        "Composing file names from only alphanumerics and underscores is a ",
        "best practice. These files don't follow this recommendation:\n",
        paste(data.files[use_i], collapse = "\n"),
        "\nPlease consider renaming these files."
      ),
      call. = FALSE
    )
  }
  
  # Get file names
  
  files <- list.files(path)
  use_i <- stringr::str_detect(string = files,
                               pattern = stringr::str_c("^", data.files, collapse = "|"))
  data_files <- files[use_i]
  
  # Reorder file names to match input ordering
  
  data_files_out <- c()
  for (i in 1:length(data.files)){
    use_i <- stringr::str_detect(string = data_files,
                                 pattern = stringr::str_c("^", data.files[i], collapse = "|"))
    data_files_out[i] <- data_files[use_i]
  }
  
  data_files_out
  
}








# Validate path
#
# @description  
#     Use \code{dir.exists} to determine whether the input path is valid and 
#     returns an error message if not.
#
# @param path 
#     A character string specifying a path to the dataset working directory.
# 
# @return 
#     A warning message if the path leads to a non-existant directory.
#
validate_path <- function(path){
  
  # Validate path
  
  if (!dir.exists(path)){
    stop('The directory specified by the argument "path" does not exist! Please enter the correct path for your dataset working directory.')
  }
  
}









# Search for an LTER Controlled Vocabulary term
#
# @param x 
#     (character) A term to search for.
# @param messages 
#     (logical) Display diagnostic messages, e.g. alternative spelling options.
# @param interactive 
#     (logical) Query user to select from alternative terms and returns back
#     selection.
#
# @return 
#     Logical value (TRUE/FALSE) indicating whether the searched term could
#     be found. 
#     
#     If messages = TRUE, then alternative spellings and near misses 
#     are displayed. 
#     
#     If interactive mode = TRUE, then a user selected term is returned.
#
vocab_lter_term <- function(x, messages = FALSE, interactive = FALSE){
  
  # The LTER controlled vocabulary produces different results for a standard
  # search and fuzzy (similar) search. Both searches are run and results 
  # combined, then direct matches sought and if not found then all results
  # are presented as near misses.
  
  # Check arguments -----------------------------------------------------------
  
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (length(x) != 1){
    stop('Input argument "x" has a length > 1! Only single terms are allowed.')
  }
  if (!missing(messages) & isTRUE(messages) & !missing(interactive) & isTRUE(interactive)){
    stop('Both arguments "messages" & "interactive" can not be used at the same time. Please select one or the other.')
  }
  
  # Construct the query and search --------------------------------------------
  
  term <- stringr::str_replace_all(
    string = x, 
    pattern = ' ', 
    replacement = '+'
  )
  
  # Standard search
  
  search_output <- xml2::read_xml(
    paste0(
      'http://vocab.lternet.edu/vocab/vocab/services.php/?task=search&arg=',
      term
    )
  )
  
  # Fuzzy search
  
  fuzzy_output <- xml2::read_xml(
    paste0(
      'http://vocab.lternet.edu/vocab/vocab/services.php/?task=fetchSimilar&arg=',
      term
    )
  )
  
  # Parse the responses and combine -------------------------------------------
  
  
  
  term_list <- c()
  
  # Get standard terms
  
  if (length(xml2::xml_find_all(search_output, './/result')) != 0){
    nodeset <- xml2::xml_find_all(search_output, './/result/term/string')
    node_terms <- xml2::xml_text(nodeset)
    term_list <- c(term_list, node_terms)
  }
  
  # Get fuzzy terms
  
  if (length(xml2::xml_find_all(fuzzy_output, './/result')) != 0){
    nodeset <- xml2::xml_find_all(fuzzy_output, './/result/string')
    node_terms <- xml2::xml_text(nodeset)
    term_list <- c(term_list, node_terms)
  }
  
  # Remove duplicates
  
  term_list <- unique(term_list)
  
  # Is the search term listed? ------------------------------------------------
  
  if (sum(term_list == x) == 1){
    term_found <- T
  } else {
    term_found <- F
  }
  
  # Report near misses --------------------------------------------------------
  
  if (!missing(messages) & isTRUE(messages) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    msg <- message(
      paste0(
        'The term "',
        x,
        '" could not be found in the LTER Controlled Vocabulary. Possible alternatives:',
        '\n',
        paste0(
          term_list, 
          collapse = '\n'
        ),
        '\n'
      )
    )
    
  }
  
  # Interactive mode ----------------------------------------------------------
  
  if (!missing(interactive) & isTRUE(interactive) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    msg <- message(
      paste0(
        'The term "',
        x,
        '" could not be found in the LTER Controlled Vocabulary. Possible alternatives:',
        '\n'
      )
    )
    
    term_list <- c(term_list, 'NONE OF THE ABOVE')
    
    print.data.frame(as.data.frame(term_list))
    answer <- readline('Enter the row number of the term you would like to use: ')
    alternative_term <- as.character(term_list[as.numeric(answer)])
    message(paste0('You selected ... ', alternative_term, '\n'))
    
  }
  
  # Output results ------------------------------------------------------------
  
  if (!missing(interactive) & isTRUE(interactive) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    alternative_term
    
  } else {
    
    term_found
    
  }
  
}









# Resolve terms to a controlled vocabulary
#
# @description  
#     Resolve terms to a controlled vocabulary.
#
# @param x
#     (character) Term(s) to resolve to a controlled vocabulary. Can be a 
#     vector of terms.
# @param cv
#     (character) A controlled vocabulary to search. Valid options are:
#     \itemize{
#         \item lter - The LTER Controlled Vocabulary (http://vocab.lternet.edu/vocab/vocab/index.php)
#     }
# @param messages 
#     (logical) Display diagnostic messages, e.g. alternative spelling options.
# @param interactive 
#     (logical) Query user to select from alternative terms and returns back
#     selection.
#
# @return 
#     (character) Controlled vocabulary names corresponding to successfully
#     resolved terms.
#
vocab_resolve_terms <- function(x, cv, messages = FALSE, interactive = FALSE){
  
  # Check arguments
  
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (cv != 'lter'){
    stop('Input argument "cv" is not one of the allowed vocabularies!')
  }
  if (!missing(messages) & isTRUE(messages) & !missing(interactive) & isTRUE(interactive)){
    stop('Both arguments "messages" & "interactive" can not be used at the same time. Please select one or the other.')
  }
  
  # Initialize output
  
  output <- data.frame(
    term = x,
    controlled_vocabulary = character(length(x)),
    stringsAsFactors = F)
  
  # Call specified vocabularies
  
  if (cv == 'lter'){
    
    if (!missing(messages) & isTRUE(messages)){
      # Messages
      use_i <- unlist(lapply(x, FUN = vocab_lter_term, messages = T))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
    } else if (!missing(interactive) & isTRUE(interactive)){
      # Interactive
      alternative_terms <- unlist(lapply(x, FUN = vocab_lter_term, interactive = T))
      use_i <- ((alternative_terms == 'NONE OF THE ABOVE') | (is.na(alternative_terms)))
      output[!use_i, 'term'] <- alternative_terms[!use_i]
      output$term[output$term == 'TRUE'] <- x[output$term == 'TRUE']
      output[!use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
      use_i <- output$term == FALSE
      output$term[use_i] <- x[use_i]
      output$controlled_vocabulary[use_i] <- ''
    } else {
      # Automatic
      use_i <- unlist(lapply(x, FUN = vocab_lter_term))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'
    }
    
  }
  
  # Return output
  
  output
  
}








#' Write tables to file
#'
#' @param path (character) A path to the directory in which the files will be written.
#' @param sep (character) Field delimiter to use when writing files. Default is comma.
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param location (tbl_df, tbl, data.frame) The location table.
#' @param taxon (tbl_df, tbl, data.frame) The taxon table.
#' @param dataset_summary (tbl_df, tbl, data.frame) The dataset_summary table.
#' @param observation_ancillary (tbl_df, tbl, data.frame) The observation_ancillary table.
#' @param location_ancillary (tbl_df, tbl, data.frame) The location_ancillary table.
#' @param taxon_ancillary (tbl_df, tbl, data.frame) The taxon_ancillary table.
#' @param variable_mapping (tbl_df, tbl, data.frame) The variable_mapping table.
#'
#' @return ecocomDP tables as \code{sep} delimited files
#'     
#' @export
#' 
#' @examples 
#' # Create directory for the tables
#' mypath <- paste0(tempdir(), "/data")
#' dir.create(mypath)
#' 
#' # Create a couple inputs to write_tables()
#' 
#' flat <- ants_L0_flat
#' 
#' observation <- create_observation(
#'   L0_flat = flat, 
#'   observation_id = "observation_id", 
#'   event_id = "event_id", 
#'   package_id = "package_id",
#'   location_id = "location_id", 
#'   datetime = "datetime", 
#'   taxon_id = "taxon_id", 
#'   variable_name = "variable_name",
#'   value = "value",
#'   unit = "unit")
#' 
#' observation_ancillary <- create_observation_ancillary(
#'   L0_flat = flat,
#'   observation_id = "observation_id", 
#'   variable_name = c("trap.type", "trap.num", "moose.cage"))
#' 
#' # Write tables to file
#' 
#' write_tables(
#'   path = mypath, 
#'   observation = observation, 
#'   observation_ancillary = observation_ancillary)
#' 
#' dir(mypath)
#' 
#' # Clean up
#' unlink(mypath, recursive = TRUE)
#'
write_tables <- function(
  path, sep = ",", observation = NULL, location = NULL, 
  taxon = NULL, dataset_summary = NULL, observation_ancillary = NULL,
  location_ancillary = NULL, taxon_ancillary = NULL, variable_mapping = NULL) {
  
  # Validate arguments
  
  if (missing(path)){
    stop('Input argument "path" is required.', call. = FALSE)
  }
  
  # Write tables to file
  
  message('Writing tables to file:')
  
  if (sep == ",") {
    suffix <- ".csv"
  } else {
    suffix <- ".txt"
  }
  
  if (!is.null(observation)) {
    message("  observation")
    f <- paste0(path, "/", paste0("observation", suffix))
    data.table::fwrite(x = observation, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(location)) {
    message("  location")
    f <- paste0(path, "/", paste0("location", suffix))
    data.table::fwrite(x = location, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(taxon)) {
    message("  taxon")
    f <- paste0(path, "/", paste0("taxon", suffix))
    data.table::fwrite(x = taxon, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(dataset_summary)) {
    message("  dataset_summary")
    f <- paste0(path, "/", paste0("dataset_summary", suffix))
    data.table::fwrite(x = dataset_summary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(observation_ancillary)) {
    message("  observation_ancillary")
    f <- paste0(path, "/", paste0("observation_ancillary", suffix))
    data.table::fwrite(x = observation_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(location_ancillary)) {
    message("  location_ancillary")
    f <- paste0(path, "/", paste0("location_ancillary", suffix))
    data.table::fwrite(x = location_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(taxon_ancillary)) {
    message("  taxon_ancillary")
    f <- paste0(path, "/", paste0("taxon_ancillary", suffix))
    data.table::fwrite(x = taxon_ancillary, file = f, sep = sep, na = "NA")
  }
  
  if (!is.null(variable_mapping)) {
    message("  variable_mapping")
    f <- paste0(path, "/", paste0("variable_mapping", suffix))
    data.table::fwrite(x = variable_mapping, file = f, sep = sep, na = "NA")
  }
  
}











# Get XML values
#
# @param nodeset (xml_node/xml_nodeset) Nodeset
# @param xpath (character) xpath
# 
# @return (character) Value of \code{xpath} within \code{nodeset}. Returns "" if returned character string has length = 1.
# 
# @details Simplifies code by wrapping \code{cnvmt(xml2::xml_text(xml2::xml_find_all(...), trim = T))}
# 
xml_val <- function(nodeset, xpath) {
  res <- cnvmt(
    xml2::xml_text(
      xml2::xml_find_all(nodeset, xpath),
      trim = T))
  return(res)
}








