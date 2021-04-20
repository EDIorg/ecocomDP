#' Get provenance metadata
#'
#' @description
#'     Add Provenance Metadata from Level-1 metadata in PASTA to an XML 
#'     document containing a single methods element in the request message 
#'     body.
#'
#' @usage api_get_provenance_metadata(package.id, environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     ("xml_document" "xml_node") EML metadata.
#'     
#'
api_get_provenance_metadata <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving provenance metadata for ', package.id))
  
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








#' List data package revisions
#'
#' @description
#'     List Data Package Revisions operation, specifying the scope and 
#'     identifier values to match in the URI. The request may be filtered by 
#'     applying the modifiers “oldest” or “newest” to the “filter” query 
#'     parameter.
#'
#' @usage api_list_data_package_revisions(scope, identifier, filter = NULL, 
#'     environment = 'production')
#'
#' @param scope
#'     (character) Data package scope (e.g. 'edi', 'knb-lter-bnz').
#' @param identifier
#'     (character) Data package identifier (e.g. '100', '275').
#' @param filter
#'     (character) Filter to data packages, can be 'oldest' or 'newest'.
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     (character) Vector of revisions, if more than one exists, otherwise a 
#'     single revision number.
#'
api_list_data_package_revisions <- function(scope, identifier, filter = NULL, environment = 'production'){
  
  message(paste('Retrieving data package revisions for', 
                paste(scope, '.', identifier)
  )
  )
  
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
    
    output <- read.csv(
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








#' Read data entity name
#'
#' @description
#'     Read Data Entity Name operation, specifying the scope, identifier, 
#'     revision, and entity identifier of the data entity whose name is 
#'     to be read in the URI.
#'
#' @usage api_read_data_entity_name(package.id, identifier, 
#'     environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param identifier
#'     (character) Data entity identifier (e.g. 
#'     5c224a0e74547b14006272064dc869b1)
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     (character) Data entity name
#'
api_read_data_entity_name <- function(package.id, identifier, environment = 'production'){
  
  message(paste('Retrieving name of', package.id, identifier))
  
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
    read.csv(
      text = c(
        'identifier',
        r
      ),
      as.is = T
    )$identifier
  )
  
  output
  
}








#' Read data package
#'
#' @description
#'     Read Data Package operation, specifying the scope, identifier, and 
#'     revision of the data package to be read in the URI, returning a resource 
#'     map with reference URLs to each of the metadata, data, and quality 
#'     report resources that comprise the data package.
#'
#' @usage api_read_data_package(package.id, 
#'     environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     (character) Reference URLs.
#'     
api_read_data_package <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving resource map for', package.id))
  
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
    read.csv(
      text = c(
        'identifier',
        r
      ),
      as.is = T
    )$identifier
  )
  
  output
  
}








#' Read data package DOI
#'
#' @description
#'     Read Data Package DOI operation, specifying the scope, identifier, 
#'     and revision of the data package DOI to be read in the URI.
#'
#' @usage api_read_data_package_doi(package.id, environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     (character) The canonical Digital Object Identifier.
#'
api_read_data_package_doi <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving DOI for', package.id))
  
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








#' Read metadata
#'
#' @description
#'     Read Metadata (EML) operation, specifying the scope, identifier, and 
#'     revision of the EML document to be read in the URI.
#'
#' @usage api_read_metadata(package.id, environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     ('xml_document' 'xml_node') EML metadata.
#'     
#'
api_read_metadata <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving EML for data package', package.id))
  
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








#' Parse character string to POSIXct POSIXt
#'
#' @param datetimes (character) Date or datetime
#'
#' @return (POSIXct POSIXt) \code{datetimes} parsed by relevant \code{lubridate} function
#' 
char2datetime <- function(datetimes) {
  datetimes <- stringr::str_remove(datetimes, "T.*$")
  datetimes <- datetimes[!is.na(datetimes)]
  res <- lubridate::parse_date_time(datetimes, orders = c("ymd", "ymd H", "ymd HM", "ymd HMS"))
  return(res)
}









#' Convert \code{character(0)} to \code{""}
#'
#' @param txt (character) Character string
#'
#' @return (character) Return \code{txt} if not empty else ""
#'
cnvmt <- function(txt) {
  if (length(txt) == 0) {
    return("")
  } else {
    return(txt)
  }
}








#' Detect field delimiter of file
#'
#' @param path (character) Path in which \code{data.files} are found
#' @param data.files (character) File names
#' @param os (character) Return from \code{detect_os()}.
#'
#' @details Parses the verbose return from \code{data.table::fread()} to get the delimiter value. If this fails, then a secondary function is called utilizing the suggested \code{reader} package. If this secondary approach fails, then a default "," is returned.
#'
#' @return (character) Field delimiter of \code{data.files}
#'
detect_delimiter <- function(path, data.files, os) {
  f <- paste0(path, "/", data.files)
  msg <- capture.output(data.table::fread(f, verbose = TRUE) %>% {NULL}) # primary method
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








#' Get field delimiters of input files (method 2)
#'
#' @description  
#'     Detect and return field delimiters of input files (tables).
#'
#' @usage 
#' detect_delimiter(
#'   path, 
#'   data.files, 
#'   os
#' )
#' 
#' @param path 
#'     (character) Path to files.
#' @param data.files
#'     (character) File names.
#' @param os
#'     (character) Operating system. Valid options are returned from  
#'     \code{EDIutils::detect_os}.
#' 
#' @return 
#'     (character) Field delimiters of input files.
#'     \item{"\\t"}{tab}
#'     \item{","}{comma}
#'     \item{";"}{semi-colon}
#'     \item{"|"}{pipe}
#'
detect_delimiter_method_2 <- function(path, data.files, os){
  
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







#' Detect operating system
#'
#' @description  
#'     This function uses \code{Sys.info} to detect the user's operating system 
#'     and outputs an abbreviated character string to be used as inputs to OS
#'     specific function calls.
#'
#' @usage detect_os()
#' 
#' @return 
#'     \item{win}{Windows OS}
#'     \item{mac}{Mac OS}
#'
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








#' Get metadata for an attribute
#'
#' @description
#'     Get attribute metadata
#'
#' @usage get_eml_attribute(attr.name, package.id)
#'
#' @param attr.name
#'     (character) Attribute name
#' @param package.id
#'     (character) Data package identifier (e.g. knb-lter-cap.627.3) in the 
#'     EDI Data Repository.
#' @param evironment (character) Repository environment in which the L0 and L1 exist. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows execution for the \code{update_L1} workflow within the context of one of these environments. Default is "production".
#'
#' @return 
#'     (list) A named vector with these attribute elements:
#'     \itemize{
#'         \item{name} <attributeName>
#'         \item{definition} <attributeDefinition> 
#'         \item{unit} <standardUnit> or <customUnit>
#'     }
#'
get_eml_attribute <- function(attr.name, package.id, environment = "production"){
  # FIXME: Input should be EML not package.id
  # Send message
  
  if (!is.na(attr.name)){
    message(paste0('Searching ', package.id, ' for "', attr.name, '"'))
  }
  
  # Load EML
  
  metadata <- suppressMessages(api_read_metadata(package.id, environment = environment))
  
  # Get definition
  
  definition <- try(
    xml2::xml_text(
      xml2::xml_find_all(
        x = metadata, 
        xpath = paste0(
          "//attributeDefinition[ancestor::attribute[child::attributeName[text() = '",
          attr.name,
          "']]]"
        )
      )
    ),
    silent = TRUE
  )
  
  # Get standard unit
  
  unit <- try(
    xml2::xml_text(
      xml2::xml_find_all(
        x = metadata, 
        xpath = paste0(
          "//standardUnit[ancestor::attribute[child::attributeName[text()  = '",
          attr.name,
          "']]]"
        )
      )
    ),
    silent = TRUE
  )
  
  # Get custom unit
  
  if (identical(unit, character(0))){
    unit <- try(
      xml2::xml_text(
        xml2::xml_find_all(
          x = metadata, 
          xpath = paste0(
            "//customUnit[ancestor::attribute[child::attributeName[text()  = '",
            attr.name,
            "']]]"
          )
        )
      ),
      silent = TRUE
    )
  }
  
  # Parse results
  
  if (class(definition) != 'character'){
    definition <- NA_character_
  } else {
    definition <- definition[1]
  }
  
  if (class(unit) != 'character'){
    unit <- NA_character_
  } else {
    unit <- unit[1]
  }
  
  # Return results
  
  list(
    name = attr.name,
    definition = definition,
    unit = unit
  )
  
}








#' Get end of line (EOL) character
#'
#' @description
#'     Get EOL character of input file(s).
#'
#' @usage get_eol(path, file.name, os)
#'
#' @param path
#'     (character) A path to the target file directory.
#' @param file.name
#'     (character) The target file name.
#' @param os
#'     (character) The operating system in which this function is called
#'     called. Valid options are generated from \code{detect_os}.
#'
#' @return
#'     A character string representation of the EOL character.
#'
get_eol <- function(path, file.name, os){
  
  # Check arguments
  
  validate_arguments(x = as.list(environment()))
  
  # Validate file.name
  
  file_name <- validate_file_names(path, file.name)
  
  # Detect end of line character
  
  if (os == 'mac'){ # Macintosh OS
    
    command <- paste0(
      'od -c "',
      path,
      '/',
      file.name,
      '"'
    )
    
    output <- system(
      command,
      intern = T
    )
    
    use_i <- stringr::str_detect(
      output,
      '\\\\r  \\\\n'
    )
    
    if (sum(use_i) > 0){
      eol <- '\\r\\n'
    } else {
      use_i <- stringr::str_detect(
        output,
        '\\\\n'
      )
      if (sum(use_i) > 0){
        eol <- '\\n'
      } else {
        eol <- '\\r'
      }
    }
    
  } else if ((os == 'win') | (os == 'lin')){ # Windows & Linux OS
    
    output <- readChar(
      paste0(
        path,
        '/',
        file.name
      ),
      nchars = 10000
    )
    
    eol <- parse_delim(output)
    
  }
  
  eol
  
}








#' Does dataset ID belong to EDI?
#'
#' @param id (character) Dataset identifier 
#'
#' @return (logical) TRUE if is an EDI identifier, otherwise FALSE
#'
is_edi <- function(id) {
  res <- stringr::str_detect(
    string = id, 
    pattern = paste0("(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|",
                     "(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)"))
  return(res)
}








#' Does dataset ID belong to NEON?
#'
#' @param id (character) Dataset identifier 
#'
#' @return (logical) TRUE if is a NEON identifier, otherwise FALSE
#'
is_neon <- function(id) {
  res <- stringr::str_detect(string = id, pattern = "^neon\\.")
  return(res)
}








#' Is provenance node?
#'
#' @param nodeset (xml_nodeset) methods nodeset at \code{/eml:eml/dataset/methods/methodStep}
#' 
#' @details Looks for provenance in \code{./dataSource}
#' 
#' @return (logical) TRUE if nodeset has provenance
#' 
is_prov <- function(nodeset) {  
  dasource <- xml2::xml_find_all(nodeset, "./dataSource")
  res <- !is_empty_nodeset(dasource)
  return(res)
}








#' Parse datetime
#'
#' @param tbl (character) Table name \code{vals} come from. This is used in warning messages.
#' @param vals (character) Vector of datetimes
#' @param frmt (character) Datetime format string
#' 
#' @details A wrapper to to \code{lubridate::ymd()}, \code{lubridate::ymd_h()}, \code{lubridate::ymd_hm()}, and \code{lubridate::ymd_hms()}.
#' 
#' @note No attempt at using the time zone component is made
#' 
#' @return (POSIXct POSIXt) Datetimes parsed
#' 
parse_datetime <- function(tbl, vals, frmt) {
  vals <- as.character(vals)
  na_i <- sum(is.na(vals))
  vals <- stringr::str_remove_all(vals, "(Z|z).+$")
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
  na_f <- sum(is.na(parsed))
  if (na_f > na_i) {
    warning((na_f - na_i), " ", tbl, " datetime strings failed to parse. Use parse.datetime = 'FALSE' to get datetimes as character strings for manual processing.", call. = FALSE)
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











#' Get previous data package version
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Previous data package version
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' \dontrun{
#' #' get_previous_data_package_version("edi.100.2")
#' }
#' 
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








#' Increment data package version number
#'
#' @param package.id (character) Data package identifier
#'
#' @return (character) Package identifier with version number incremented by 1.
#' 
#' @details Supports repository specific methods.
#'
#' @examples
#' increment_package_version("edi.100.1")
#' 
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








#' Is empty nodeset?
#'
#' @param nodeset (xml_nodeset) Any nodeset returned by the xml2 library
#' 
#' @return (logical) TRUE if nodeset length = 0
#' 
is_empty_nodeset <- function(nodeset) {
  res <- length(nodeset) == 0
  return(res)
}








#' Read ecocomDP criteria
#'
#' @return (data.frame) ecocomDP criteria
#' 
read_criteria <- function() {
  res <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  return(res)
}








#' Read example dataset from /data
#'
#' @return (list) Example dataset
#'
read_example_dataset <- function() {
  fname <- system.file("/data/example_dataset.rds", package = "ecocomDP")
  res <- read_data(from.file = fname)
  return(res)
}








#' Read the ecocomDP dataset_summary table (from EDI)
#'
#' @description  
#'     Use this function to read the dataset_summary table of an ecocomDP data 
#'     package from the EDI Data Repository.
#' 
#' @param package.id
#'     (character) Data package identifier of an ecocomDP dataset in the EDI
#'     data repository.
#'     
#' @return 
#'     (tibble) The dataset_summary table
#'         
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









#' Make URL for PASTA+ environment
#'
#' @description
#'     Create the URL suffix to the PASTA+ environment specified by the
#'     environment argument.
#'
#' @usage url_env(environment)
#'
#' @param environment
#'     (character) Data repository environment to perform the evaluation in.
#'     Can be: 'development', 'staging', 'production'.
#'
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








#' Validate file names
#'
#' @description  
#'     Identify whether input data file names exist in the specified directory.
#'
#' @usage validate_file_names(path, data.files)
#' 
#' @param path 
#'     (character) A character string specifying a path to the dataset working 
#'     directory.
#' @param data.files
#'     A list of character strings specifying the names of the data files of 
#'     your dataset.
#' 
#' @return 
#'     A warning message if the data files don't exist at path, and which of
#'     the input data files are missing.
#'     
#'     The full names of files listed in the data.files argument.
#'
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








#' Validate path
#'
#' @description  
#'     Use \code{dir.exists} to determine whether the input path is valid and 
#'     returns an error message if not.
#'
#' @usage validate_path(path)
#' 
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' 
#' @return 
#'     A warning message if the path leads to a non-existant directory.
#'
validate_path <- function(path){
  
  # Validate path
  
  if (!dir.exists(path)){
    stop('The directory specified by the argument "path" does not exist! Please enter the correct path for your dataset working directory.')
  }
  
}








#' Resolve terms to a controlled vocabulary
#'
#' @description  
#'     Resolve terms to a controlled vocabulary.
#'
#' @usage 
#'     vocab_resolve_terms(x, cv, messages = FALSE, interactive = FALSE)
#'
#' @param x
#'     (character) Term(s) to resolve to a controlled vocabulary. Can be a 
#'     vector of terms.
#' @param cv
#'     (character) A controlled vocabulary to search. Valid options are:
#'     \itemize{
#'         \item{lter} - The LTER Controlled Vocabulary (http://vocab.lternet.edu/vocab/vocab/index.php)
#'     }
#' @param messages 
#'     (logical) Display diagnostic messages, e.g. alternative spelling options.
#' @param interactive 
#'     (logical) Query user to select from alternative terms and returns back
#'     selection.
#'
#' @return 
#'     (character) Controlled vocabulary names corresponding to successfully
#'     resolved terms.
#'
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










#' Get XML values
#'
#' @param nodeset (xml_node/xml_nodeset) Nodeset
#' @param xpath (character) xpath
#' 
#' @return (character) Value of \code{xpath} within \code{nodeset}. Returns "" if returned character string has length = 1.
#' 
#' @details Simplifies code by wrapping \code{cnvmt(xml2::xml_text(xml2::xml_find_all(...), trim = T))}
#' 
xml_val <- function(nodeset, xpath) {
  res <- cnvmt(
    xml2::xml_text(
      xml2::xml_find_all(nodeset, xpath),
      trim = T))
  return(res)
}