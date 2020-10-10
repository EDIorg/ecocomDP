#' Make EML metadata for a DWcA occurrence from an ecocomDP data package
#'
#' @param path 
#'     (character) Path to the directory containing ecocomDP data tables, 
#'     conversion scripts, and where EML metadata will be written.
#' @param data.table
#'     (character) DWcA table to create EML for. Can be: "event.csv", 
#'     "occurrence.csv", or "measurementOrFact.csv".
#' @param data.table.url
#'     (character; optional) The publicly accessible URL from which 
#'     \code{data.table} can be downloaded. If more than one, then supply as 
#'     a vector of character strings in the same order as listed in 
#'     \code{data.table}. If wanting to include URLs for some but not all
#'     \code{data.table}, then use a "" for those that don't have a URL
#'     (e.g. \code{data.table.url = c("", "/url/to/decomp.csv")}).
#' @param contact
#'    (data frame) Contact information of person creating this DWcA occurrence 
#'    record with these columns:
#'    \itemize{
#'        \item{givenName}
#'        \item{surName}
#'        \item{organizationName}
#'        \item{electronicMailAddress}
#'    }
#' @param parent.package.id
#'     (character) ID of an ecocomDP data package. Only 
#'     EDI Data Repository package IDs are currently supported.
#' @param child.package.id
#'     (character) ID of DWcA occurrence data package being created.
#' @param user.id
#'     (character; optional) Repository user identifier. If more than one, 
#'     then enter as a vector of character strings (e.g. 
#'     \code{c("user_id_1", "user_id_2")}). \code{user.id} sets the 
#'     /eml/access/principal element for all \code{user.domain} except
#'     "KNB", "ADC", and if \code{user.domain = NULL}.
#' @param user.domain
#'     (character; optional) Repository domain associated with 
#'     \code{user.id}. Currently supported values are "EDI" 
#'     (Environmental Data Initiative), "LTER" (Long-Term Ecological Research 
#'     Network), "KNB" (The Knowledge Network for Biocomplexity), "ADC" (The 
#'     Arctic Data Center). If you'd like your system supported please contact
#'     maintainers of the EMLassemblyline R package. If using more than one 
#'     \code{user.domain}, then enter as a vector of character strings (e.g. 
#'     \code{c("user_domain_1", "user_domain_2")}) in the same order as 
#'     corresponding \code{user.id}. If \code{user.domain} is missing then a 
#'     default value "unknown" is assigned. \code{user.domain} sets the EML 
#'     header "system" attribute and for all \code{user.domain}, except "KNB" 
#'     and "ADC", sets the /eml/access/principal element attributes and values.
#'
#' @return 
#'     An EML metadata record for the DWcA table defined by \code{data.table}.
#'
#' @details 
#'     This function creates an EML record for an Darwin Core Archive record
#'     (DwC-A) combining metadata from the parent data package and
#'     boiler-plate metadata describing the DwC-A tables. Changes to the 
#'     parent EML include:
#'     \itemize{
#'         \item \strong{<access>} Adds the \code{user.id} to the list of 
#'         principals granted read and write access to the DwC-A data 
#'         package this EML describes.
#'         \item \strong{<title>} Appends "Darwin Core Archive: " to the title.
#'         \item \strong{<pubDate>} Adds the date when this EML record is 
#'         created.
#'         \item \strong{<abstract>} Adds a note that this is a derived data 
#'         package in a DwC-A format.
#'         \item \strong{<keywordSet>} Essential Biodiversity Variables: 
#'         "Population Abundance" and Darwin Core Terms: 
#'         "BasisofRecord: HumanObservation", "Occurrence: OrganismQuantity",
#'         "Taxon: ScientificName".
#'         \item \strong{<intellectualRights>} Keeps intact the intellectual
#'         rights license of the parent data package, or replaces it with
#'         "CCO" (https://creativecommons.org/publicdomain/zero/1.0/legalcode).
#'         \item \strong{<contact>} Adds contact information of the DwC-A
#'         creator to the list of contacts in the parent data package EML.
#'         \item \strong{<methodStep>} Adds a note that this data package was
#'         created by methods within the ecocomDP R package and adds provenance 
#'         metadata noting that this is a derived data and describing where 
#'         the parent data package can be accessed.
#'         \item \strong{<dataTables>} Replaces the parent data package data
#'         tables metadata with boiler-plate metadata for the DwC-A tables.
#'         \item \strong{<otherEntity>} Describes the meta.xml accompanying 
#'         each DwC-A. Any other entities listed in the parent EML are removed.
#'     }
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' }
#'
make_eml_dwca <- function(
  path, data.table, data.table.url, contact, parent.package.id, 
  child.package.id, user.id, user.domain) {
  
  message("Creating DwC-A EML for derived data package (" , child.package.id, ")")
  
  # Validate inputs -----------------------------------------------------------
  
  if (missing(path)) {
    stop(
      "Input argument 'path' is missing.", "Please specify the directory to ",
      "the DwC-A table and where the EML will be written.", 
      call. = FALSE)
  }
  
  # Check the data package exists
  if (missing(parent.package.id)) {
    stop(
      "Input argument 'parent.package.id' is missing. Please specify the",
      "ID of the parent data package from which this ecocomDP was derived.",
      "NOTE: Only data packages in the EDI Data Repository are currently",
      "supported.", call. = FALSE)
  }
  
  if (missing(child.package.id)) {
    stop(
      "Input argument 'child.package.id' is missing. Please specify the",
      "ID of the data package this EML will describe.", call. = FALSE)
  }
  
  if (missing(contact)) {
    stop(
      "Input argument 'contact' is missing. Please specify the contact",
      "information for the creator of this ecocomDP data package. Supply",
      "this information as a data frame with the columns 'givenName',",
      "'surName', 'organizationName', 'electronicMailAddress'.", 
      call. = FALSE)
  }
  
  if (!all(
    c('givenName', 'surName', 'organizationName', 'electronicMailAddress') %in% 
    names(contact))) {
    stop(
      "Input argument 'contact' is missing required columns.", 
      "This data frame should have the columns: 'givenName',",
      "'surName', 'organizationName', 'electronicMailAddress'.",
      call. = FALSE)
  }
  
  if (missing(user.id)) {
    stop(
      "Input argument 'user.id' is missing. Please specify the user ID under",
      "which this data package will be uploaded.", call. = FALSE)
  }
  
  if (missing(user.domain)) {
    stop(
      "Input argument 'user.domain' is missing. Please specify the domain",
      "under which the 'user.id' argument exists.", call. = FALSE)
  }
  
  if (length(user.id) != length(user.domain)) {
    stop(
      "The number of items listed under the 'user.id' and",
      "'user.domain' arguments must match.", call. = FALSE)
  }
  
  # FIXME: Add check for publicly accessible 'data.url' argument
  
  # Read parent EML -----------------------------------------------------------
  
  message(paste0("Reading parent data package EML (", parent.package.id, ")"))
  
  scope <- unlist(strsplit(parent.package.id, split = ".", fixed = T))[1]
  identifier <- unlist(strsplit(parent.package.id, split = ".", fixed = T))[2]
  revision <- unlist(strsplit(parent.package.id, split = ".", fixed = T))[3]
  
  eml <- EML::read_eml(
    paste0(
      "https://pasta.lternet.edu/package/metadata/eml", "/", scope, "/",
      identifier, "/", revision))
  
  message("Updating nodes ...")
  message("<eml>")
  
  # Create (some) child EML ---------------------------------------------------
  
  # TODO: Run make_eml() to create child EML elements to be added with the 
  # parent EML
  
  # Update <access> of parent -------------------------------------------------
  # Update <access> to enable repository upload by ecocomDP 
  # creator(s)/maintainer(s).
  
  # TODO: Update LDAP handling in EMLassemblyline and add the corresponding 
  # access control rules
  
  message("  <access>")
  
  for (i in 1:length(user.id)){
    
    if (user.domain[i] == 'LTER'){
      
      eml$access$allow[[length(eml$access$allow) + 1]] <- list(
        principal = paste0(
          'uid=', user.id[i], ',o=', user.domain[i], ',dc=ecoinformatics,dc=org'
        ),
        permission = "all"
      )
      
    } else if (user.domain[i] == 'EDI'){
      
      eml$access$allow[[length(eml$access$allow) + 1]] <- list(
        principal = paste0(
          'uid=', user.id[i], ',o=', user.domain[i], ',dc=edirepository,dc=org'
        ),
        permission = "all"
      )
      
    }
    
  }
  
  # Remove <alternateIdentifier> ----------------------------------------------
  # Remove <alternateIdentifier> to prevent downstream errors
  
  eml$dataset$alternateIdentifier <- NULL
  
  # Update <dataset> ----------------------------------------------------------
  
  message("  <dataset>")
  
  # Update <title> ------------------------------------------------------------
  
  message("    <title>")
  
  # Notify the user that this is a Darwin Core Archive
  eml$dataset$title <- paste(
    eml$dataset$title, "(Reformatted to a Darwin Core Archive)")
  
  # Update <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  
  eml$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")
  
  # Updating <abstract> -------------------------------------------------------
  
  message("    <abstract>")
  
  # Get parent abstract
  src_abstract <- unname(
    unlist(
      stringr::str_remove_all(
        eml$dataset$abstract, 
        "</?para>"
      )
    )
  )
  
  # Reset abstract node
  eml$dataset$abstract <- list()
  eml$dataset$abstract$section <- list()
  eml$dataset$abstract$para <- list()
  
  # Add boiler-plate ecocomDP (paragraph 1)
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- paste(
    "This data package is formatted according to the 'ecocomDP', a data",
    "package design pattern for ecological community surveys, and data from",
    "studies of composition and biodiversity. For more information on the",
    "ecocomDP project see https://github.com/EDIorg/ecocomDP/tree/master, or",
    "contact EDI https://environmentaldatainitiative.org."
  )
  
  # Add boiler-plate ecocomDP (paragraph 2)  
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- paste(
    "This Level-1 data package was derived from the Level-0 data package",
    "found here:",
    paste0(
      'https://portal.edirepository.org/nis/mapbrowse?scope=', scope,
      '&identifier=', identifier, '&revision=', revision
    )
  )
  
  # Add boiler-plate ecocomDP (paragraph 3)
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- paste(
    "The abstract below was extracted from the Level-0 data package and is",
    "included for context:"
  )
  
  # Add parent abstract (paragraph 4)
  for (i in 1:length(src_abstract)) {
    eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- 
      src_abstract[i]
  }
  
  # Update <keywordSet> -------------------------------------------------------
  
  message("    <keywordSet>")
  
  # TODO: Remove "ecocomDP" since this is no longer an ecocomDP data package
  
  # TODO: This looks like it will be dependent on the core type specified. Revisit this feature once we know if we will be supporting multiple Darwin core types. 
  # Read boiler-plate DwC-A keywords
  keywordSet <- read.table(
    system.file("/controlled_vocabulary.csv", package = "ecocomDP"),
    header = T, 
    sep = ",",
    as.is = T,
    na.strings = "NA")
  
  # Add to the parent keywordSet
  eml$dataset$keywordSet[[length(eml$dataset$keywordSet) + 1]] <- list(
    keywordThesaurus = keywordSet$keywordThesaurus[1],
    keyword = as.list(keywordSet$keyword))
  
  # Update <intellectualRights> -----------------------------------------------
  # Use parent intellectual rights or CC0 if none exists
  
  if (is.null(eml$dataset$intellectualRights)) {
    message("    <intellectualRights>")
    eml$dataset$intellectualRights <- EML::set_TextType(
      system.file('intellectual_rights_cc0_1.txt', package = 'ecocomDP'))
  }
  
  # Update <contact> ----------------------------------------------------------
  # Add ecocomDP creator as a contact incase questions arise
  
  message("    <contact>")
  eml$dataset$contact <- list(
    eml$dataset$contact, 
    list(
      individualName = list(
        givenName = contact$givenName,
        surName = contact$surName),
      organizationName = contact$organizationName,
      electronicMailAddress = contact$electronicMailAddress))
  
  # Update <methods> ----------------------------------------------------------
  # Update parent methods with ecocomDP creation process and provenance 
  # metadata to provide the user with a full understanding of how these data 
  # were created
  
  message("    <methods>")
  
  # Get parent methods
  src_methods <- eml$data$methods$methodStep
  
  # Reset methods node
  eml$dataset$methods <- list()
  eml$dataset$methods$methodStep <- list()
  
  # Add ecocomDP methods (paragraph 1)
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    list(
      description = list(
        para = paste0(
          "The source data package is programmatically converted into an ",
          "ecocomDP data package using the scripts: ", 
          paste(script, collapse = ", "), ". For more information on the ",
          "ecocomDP project see: ",
          "'https://github.com/EDIorg/ecocomDP/tree/master' or contact EDI ",
          "(https://environmentaldatainitiative.org). Below are the source ",
          "data methods:"
        )
      )
    )
  
  # Add parent methods (paragraph 2)
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    src_methods
  
  # Add provenance metadata (paragraph 3). Read provenance from the EDI Data 
  # Repository and remove creator and contact IDs to preempt ID clashes. 
  # Metadata is written to tempdir() so EML::read_eml() can apply its unique 
  # parsing algorithm.
  provenance <- xml2::read_xml(
    paste0(
      "https://pasta.lternet.edu/package/provenance/eml", "/", scope,
      "/", identifier, "/", revision
    )
  )
  
  xml2::xml_set_attr(
    xml2::xml_find_all(provenance, './/dataSource/creator'),
    'id', 
    NULL
  )
  
  xml2::xml_set_attr(
    xml2::xml_find_all(provenance, './/dataSource/contact'),
    'id',
    NULL
  )
  
  xml2::write_xml(
    provenance,
    paste0(tempdir(), "/provenance.xml")
  )
  
  provenance <- EML::read_eml(
    paste0(tempdir(), "/provenance.xml")
  )
  
  provenance$`@context` <- NULL
  provenance$`@type` <- NULL
  unlink(paste0(tempdir(), "/provenance.xml"))
  
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    provenance
  
  # Update <dataTable> --------------------------------------------------------
  # Combine boiler-plate ecocomDP table attributes with table specific 
  # metadata
  
  r <- suppressMessages(
    compile_attributes(path)
  )
  data_table <- list()
  
  for (i in 1:length(r$tables_found)) {
    
    message("    <dataTable>")
    
    # Remove white space from categorical variables
    cat.vars <- as.data.frame(
      apply(cat.vars, 2, trimws, which = "both"), 
      stringsAsFactors = FALSE
    )
    
    # Remove white space from attributes variables
    r$attributes[[i]] <- as.data.frame(
      apply(r$attributes[[i]], 2, trimws, which = "both"), 
      stringsAsFactors = FALSE
    )
    
    # Create the attribute list
    attributeList <- suppressWarnings(
      EML::set_attributes(
        r$attributes[[i]][ , c(
          'attributeName', 
          'formatString',
          'unit',
          'numberType',
          'definition',
          'attributeDefinition',
          'minimum',
          'maximum',
          'missingValueCode',
          'missingValueCodeExplanation')],
        factors = cat.vars,
        col_classes = r$attributes[[i]][ ,"columnClasses"]
      )
    )
    
    # Set physical
    physical <- suppressMessages(
      EML::set_physical(
        paste0(path, '/', r$tables_found[i]),
        numHeaderLines = "1",
        recordDelimiter = EDIutils::get_eol(
          path = path,
          file.name = r$tables_found[i],
          os = EDIutils::detect_os()
        ),
        attributeOrientation = "column",
        url = 'placeholder'
      )
    )
    
    if (!is.null(data.url)) {
      physical$distribution$online$url[[1]] <- paste0(
        data.url, "/", r$tables_found[i]
      )
    } else {
      physical$distribution <- list()
    }
    
    # Read the data table and get number of records
    df <- read.table(
      paste0(path, "/", r$tables_found[i]),
      header = TRUE,
      sep = r$delimiter[i],
      quote = "\"",
      as.is = TRUE,
      comment.char = ""
    )
    
    # Pull together information for the data table
    data_table[[i]] <- list(
      entityName = r$table_descriptions[i],
      entityDescription = r$table_descriptions[i],
      physical = physical,
      attributeList = attributeList,
      numberOfRecords = as.character(nrow(df))
    )
    
  }
  
  # Compile data tables
  
  eml$dataset$dataTable <- data_table
  
  # Add <otherEntity> ---------------------------------------------------------
  # Add items listed under the "script" argument as other entities
  
  other_entity <- list()
  
  for (i in 1:length(script)) {
    
    message("    <otherEntity>")
    
    # Create other entity
    otherEntity <- list(
      entityName = script[i],
      entityDescription = script.description[i],
      physical = suppressMessages(
        EML::set_physical(
          paste0(path, '/', script[i])
        )
      )
    )
    
    otherEntity$physical$dataFormat$textFormat <- NULL
    
    # Get and add file format name and entity type
    file_extension <- stringr::str_extract(script[i], "\\.[:alpha:]*$")
    if (file_extension == ".R") {
      format_name <- "application/R"
      entity_type <- "text/x-rsrc"
    } else if (file_extension == ".m") {
      format_name <- "application/MATLAB"
      entity_type <- "text/x-matlab"
    } else if (file_extension == ".py") {
      format_name <- "application/Python"
      entity_type <- "text/x-python"
    } else {
      format_name <- "unknown"
      entity_type <- "unknown"
    }
    
    otherEntity$physical$dataFormat$externallyDefinedFormat$formatName <- format_name
    otherEntity$entityType <- entity_type
    
    # Add download url
    if (!is.null(data.url)) {
      otherEntity$physical$distribution$online$url[[1]] <- paste0(
        data.url, "/", script[i]
      )
    } else {
      otherEntity$physical$distribution <- list()
    }
    
    # Add otherEntity to list
    other_entity[[i]] <- otherEntity
    
  }
  
  eml$dataset$otherEntity <- other_entity
  
  # Update <eml> --------------------------------------------------------------
  
  eml$schemaLocation <- "https://eml.ecoinformatics.org/eml-2.2.0  https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd"
  eml$packageId <- child.package.id
  eml$system <- "edi"
  
  message("  </dataset>")
  message("</eml>")
  
  # Write EML -----------------------------------------------------------------
  
  message("Writing EML")
  emld::eml_version("eml-2.2.0")
  EML::write_eml(
    eml, 
    paste0(path, "/", child.package.id, ".xml"))
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")
  
  validation_result <- EML::eml_validate(eml)
  
  if (validation_result == "TRUE"){
    message("EML passed validation!")
  } else {
    message("EML validaton failed. See warnings for details.")
  }
  
}




# Compile attributes
#
# @description  
#     This is a helper function for make_eml.R. It compiles attributes, 
#     retrieves minimum and maximum values for numeric data and reformats the 
#     attributes table.
#
# @usage 
#     make_eml(path, parent.package.id, child.package.id)
#
# @param path 
#     A path to the dataset working directory containing the validated 
#     ecocomDP tables.
#
# @return 
#     Attributes formatted for make_eml.R
#     
# @export
#
compile_attributes <- function(path){
  
  message('Compiling table attributes:')
  
  # Parameterize --------------------------------------------------------------
  
  table_patterns <- c(
    "observation\\b", 
    "observation_ancillary\\b", 
    "location_ancillary\\b", 
    "taxon_ancillary\\b", 
    "dataset_summary\\b", 
    "location\\b", 
    "taxon\\b", 
    "variable_mapping\\b"
  )
  
  table_names <- c(
    "observation", 
    "observation_ancillary", 
    "location_ancillary", 
    "taxon_ancillary", 
    "dataset_summary", 
    "location", 
    "taxon", 
    "variable_mapping"
  )
  
  table_descriptions <- c(
    "Observation table", 
    "Observation ancillary table", 
    "Location ancillary table", 
    "Taxon ancillary table", 
    "Dataset summary table", 
    "Location table", 
    "Taxon table", 
    "Variable mapping table"
  )
  
  dir_files <- list.files(path)
  table_names_found <- list()
  table_descriptions_found <- list()
  tables_found <- list()
  
  for (i in 1:length(table_patterns)){
    tables_found[[i]] <- dir_files[
      grep(
        paste0(
          "^(?=.*",
          table_patterns[i],
          ")(?!.*variables)"
        ), 
        dir_files,
        perl=TRUE
      )
      ]
    if (!identical(tables_found[[i]], character(0))){
      table_names_found[[i]] <- table_names[i]
    }
    if (!identical(tables_found[[i]], character(0))){
      table_descriptions_found[[i]] <- table_descriptions[i]
    }
  }
  
  tables_found <- unlist(tables_found)
  table_names <- unlist(table_names_found)
  table_descriptions <- unlist(table_descriptions_found)
  delimiter <- EDIutils::detect_delimeter(path, tables_found, EDIutils::detect_os())
  
  # Loop through each ecocomDP table that is present --------------------------
  
  attributes_stored <- list()
  
  for (i in 1:length(table_names)){
    
    message(table_names[i])
    
    if (delimiter[i] == ","){
      
      df_table <- read.csv(
        paste0(
          path,
          "/",
          tables_found[i]
        )
      )
      
    } else {
      
      df_table <- read.table(
        paste0(
          path,
          "/",
          tables_found[i]
        ),
        header = T,
        sep = delimiter[i],
        as.is = T,
        na.strings = "NA"
      ) 
      
    }
    
    # Read attributes_draft table
    
    df_attributes <- read.table(
      system.file(
        paste0(
          '/attributes_',
          table_names[i],
          '.txt'
        ),
        package = 'ecocomDP'
      ),
      header = T,
      sep = "\t",
      as.is = T,
      na.strings = "NA",
      colClasses = rep("character", 7)
    )
    
    # Synchronize data table and attributes table
    
    use_i <- match(
      colnames(df_table), 
      df_attributes[["attributeName"]]
    )
    
    df_attributes <- df_attributes[use_i, ]
    
    # Initialize outgoing attribute table 
    
    rows <- nrow(df_attributes)
    attributes <- data.frame(
      attributeName = character(rows),
      formatString = character(rows),
      unit = character(rows),
      numberType = character(rows),
      definition = character(rows),
      attributeDefinition = character(rows),
      columnClasses = character(rows),
      minimum = character(rows),
      maximum = character(rows),
      missingValueCode = character(rows),
      missingValueCodeExplanation = character(rows),
      stringsAsFactors = FALSE
    )
    
    # Set attribute names
    
    attributes$attributeName <- df_attributes$attributeName
    
    # Set attribute definition (i.e. "attributeDefinition")
    
    attributes$attributeDefinition <- df_attributes$attributeDefinition
    
    # Set attribute class
    
    attributes$columnClasses <- df_attributes$class
    
    # Set attribute units
    
    attributes$unit <- df_attributes$unit
    
    # Set attribute date time format string
    
    attributes$formatString <- df_attributes$dateTimeFormatString
    
    # Set attribute missing value code
    
    attributes$missingValueCode <- df_attributes$missingValueCode
    
    use_i <- is.na(attributes$missingValueCode)
    attributes$missingValueCode[use_i] <- "NA"
    
    # Set attribute missing value code explanation
    
    attributes$missingValueCodeExplanation <- df_attributes$missingValueCodeExplanation
    
    # Set attribute number type, then minimumm and maximum values
    
    is_numeric <- which(attributes$columnClasses == "numeric")
    attributes$minimum <- as.numeric(attributes$minimum)
    attributes$maximum <- as.numeric(attributes$maximum)
    
    if (!identical(is_numeric, integer(0))){
      for (j in 1:length(is_numeric)){
        raw <- df_table[ ,is_numeric[j]]
        if (attributes$missingValueCode[is_numeric[j]] != ""){
          useI <- raw == attributes$missingValueCode[is_numeric[j]]
          raw <- as.numeric(raw[!useI])
        }
        if (sum(is.na(raw)) == length(raw)){
          attributes$columnClasses[is_numeric[j]] <- "character"
          attributes$unit[is_numeric[j]] <- ""
        } else {
          rounded <- floor(raw)
          if (length(raw) - sum(raw == rounded, na.rm = T) > 0){
            attributes$numberType[is_numeric[j]] <- "real"
          } else if (min(raw, na.rm = T) > 0){
            attributes$numberType[is_numeric[j]] <- "natural"
          } else if (min(raw, na.rm = T) < 0){
            attributes$numberType[is_numeric[j]] <- "integer"
          } else {
            attributes$numberType[is_numeric[j]] <- "whole"
          }
          attributes$minimum[is_numeric[j]] <- round(
            min(
              raw,
              na.rm = TRUE
            ),
            digits = 2
          )
          attributes$maximum[is_numeric[j]] <- round(
            max(
              raw,
              na.rm = TRUE
            ),
            digits = 2
          )
        }
      }
    }
    
    is_character <- which(attributes$columnClasses == "character") 
    is_catvar <- which(attributes$columnClasses == "categorical")
    use_i <- c(is_character, is_catvar)
    attributes$numberType[use_i] <- "character"
    attributes$columnClasses[is_catvar] <- "factor"
    
    # Set attribute definition
    
    use_i <- c(is_character, is_catvar)
    if (length(use_i) > 0){
      attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
    }
    
    # Define datetime format string. Remove datetime column if empty (i.e. NA).
    
    if (sum(use_i <- attributes$columnClasses == "Date") > 0){
      use_i <- attributes$columnClasses == "Date"
      colname <- attributes$attributeName[use_i]
      if (sum(is.na(df_table[ , colname])) == nrow(df_table)){
        attributes$formatString[use_i] <- 'YYYY'
      } else {
        datetime_format <- dataCleanr::iso8601_get_format_string(df_table[ , colname])
        attributes$formatString[use_i] <- datetime_format
      }
    }
    
    # Add missing value codes
    
    attributes$missingValueCode <- 'NA'
    attributes$missingValueCodeExplanation <- 'Missing value'
    
    # Store attributes
    
    attributes_stored[[i]] <- attributes
    
  }
  
  list(
    attributes = attributes_stored,
    tables_found = tables_found,
    table_names = unlist(table_names_found),
    table_descriptions = unlist(table_descriptions_found),
    delimiter = delimiter
  )
  
}
