#' Convert a dataset to the Darwin Core Archive format
#'
#' @param path (character) Path to which the DwC-A data objects and EML will be written.
#' @param core_name (character) The central table of the DwC-A dataset being created. Can be: "event" (event core). Occurrence core is not yet supported.
#' @param source_id (character) Identifier of an ecocomDP dataset published in a supported repository. Currently, the EDI Data Repository is supported.
#' @param derived_id (character) Identifier of the DwC-A dataset being created.
#' @param url (character) URL to the publicly accessible directory containing DwC-A data objects. This argument supports direct download of the data entities by a data repository and is used for automated revisioning and publication.
#' @param user_id (character) Identifier of user account associated with the data repository in which this ecocomDP dataset will be archived. Only \code{user_id} from the EDI is currently supported.
#' @param user_domain (character) Domain (data repository) the \code{user_id} belongs to. Currently, EDI is supported.
#' 
#' @details Reads in an ecocomDP dataset from a supported repository and converts it to a DwC-A package.
#' 
#' @return DwC-A tables, meta.xml, and corresponding EML metadata.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Create directory for DwC-A outputs
#' mypath <- paste0(tempdir(), "/data")
#' dir.create(mypath)
#' 
#' # Convert an EDI published ecocomDP dataset to a DwC-A
#' convert_to_dwca(
#'   path = mypath, 
#'   core_name = "event", 
#'   source_id = "edi.193.5", 
#'   derived_id = "edi.834.2", 
#'   user_id = "ecocomdp",
#'   user_domain = "EDI")
#' 
#' dir(mypath)
#' 
#' # Clean up
#' unlink(mypath, recursive = TRUE)
#' }
#' 
convert_to_dwca <- function(path, 
                          core_name, 
                          source_id, 
                          derived_id,
                          url = NULL,
                          user_id,
                          user_domain) {
  
  validate_arguments(fun.name = "convert_to_dwca", fun.args = as.list(environment()))
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("environment", envir = .GlobalEnv)) {
    environment <- get("environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Load data -----------------------------------------------------------------
  
  d <- read_data(source_id)
  d <- d$tables
  
  # Convert tables ------------------------------------------------------------
  
  if (core_name == 'event') {
    # call a function to create event core. inputs: data objects, dwca_mappings, dwca_config  
    # print('calling function to create DwC-A, event core')
    r <- create_tables_dwca_event_core(
      dt_obs = d$observation,
      dt_loc = d$location,
      dt_tax = d$taxon,
      dt_loc_ancil = d$location_ancillary,
      dt_obs_ancil = d$observation_ancillary,
      source_id = source_id,
      derived_id = derived_id)
  }
  
  # Write tables to file ------------------------------------------------------
  
  # Set physical attributes here so they will match what is listed in meta.xml
  for (i in names(r)) {
    data.table::fwrite(
      x = r[[i]],
      file = paste0(path, "/", i, ".csv"),
      sep = ",",
      quote = TRUE,
      eol = "\r\n")
  }
  
  # Write meta.xml ------------------------------------------------------------
  
  if (core_name == "event") {
    meta <- xml2::read_xml(
      system.file(
        "extdata", "/dwca_event_core/meta.xml", 
        package = "ecocomDP"
      )
    )
    meta_file <- paste0(derived_id, ".xml")
    xml2::xml_set_attr(x = meta, attr = "metadata", value = meta_file)
    xml2::write_xml(meta, file = paste0(path, "/meta.xml"))
  } else if (core_name == "occurrence") {
    # file.copy(
    #   from = system.file("/dwca_occurrence_core/meta.xml", package = "ecocomDP"),
    #   to = path)
  }

  # Write EML to file ---------------------------------------------------------
  
  eml <- make_eml_dwca(
    path = path,
    core_name = core_name,
    source_id = source_id, 
    derived_id = derived_id, 
    user.id = user_id, 
    user.domain = user_domain,
    url = url)
  
}






# Creates DwC-A tables in event core 
#
# @param dt_obs (data.frame) Observation table
# @param dt_obs_ancil (data.frame) Observation ancillary table
# @param dt_loc_ancil (data.frame) Location ancillary table
# @param dt_loc (data.frame) Location table
# @param dt_tax (data.frame) Taxon table
# @param source_id (character) ID of an ecocomDP data package. Only EDI Data Repository package IDs are currently supported.
# @param derived_id (character) Identifier of the DwC-A dataset being created.
#
# @return (.csv) event, occurrence, measurementOrFact tables
#
create_tables_dwca_event_core <- function(
  dt_obs,
  dt_obs_ancil,
  dt_loc_ancil,
  dt_loc,
  dt_tax,
  source_id,
  derived_id) {
  
  message('Creating DwC-A Event Core tables')
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Validate function inputs --------------------------------------------------
  # Do you have what you need to get these vars? confirm fields used by mapping 
  # table are present. Anything that is required by the ecocomDP model already, 
  # you can assume is present. location_ancillary.value, or 
  # observation_ancillary.value ?? what does this note mean? do I need these?
  
  # Join the tables -----------------------------------------------------------
  
  obs_loc_tax <- join_obs_loc_tax(
    dt_obs = dt_obs, 
    dt_loc = dt_loc, 
    dt_tax = dt_tax)
  
  # Add column
  # Create computed vectors:
  
  # Define new cols specifically needed for DwC-A
  obs_loc_tax$id <- seq_along(obs_loc_tax$observation_id)
  obs_loc_tax$lsid <- NA_character_
  obs_loc_tax$dc_occurrence_id <- paste(
    obs_loc_tax$id, 
    obs_loc_tax$taxon_id,
    obs_loc_tax$observation_id,
    sep = "_"
  )
  obs_loc_tax$dc_samplingprotocol <- NA_character_
  obs_loc_tax$dc_event_id <- obs_loc_tax$id
  obs_loc_tax$dc_dataset_name <- NA_character_
  
  # comb_id
  
  # Use the DOI of the L1 and construct as: "See methods in DOI"
  obs_loc_tax$dc_samplingprotocol <- paste0(
    "See methods in ",
    suppressMessages(api_read_data_package_doi(source_id, environment)))
  
  # Determine if observation was made by an instrument or human. This 
  # info is stored in the L1 EML keywordSet
  
  keywords <- xml2::xml_text(
    xml2::xml_find_all(
      suppressMessages(
        api_read_metadata(source_id, environment)), ".//keyword"))
  
  basis_of_record <- trimws(
    stringr::str_remove(
      keywords[stringr::str_detect(keywords, "basisOfRecord:")],
      "basisOfRecord:"))
  
  if (length(basis_of_record) == 1) {
    obs_loc_tax$dc_basisofrecord <- basis_of_record
  } else {
    obs_loc_tax$dc_basisofrecord <- NA_character_
  }

  # datasetName 
  dc_dataset_name <- "Dataset name"
  
  # Handle YYYY datetime exception --------------------------------------------
  
  # datetime format - For parsing datetimes of L1
  if (environment == "production") {
    url_parent <- paste0(
      "https://pasta.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  } else if (environment == "staging") {
    url_parent <- paste0(
      "https://pasta-s.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  }
  xml_L1 <- suppressMessages(read_eml(source_id))
  data_table_nodes_parent <- xml2::xml_find_all(
    xml_L1,
    ".//dataTable")
  observation_table_node_parent <- data_table_nodes_parent[
    stringr::str_detect(
      xml2::xml_text(
        xml2::xml_find_all(
          xml_L1, 
          ".//physical/objectName")), 
      "observation\\..*$")]
  format_string <- xml2::xml_text(
    xml2::xml_find_all(
      observation_table_node_parent, 
      ".//formatString"))
  
  if (format_string == "YYYY") {
    obs_loc_tax$datetime <- as.character(lubridate::year(obs_loc_tax$datetime))
  }
  
  # Create event --------------------------------------------------------------
  
  event_table <- data.frame(
    id = obs_loc_tax$id,
    eventID = obs_loc_tax$dc_event_id,
    datasetName = dc_dataset_name,
    samplingProtocol = obs_loc_tax$dc_samplingprotocol,
    eventDate = obs_loc_tax$datetime,
    decimalLatitude = obs_loc_tax$latitude,
    decimalLongitude = obs_loc_tax$longitude,
    georeferenceRemarks = obs_loc_tax$location_name,
    stringsAsFactors = FALSE)
  
  # Unique event_table based on all columns
  event_table <- unique.data.frame(
    event_table)
  
  # Create occurrence ---------------------------------------------------------
  
  occurrence_table <- data.frame(
    id = obs_loc_tax$id,
    occurrenceID = obs_loc_tax$dc_occurrence_id,
    basisOfRecord = obs_loc_tax$dc_basisofrecord,
    scientificName = obs_loc_tax$taxon_name,
    taxonID = obs_loc_tax$authority_taxon_id,
    nameAccordingTo = obs_loc_tax$authority_system,
    scientificNameID = obs_loc_tax$lsid,
    stringsAsFactors = FALSE)
  
  # Unique occurrence_table based on all columns except occurrenceID to reduce
  # redundancy
  occurrence_table <- dplyr::distinct_at(
    occurrence_table, 
    .vars = c("basisOfRecord", "scientificName", "taxonID",
              "nameAccordingTo", "scientificNameID"),
    .keep_all = TRUE)
  
  # Create extendedmeasurementorfact ------------------------------------------
  
  extendedmeasurementorfact_table <- data.frame(
    id = obs_loc_tax$id,
    occurrenceID = obs_loc_tax$dc_occurrence_id,
    measurementType = obs_loc_tax$variable_name,
    measurementTypeID = NA_character_,
    measurementValue = obs_loc_tax$value,
    measurementUnit = obs_loc_tax$unit,
    stringsAsFactors = FALSE)
  
  return(
    list(
      event = event_table,
      occurrence = occurrence_table,
      extendedmeasurementorfact = extendedmeasurementorfact_table))
  
}








# Make EML metadata for a DWcA occurrence from an ecocomDP data package
#
# @param path 
#     (character) Path to the directory containing ecocomDP data tables, conversion scripts, and where EML metadata will 
#     be written. This \code{path}, when defined on a web server, also serves as the publicly accessible URL from which the 
#     data objects can be downloaded.
# @param core_name
#     (character) The Darwin Core central table of the package. Can be: "occurence" (occurrence core) or "event" (event core).
# @param source_id
#     (character) ID of an ecocomDP data package. Only EDI Data Repository package IDs are currently supported.
# @param derived_id (character) Identifier of the DwC-A dataset being created.
# @param user.id
#     (character; optional) Repository user identifier. If more than one, then enter as a vector of character strings 
#     (e.g. \code{c("user_id_1", "user_id_2")}). \code{user.id} sets the /eml/access/principal element for all 
#     \code{user.domain} except "KNB", "ADC", and if \code{user.domain = NULL}.
# @param user.domain
#     (character; optional) Repository domain associated with \code{user.id}. Currently 
#     supported values are "EDI" (Environmental Data Initiative), "LTER" (Long-Term Ecological Research Network), 
#     "KNB" (The Knowledge Network for Biocomplexity), "ADC" (The Arctic Data Center). If you'd like your system supported please
#     contact maintainers of the ecocomDP R package. If using more than one \code{user.domain}, then enter as a vector 
#     of character strings (e.g. \code{c("user_domain_1", "user_domain_2")}) in the same order as corresponding \code{user.id}. 
#     If \code{user.domain} is missing then a default value "unknown" is assigned. \code{user.domain} sets the EML header 
#     "system" attribute and for all \code{user.domain}, except "KNB" and "ADC", sets the /eml/access/principal element 
#     attributes and values.
# @param url
#     (character) URL to the publicly accessible directory containing DwC-A tables and meta.xml. This argument supports 
#     direct download of the data entities by a data repository and is used within the scope of the ecocomDP project for 
#     automated revision and upload of ecocomDP data packages and derived products.
#
# @return 
#     An EML metadata record for the DWcA table defined by \code{data.table}.
#
# @details 
#     This function creates an EML record for an Darwin Core Archive record
#     (DwC-A) combining metadata from the parent data package and
#     boiler-plate metadata describing the DwC-A tables. Changes to the 
#     parent EML include:
#     \itemize{
#         \item \strong{<access>} Adds the \code{user.id} to the list of principals granted read and write 
#         access to the DwC-A data package this EML describes.
#         \item \strong{<title>} Appends "Darwin Core Archive: " to the title.
#         \item \strong{<pubDate>} Adds the date when this EML record is created.
#         \item \strong{<abstract>} Adds a note that this is a derived data package in a DwC-A format.
#         \item \strong{<keywordSet>} Essential Biodiversity Variables: "Population Abundance" and Darwin Core Terms: 
#         "BasisofRecord: HumanObservation", "Occurrence: OrganismQuantity", "Taxon: ScientificName". 
#         \item \strong{<intellectualRights>} Keeps intact the intellectual rights license of the parent data package, or 
#         replaces it with "CCO" (https://creativecommons.org/publicdomain/zero/1.0/legalcode).
#         \item \strong{<methodStep>} Adds a note that this data package was created by methods within the ecocomDP R package 
#         and adds provenance metadata noting that this is a derived data and describing where the parent data package can 
#         be accessed.
#         \item \strong{<dataTables>} Replaces the parent data package data tables metadata with boiler-plate metadata for the 
#         DwC-A tables.
#         \item \strong{<otherEntity>} Describes the meta.xml accompanying each DwC-A. Any other entities listed in the parent 
#         EML are removed.
#     }
#
make_eml_dwca <- function(path, 
                          core_name, 
                          source_id, 
                          derived_id, 
                          user.id, 
                          user.domain,
                          url = NULL) {
  
  message("Creating DwC-A ", stringr::str_to_title(core_name), 
          " Core EML")
  
  # Load Global Environment config --------------------------------------------
  
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
  
  # Validate inputs -----------------------------------------------------------
  
  # The parent data package should exist
  parent_data_package_eml <- suppressMessages(
    api_read_metadata(source_id, environment)
  )
  if (typeof(parent_data_package_eml) == "character") {
    # A type of character indicates there was a failure in reading the parent 
    # data package EML (we are expecting a list of XML elements), and therefore 
    # cannot proceed with processing.
    stop(
      "The L1 data package '", source_id, "' does not exist.",
      call. = FALSE)
  }
  
  # The child data package should not exist since it's being created here, but
  # could in a non-production environment
  child_data_package_exists <- api_read_metadata(derived_id, environment)
  if ("xml_document" %in% class(child_data_package_exists)) {
    warning(
      "The L2 data package '", derived_id, "' already exists.",
      call. = FALSE)
  }
  
  # A "user.id" is required for each "user.domain"
  if (length(user.id) != length(user.domain)) {
    stop(
      "The number of items listed under the 'user.id' and 'user.domain' ",
      "arguments must match.", call. = FALSE)
  }
  
  # Parameterize --------------------------------------------------------------
  
  # Table names, types, and descriptions are standardized for the input 
  # "core_name"
  if (core_name == "event") {
    data.table <- c(
      "event.csv", 
      "occurrence.csv",
      "extendedmeasurementorfact.csv")
    data.table.description <- c(
      "DwC-A Event Table", 
      "DwC-A Occurrence Table",
      "DwC-A Extended Measurement Or Fact Table")
  } else if (core_name == "occurrence") {
    data.table <- "occurrence.csv"
    data.table.description <- "DwC-A Occurrence Table"
  }
  
  # Other entity name, type, and description is standardized for the input 
  # "core_name"
  other.entity <- "meta.xml"
  other.entity.description <- "The meta file associated with this dataset"
  
  # Error if the expected standards required by this function are not followed
  missing_data_objects <- c(
    !(data.table %in% dir(path)),
    !("meta.xml" %in% dir(path)))
  if (any(missing_data_objects)) {
    stop(
      "Missing data objects: ",
      paste(c(data.table, "meta.xml")[missing_data_objects], collapse = ","),
      call. = FALSE)
  }
  
  # Expand url for each data object of this L1 for use in 
  # EAL_make_eml()
  
  if (!is.null(url)) {
    data.table.url <- paste0(url, "/", data.table)
    other.entity.url <- paste0(url, "/", other.entity)
  } else {
    data.table.url <- NULL
    other.entity.url <- NULL
  }
  
  # Read L1 EML ---------------------------------------------------------------
  
  message("Reading EML of L1 data package ", source_id)
  
  # Create two objects of the same metadata, eml_L1 (emld list object) for
  # editing, and xml_L1 (xml_document) for easy parsing
  
  if (environment == "production") {
    url_parent <- paste0(
      "https://pasta.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  } else if (environment == "staging") {
    url_parent <- paste0(
      "https://pasta-s.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  }
  
  eml_L1 <- EML::read_eml(url_parent)
  xml_L1 <- suppressMessages(
    read_eml(source_id))
  
  # Read L0 EML ---------------------------------------------------------------
  
  # Some metadata from the L0 is required by the L2 and simpler to get from the
  # L0 than parsing from the L1
  url_grandparent <- xml2::xml_text(
    xml2::xml_find_all(
      xml_L1,
      ".//methodStep/dataSource/distribution/online/url"))
  grandparent.package.id <- stringr::str_replace_all(
    stringr::str_extract(
      url_grandparent,
      "(?<=eml/).*"),
    "/",
    ".")
  
  # Create two objects of the same metadata, eml_L0 (emld list object) for
  # editing, and xml_L0 (xml_document) for easy parsing
  message("Reading EML of L0 data package ", grandparent.package.id)
  
  xml_L0 <- suppressMessages(
    read_eml(grandparent.package.id))
  
  
  eml_L0 <- suppressMessages(
    EML::read_eml(url_grandparent))
  
  # Create L2 EML -------------------------------------------------------------
  # This is not a full EML record, it is only the sections of EML that will be 
  # added to the parent EML.
  
  message("Creating EML of L2 data package ", derived_id)
  
  # Create list of inputs to EAL_make_eml()
  eal_inputs <- EAL_template_arguments(
    path = system.file("extdata", "/dwca_event_core", package = "ecocomDP"), 
    data.path = path, 
    data.table = data.table,
    other.entity = "meta.xml")
  eal_inputs$path <- system.file("extdata", "/dwca_event_core", package = "ecocomDP")
  eal_inputs$data.path <- path
  eal_inputs$eml.path <- path
  eal_inputs$dataset.title <- "placeholder"
  eal_inputs$data.table <- data.table
  eal_inputs$data.table.name <- tools::file_path_sans_ext(data.table)
  eal_inputs$data.table.description <- data.table.description
  eal_inputs$data.table.url <- data.table.url
  eal_inputs$data.table.quote.character <- rep('"', length(data.table))
  eal_inputs$other.entity <- other.entity
  eal_inputs$other.entity.name <- tools::file_path_sans_ext(other.entity)
  eal_inputs$other.entity.description <- other.entity.description
  eal_inputs$other.entity.url <- other.entity.url
  eal_inputs$provenance <- source_id
  eal_inputs$package.id <- derived_id
  eal_inputs$user.id <- user.id
  eal_inputs$user.domain <- user.domain
  eal_inputs$return.obj <- TRUE
  
  # Get date and time format string from the L1 EML and add to the event table 
  # attributes template (attributes_event.txt) of the L2 since this 
  # information can vary with the L1.
  data_table_nodes_parent <- xml2::xml_find_all(
    xml_L1,
    ".//dataTable")
  observation_table_node_parent <- data_table_nodes_parent[
    stringr::str_detect(
      xml2::xml_text(
        xml2::xml_find_all(
          xml_L1, 
          ".//physical/objectName")), 
      "observation\\..*$")]
  format_string <- xml2::xml_text(
    xml2::xml_find_all(
      observation_table_node_parent, 
      ".//formatString"))
  use_i <- eal_inputs$x$template$attributes_event.txt$content$attributeName == 
    "eventDate"
  eal_inputs$x$template$attributes_event.txt$content$dateTimeFormatString[
    use_i] <- format_string
  
  # All annotations in the annotations template used by 
  # EAL_template_arguments() are used here since the DwC-A format
  # is constant (i.e. the table attributes don't change). Some annotations
  # in this template may not yet have definition, so incomplete cases will be
  # dropped.
  
  eal_inputs$x$template$annotations.txt$content[
    eal_inputs$x$template$annotations.txt$content == ""] <- NA_character_
  
  eal_inputs$x$template$annotations.txt$content <- 
    eal_inputs$x$template$annotations.txt$content[
      stats::complete.cases(eal_inputs$x$template$annotations.txt$content), ]
  
  # Create child EML
  eml_L2 <- suppressWarnings(
    suppressMessages(
      do.call(
        EAL_make_eml, 
        eal_inputs[
          names(eal_inputs) %in% names(formals(EAL_make_eml))])))
  
  # Update <eml> --------------------------------------------------------------
  
  message("Updating:")
  message("<eml>")
  eml_L1$schemaLocation <- paste0(
    "https://eml.ecoinformatics.org/eml-2.2.0  ",
    "https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd")
  eml_L1$packageId <- derived_id
  eml_L1$system <- "edi"
  
  # Update <access> of parent -------------------------------------------------
  
  message("  <access>")
  
  # Access control rules are used by some repositories to manage 
  # editing, viewing, downloading permissions. Adding the user.id and 
  # user.domain here expands editing permission to the creator of the DwC-A 
  # data package this EML will be apart of.
  eml_L1$access$allow <- unique(
    c(eml_L1$access$allow, 
      eml_L2$access$allow))
  
  # Update <dataset> ----------------------------------------------------------
  
  # For purposes of annotation references, the <dataset> attribute (which may
  # have been set by the L1 creator) needs to be set to "dataset", which is 
  # expected by the L2 dataset annotation.
  
  eml_L1$dataset$id <- "dataset"
  
  # Update <alternateIdentifier> ----------------------------------------------
  
  message("  <dataset>")
  message("    <alternateIdentifier>")
  
  # Some repositories assign a DOI to this element. Not removing it here 
  # an error when uploading to the repository.
  
  eml_L1$dataset$alternateIdentifier <- NULL
  
  # Update <title> ------------------------------------------------------------
  
  message("    <title>")
  
  # Add notification to indicate this is a Darwin Core Archive
  eml_L1$dataset$title <- paste(
    eml_L0$dataset$title, "(Reformatted to a Darwin Core Archive)")
  
  # Update <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  eml_L1$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")
  
  # Update <abstract> ---------------------------------------------------------
  
  message("    <abstract>")
  
  # Constructing the L2 abstract paragraph here because "<ulink>" required by 
  # GBIF, gets mangled when reading from file with EML::set_TextType()
  
  L2_para <- paste0(
    "This data package is formatted as a Darwin Core Archive (DwC-A, event ",
    "core). For more information on Darwin Core see ",
    "<ulink>https://www.tdwg.org/standards/dwc/</ulink>. This Level 2 data ",
    "package was derived from the Level 1 data package found here: <ulink>",
    url_parent, "</ulink>, which was derived from the Level 0 data package ",
    "found here: <ulink>", url_grandparent, "</ulink>. The abstract above was",
    " extracted from the Level 0 data package and is included for context."
  )
  
  L0_para <- xml2::xml_text(
    xml2::xml_find_all(xml_L0, ".//abstract//para"))
  eml_L1$dataset$abstract <- NULL
  
  # Create L2 abstract
  eml_L1$dataset$abstract$para <- c(
    list(L0_para),
    list(L2_para))
  
  # Update <keywordSet> -------------------------------------------------------
  
  message("    <keywordSet>")
  
  # Preserve the L0 keywords, all L1 keywords except "ecocomDP" (since this is 
  # no longer an ecocomDP data package), and add L2 keywords.
  
  keywords_L1_to_keep <- lapply(
    eml_L1$dataset$keywordSet, 
    function(x) {
      if (!("EDI Controlled Vocabulary" %in% x$keywordThesaurus)) {
        x
      }
    })
  
  eml_L1$dataset$keywordSet <- c(
    eml_L2$dataset$keywordSet, 
    keywords_L1_to_keep)
  
  # Update <intellectualRights> -----------------------------------------------
  
  # FIXME Are we keeping the original license or adding our own? Any change
  # here should also be applied to create_eml() for the L1.
  
  message("    <intellectualRights>")
  eml_L1$dataset$intellectualRights <- list(
    para = paste0(
      "This data package is released to the public domain under Creative ",
      "Commons CC0 1.0 No Rights Reserved (see: ",
      "<ulink>https://creativecommons.org/publicdomain/zero/1.0/</ulink>). ",
      "It is considered professional etiquette to provide attribution of ",
      "the original work if this data package is shared in whole or by ",
      "individual components. A generic citation is provided for this data ",
      "package on the website ",
      "<ulink>https://portal.edirepository.org</ulink> (herein website) ",
      "in the summary metadata page. Communication (and collaboration) with ",
      "the creators of this data package is recommended to prevent duplicate ",
      "research or publication. This data package (and its components) is ",
      "made available as is and with no warranty of accuracy or fitness for ",
      "use. The creators of this data package and the website shall not be ",
      "liable for any damages resulting from misinterpretation or misuse of ",
      "the data package or its components. Periodic updates of this data ",
      "package may be available from the website. Thank you."
    )
  )
  
  # Update <methods> ----------------------------------------------------------
  
  message("    <methods>")
  
  # Constructing the L2 methods paragraph here because "<ulink>" required by 
  # GBIF, gets mangled when reading from file with EML::set_methods()
  
  methods_L2 <- list(
    description = list(
      para = paste0(
        "This data package is a child (Level 2) of the parent data package ",
        "(Level 1: <ulink>", url_parent, "</ulink>), and listed in the ",
        "dataSource element below. This Level 2 data package was created ",
        "using the convert_to_dwca() function of the ecocomDP R library: ",
        "<ulink>https://github.com/EDIorg/ecocomDP</ulink>. Methods of the ",
        "Level 0 original source dataset (<ulink>", url_grandparent, 
        "</ulink>) are included below for context:"
      )
    )
  )
  
  provenance_L1 <- suppressMessages(
    api_get_provenance_metadata(
      package.id = source_id,
      environment = environment))
  xml2::xml_set_attrs(xml2::xml_find_all(provenance_L1, ".//*[@id]"), c(id = NULL)) # Remove attributes to prevent id clashing
  provenance_L1 <- EML::read_eml(provenance_L1)
  provenance_L1 <- list(
    dataSource = provenance_L1$dataSource,
    description = provenance_L1$description)
  
  # Combine L2 methods, L0 methods, and L1 provenance
  eml_L1$dataset$methods$methodStep <- c(
    list(methods_L2),
    list(eml_L0$data$methods$methodStep),
    list(provenance_L1))
  
  # Update <dataTable> --------------------------------------------------------
  
  message("    <dataTable>")
  eml_L1$dataset$dataTable <- eml_L2$dataset$dataTable
  
  # Add <otherEntity> ---------------------------------------------------------
  
  message("    <otherEntity>")
  eml_L1$dataset$otherEntity <- eml_L2$dataset$otherEntity
  
  # Update <annotations> ------------------------------------------------------
  
  message("    <annotations>")
  eml_L1$annotations <- eml_L2$annotations
  
  # Write EML -----------------------------------------------------------------
  
  message("</eml>")
  message("Writing EML")
  
  emld::eml_version("eml-2.2.0")
  EML::write_eml(
    eml_L1, 
    paste0(path, "/", derived_id, ".xml"))
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")
  
  r <- EML::eml_validate(eml_L1)
  if (isTRUE(r)) {
    message("  Validation passed :)")
  } else {
    message("  Validation failed :(")
  }
  message("Done.")
  
  # Return --------------------------------------------------------------------
  
  return(eml_L1)
  
}

