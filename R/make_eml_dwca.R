#' Make EML metadata for a DWcA occurrence from an ecocomDP data package
#'
#' @param path 
#'     (character) Path to the directory containing ecocomDP data tables, conversion scripts, and where EML metadata will be written. This \code{path}, when defined on a web server, also serves as the publicly accessible URL from which the data objects can be downloaded.
#' @param core.name
#'     (character) The Darwin Core central table of the package. Can be: "occurence" (occurrence core) or "event" (event core).
#' @param parent.package.id
#'     (character) ID of an ecocomDP data package. Only EDI Data Repository package IDs are currently supported.
#' @param child.package.id
#'     (character) ID of DWcA occurrence data package being created.
#' @param user.id
#'     (character; optional) Repository user identifier. If more than one, then enter as a vector of character strings (e.g. \code{c("user_id_1", "user_id_2")}). \code{user.id} sets the /eml/access/principal element for all \code{user.domain} except "KNB", "ADC", and if \code{user.domain = NULL}.
#' @param user.domain
#'     (character; optional) Repository domain associated with \code{user.id}. Currently supported values are "EDI" (Environmental Data Initiative), "LTER" (Long-Term Ecological Research Network), "KNB" (The Knowledge Network for Biocomplexity), "ADC" (The Arctic Data Center). If you'd like your system supported please contact maintainers of the EMLassemblyline R package. If using more than one \code{user.domain}, then enter as a vector of character strings (e.g. \code{c("user_domain_1", "user_domain_2")}) in the same order as corresponding \code{user.id}. If \code{user.domain} is missing then a default value "unknown" is assigned. \code{user.domain} sets the EML header "system" attribute and for all \code{user.domain}, except "KNB" and "ADC", sets the /eml/access/principal element attributes and values.
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
make_eml_dwca <- function(path, 
                          core.name, 
                          parent.package.id, 
                          child.package.id, 
                          user.id, 
                          user.domain) {
  
  message(
    "Creating Darwin Core ", stringr::str_to_title(core.name), " Core EML ",
    "for data package ", child.package.id)
  
  # Validate inputs -----------------------------------------------------------
  
  # TODO: Check the child data package exists
  
  # A user.id is required for each user.domain
  if (length(user.id) != length(user.domain)) {
    stop(
      "The number of items listed under the 'user.id' and 'user.domain' ",
      "arguments must match.", call. = FALSE)
  }
  
  # Parameterize --------------------------------------------------------------
  
  # Table names, types, and descriptions are standardized to core.name
  if (core.name == "event") {
    data.table <- c(
      "event.csv", 
      "occurrence.csv",
      "extendedmeasurementorfact.csv")
    data.table.description <- c(
      "DwC-A Event Table", 
      "DwC-A Occurrence Table",
      "DwC-A Extended Measurement Or Fact Table")
  } else if (core.name == "occurrence") {
    data.table <- "occurrence.csv"
    data.table.description <- "DwC-A Occurrence Table"
  }
  
  # Other entity name, type, and description is standardized for any core.name
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
  
  # Read parent EML -----------------------------------------------------------
  
  message("Reading EML of parent data package ", parent.package.id)
  
  # Create two objects of the same metadata, eml_parent (emld list object) for
  # editing, and xml_parent (xml_document) for easy parsing
  eml_parent <- EML::read_eml(
    paste0(
      "https://pasta.lternet.edu/package/metadata/eml/", 
      stringr::str_replace_all(parent.package.id, "\\.", "/")))
  xml_parent <- suppressMessages(
    EDIutils::api_read_metadata(parent.package.id))
  
  # Create child EML ----------------------------------------------------------
  # This is not a full EML record, it is only the sections of EML that will be 
  # added to the parent EML.
  
  # Create list of inputs to EMLassemblyline::make_eml()
  eal_inputs <- EMLassemblyline::template_arguments(
    path = system.file("/dwca_event_core", package = "ecocomDP"), 
    data.path = path, 
    data.table = data.table,
    other.entity = "meta.xml")
  eal_inputs$path <- system.file("/dwca_event_core", package = "ecocomDP")
  eal_inputs$data.path <- path
  eal_inputs$eml.path <- path
  eal_inputs$dataset.title <- "placeholder"
  eal_inputs$data.table <- data.table
  eal_inputs$data.table.description <- data.table.description
  eal_inputs$data.table.url <- paste0(path, "/", data.table)
  eal_inputs$data.table.quote.character <- rep('"', length(data.table))
  eal_inputs$other.entity <- other.entity
  eal_inputs$other.entity.description <- other.entity.description
  eal_inputs$other.entity.url <- paste0(path, "/", other.entity)
  eal_inputs$package.id <- child.package.id
  eal_inputs$user.id <- user.id
  eal_inputs$user.domain <- user.domain
  eal_inputs$return.obj <- TRUE
  
  # Get date and time format string from the L1 EML and add to the event table 
  # attributes template (attributes_event.txt) of the L2 since this 
  # information can vary with the L1.
  data_table_nodes_parent <- xml2::xml_find_all(
    xml_parent,
    ".//dataTable")
  observation_table_node_parent <- data_table_nodes_parent[
    stringr::str_detect(
      xml2::xml_text(
        xml2::xml_find_all(
          xml_parent, 
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
  
  # Create child EML
  eml_child <- suppressWarnings(
    suppressMessages(
      do.call(
        EMLassemblyline::make_eml, 
        eal_inputs[
          names(eal_inputs) %in% names(formals(EMLassemblyline::make_eml))])))

  # Update <access> of parent -------------------------------------------------
  
  # Access control rules are used by some repositories to manage 
  # editing, viewing, downloading permissions. Adding the user.id and 
  # user.domain here expands editing permission to the creator of the DwC-A 
  # data package this EML will be apart of.
  message("Updating:")
  message("  <access>")
  eml_parent$access$allow <- unique(
    c(eml_parent$access$allow, 
      eml_child$access$allow))
  
  # Remove <alternateIdentifier> ----------------------------------------------
  
  # Some repositories assign a DOI to this element. Not removing it here 
  # an error when uploading to the repository.
  eml_parent$dataset$alternateIdentifier <- NULL
  
  # Update <title> ------------------------------------------------------------
  
  message("    <title>")
  
  # Notify the user that this is a Darwin Core Archive
  eml_parent$dataset$title <- paste(
    eml_parent$dataset$title, "(Reformatted to a Darwin Core Archive)")
  
  # Update <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  
  eml_parent$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")
  
  # Updating <abstract> -------------------------------------------------------
  
  message("    <abstract>")
  
  # Get parent abstract
  src_abstract <- unname(
    unlist(
      stringr::str_remove_all(
        eml_parent$dataset$abstract, 
        "</?para>"
      )
    )
  )
  
  # Reset abstract node
  eml_parent$dataset$abstract <- list()
  eml_parent$dataset$abstract$section <- list()
  eml_parent$dataset$abstract$para <- list()
  
  # Add boiler-plate ecocomDP (paragraph 1)
  eml_parent$dataset$abstract$para[[length(eml_parent$dataset$abstract$para) + 1]] <- paste(
    "This data package is formatted according to the 'ecocomDP', a data",
    "package design pattern for ecological community surveys, and data from",
    "studies of composition and biodiversity. For more information on the",
    "ecocomDP project see https://github.com/EDIorg/ecocomDP/tree/master, or",
    "contact EDI https://environmentaldatainitiative.org."
  )
  
  # Add boiler-plate ecocomDP (paragraph 2)  
  eml_parent$dataset$abstract$para[[length(eml_parent$dataset$abstract$para) + 1]] <- paste(
    "This Level-1 data package was derived from the Level-0 data package",
    "found here:",
    paste0(
      'https://portal.edirepository.org/nis/mapbrowse?scope=', scope,
      '&identifier=', identifier, '&revision=', revision
    )
  )
  
  # Add boiler-plate ecocomDP (paragraph 3)
  eml_parent$dataset$abstract$para[[length(eml_parent$dataset$abstract$para) + 1]] <- paste(
    "The abstract below was extracted from the Level-0 data package and is",
    "included for context:"
  )
  
  # Add parent abstract (paragraph 4)
  for (i in 1:length(src_abstract)) {
    eml_parent$dataset$abstract$para[[length(eml_parent$dataset$abstract$para) + 1]] <- 
      src_abstract[i]
  }
  
  # Update <keywordSet> -------------------------------------------------------
  
  message("    <keywordSet>")
  
  # TODO: Add measurement variable in a standardized and human readable way. Could
  # also annotate /eml/dataset with variable mapping values.
  
  # TODO: Add GBIF terms at /eml/dataset (first) and /eml/dataset/dataTable (second)
  
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
  eml_parent$dataset$keywordSet[[length(eml_parent$dataset$keywordSet) + 1]] <- list(
    keywordThesaurus = keywordSet$keywordThesaurus[1],
    keyword = as.list(keywordSet$keyword))
  
  # Update <intellectualRights> -----------------------------------------------
  # Use parent intellectual rights or CC0 if none exists
  
  if (is.null(eml_parent$dataset$intellectualRights)) {
    message("    <intellectualRights>")
    eml_parent$dataset$intellectualRights <- EML::set_TextType(
      system.file('intellectual_rights_cc0_1.txt', package = 'ecocomDP'))
  }
  
  # Update <contact> ----------------------------------------------------------
  # Add ecocomDP creator as a contact incase questions arise
  
  message("    <contact>")
  eml_parent$dataset$contact <- list(
    eml_parent$dataset$contact, 
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
  src_methods <- eml_parent$data$methods$methodStep
  
  # TODO: Add boiler plate as first para of methods followed by the L0 methods
  
  eml_parent$dataset$methods <- list()
  eml_parent$dataset$methods$methodStep <- list()
  
  # Add ecocomDP methods (paragraph 1)
  eml_parent$dataset$methods$methodStep[[length(eml_parent$dataset$methods$methodStep) + 1]] <- 
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
  eml_parent$dataset$methods$methodStep[[length(eml_parent$dataset$methods$methodStep) + 1]] <- 
    src_methods
  
  # Add provenance metadata (paragraph 3). Read provenance from the EDI Data 
  # Repository and remove creator and contact IDs to preempt ID clashes. 
  # Metadata is written to tempdir() so EML::read_eml() can apply its unique 
  # parsing algorithm.
  provenance <- xml2::read_xml(
    paste0(
      "https://pasta.lternet.edu/package/provenance/eml_parent", "/", scope,
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
  
  eml_parent$dataset$methods$methodStep[[length(eml_parent$dataset$methods$methodStep) + 1]] <- 
    provenance
  
  # Update <dataTable> --------------------------------------------------------
  
  # TODO: Combine boiler-plate DwC-A table attributes with table specific metadata

  # Add <otherEntity> ---------------------------------------------------------
  
  # TODO: Add meta.xml
  
  # Update <eml_parent> --------------------------------------------------------------
  
  eml_parent$schemaLocation <- "https://eml_parent.ecoinformatics.org/eml_parent-2.2.0  https://nis.lternet.edu/schemas/EML/eml_parent-2.2.0/xsd/eml_parent.xsd"
  eml_parent$packageId <- child.package.id
  eml_parent$system <- "edi"
  
  message("  </dataset>")
  message("</eml_parent>")
  
  # Write EML -----------------------------------------------------------------
  
  message("Writing EML")
  emld::eml_version("eml_parent-2.2.0")
  EML::write_eml(
    eml_parent, 
    paste0(path, "/", child.package.id, ".xml"))
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")
  
  validation_result <- EML::eml_validate(eml_parent)
  
  if (validation_result == "TRUE"){
    message("EML passed validation!")
  } else {
    message("EML validaton failed. See warnings for details.")
  }
  
}