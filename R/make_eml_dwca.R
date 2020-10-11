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
  
  # TODO: Check the data package exists
  
  # Check contact info
  if (!all(
    c('givenName', 'surName', 'organizationName', 'electronicMailAddress') %in% 
    names(contact))) {
    stop(
      "Input argument 'contact' is missing required columns.", 
      "This data frame should have the columns: 'givenName',",
      "'surName', 'organizationName', 'electronicMailAddress'.",
      call. = FALSE)
  }
  
  # A user.id is required for each user.domain
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
  
  eml_parent <- EML::read_eml(
    paste0(
      "https://pasta.lternet.edu/package/metadata/eml_parent", "/", scope, "/",
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
      
      eml_parent$access$allow[[length(eml_parent$access$allow) + 1]] <- list(
        principal = paste0(
          'uid=', user.id[i], ',o=', user.domain[i], ',dc=ecoinformatics,dc=org'
        ),
        permission = "all"
      )
      
    } else if (user.domain[i] == 'EDI'){
      
      eml_parent$access$allow[[length(eml_parent$access$allow) + 1]] <- list(
        principal = paste0(
          'uid=', user.id[i], ',o=', user.domain[i], ',dc=edirepository,dc=org'
        ),
        permission = "all"
      )
      
    }
    
  }
  
  # Remove <alternateIdentifier> ----------------------------------------------
  # Remove <alternateIdentifier> to prevent downstream errors
  
  eml_parent$dataset$alternateIdentifier <- NULL
  
  # Update <dataset> ----------------------------------------------------------
  
  message("  <dataset>")
  
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