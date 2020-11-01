#' Make EML metadata for ecocomDP tables
#' 
#' Run this function AFTER you have validated your ecocomDP tables with 
#' \code{ecocomDP::validate_ecocomDP()}.
#'
#' @param path 
#'     (character) Path to the directory containing ecocomDP data tables, conversion scripts, and where EML metadata will be written.
#' @param parent.package.id
#'     (character) Parent data package ID (e.g. "knb-lter-hfr.118.28"). Only 
#'     EDI Data Repository package IDs are currently supported.
#' @param child.package.id
#'     (character) Child data package ID (e.g. "edi.53.1") to be uploaded to 
#'     EDI Data Repository.
#' @param script
#'     (character) Names of scripts used to convert and repackage the source 
#'     data into the ecocomDP.
#' @param script.description
#'     (character) A description for each object listed under \code{script}.
#' @param is.about
#'     (named character) Dataset level annotations describing what this dataset "is about" (L0 keywords converted to object label + URI pairs) describing what the L1 dataset is about. (e.g. \code{is.about = c('level of ecological disturbance' = "http://purl.dataone.org/odo/ECSO_00002588",' type of ecological disturbance' = "http://purl.dataone.org/odo/ECSO_00002589")}).
#' @param cat.vars
#'     (data frame) Categorical variables with associated definitions and 
#'     units. Create a cat.vars data frame with the 
#'     \code{ecocomDP::define_variables()} function. Variables that can't be 
#'     defined by the parent package EML should be manually supplied to the 
#'     cat.vars data frame in one of the scripts specified in the 
#'     \code{script} argument.
#' @param contact
#'    (data frame) Contact information of this ecocomDP creator.
#'    The data frame must have these columns:
#'    \itemize{
#'        \item{givenName}
#'        \item{surName}
#'        \item{organizationName}
#'        \item{electronicMailAddress}
#'    }
#' @param user.id
#'     (character) ID(s) of user account(s) associated with the data repository
#'     in which this ecocomDP data package will be archived. Only EDI Data 
#'     Repository IDs are currently supported.
#' @param user.domain
#'     (character) Domain of the \code{user.id}(s). Only "EDI" and "LTER" are 
#'     currently supported. If more than one, then supply as a vector of 
#'     character strings in the same order as the corresponding \code{user.id}.
#' @param basis.of.record
#'     (character) To create a Darwin Core record from this ecocomDP using
#'     \code{L1_to_L2_DwCA()} then the Darwin Core property 
#'     \href{basisOfRecord}{https://dwc.tdwg.org/terms/#dwc:basisOfRecord}
#'     requires definition as 
#'     \href{HumanObservation}{http://rs.tdwg.org/dwc/terms/HumanObservation} or 
#'     \href{MachineObservation}{http://rs.tdwg.org/dwc/terms/MachineObservation}.
#' @param url
#'     (character) URL to the publicly accessible directory containing ecocomDP data tables, conversion scripts, and EML metadata. This argument supports direct download of the data entities by a data repository and is used within the scope of the ecocomDP project for automated revision and upload of ecocomDP data packages and derived products.
#'
#' @return 
#'     An EML metadata record for the ecocomDP dataset defined by the arguments
#'     to this function.
#'
#' @details 
#'     This function creates an EML record for an Ecological Community Data 
#'     Pattern (ecocomDP) combining metadata from the parent data package and
#'     boiler-plate metadata describing the ecocomDP tables. Changes to the 
#'     parent EML include:
#'     \itemize{
#'         \item \strong{<access>} Adds the \code{user.id} to the list of 
#'         principals granted read and write access to the ecocomDP data 
#'         package this EML describes.
#'         \item \strong{<title>} Adds a note that this is a derived data 
#'         package in the ecocomDP format.
#'         \item \strong{<pubDate>} Adds the date this EML was created.
#'         \item \strong{<abstract>} Adds a note that this is a derived data 
#'         package in the ecocomDP format.
#'         \item \strong{<keywordSet} Adds the "ecocomDP" keyword to enable
#'         search and discovery of all ecocomDP data packages in the EDI Data
#'         Repository, and 7 terms from the LTER Controlled vocabulary:
#'         "communities", "community composition", "community dynamics", 
#'         "community patterns", "species composition", "species diversity",
#'         and "species richness". Darwin Core Terms listed under \code{
#'         basis.of.record} are listed and used by \code{L1_to_L2_DwCA()} 
#'         to create a Darwin Core Archive of this ecocomDP data package.
#'         \item \strong{<intellectualRights>} Keeps intact the intellectual
#'         rights license of the parent data package, or replaces it with
#'         "CCO" (https://creativecommons.org/publicdomain/zero/1.0/legalcode).
#'         \item \strong{<taxonomicCoverage>} Updates the taxonomic coverage 
#'         element with data supplied in the taxon table of the ecocomDP.
#'         \item \strong{<contact>} Adds contact information of the ecocomDP
#'         creator to the list of contacts in the parent data package EML.
#'         \item \strong{<methodStep>} Adds a note that this data package was
#'         created by the scripts listed under the \code{script} argument,
#'         and adds provenance metadata noting that this is a derived data 
#'         and describing where the parent data package can be accessed.
#'         \item \strong{<dataTables>} Replaces the parent data package data
#'         tables metadata with boiler-plate metadata for the ecocomDP tables.
#'         \item \strong{<otherEntity>} Describes scripts listed in the 
#'         \code{script} and \code{script.description} arguments. Any other
#'         entities listed in the parent EML are removed.
#'     }
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' # Set path to ecocomDP tables and scripts
#' path <- "/Users/csmith/Desktop/ecocomDP"
#' 
#' # Validate ecocomDP tables
#' ecocomDP::validate_ecocomDP(path)
#' 
#' # Use parent EML to define variables of the ecocomDP and manually add variable 
#' # names and definitions created in the ecocomDP re-formatting process.
#' catvars <- ecocomDP::define_variables(path, "knb-lter-sev.29.12")
#' catvars$unit[catvars$code == "Count"] <- "number"
#' catvars$definition[catvars$code == "Comments"] <- "A special statement related to an observation"
#' 
#' # Create contact information for the ecocomDP creator
#' additional_contact <- data.frame(
#'   givenName = 'Colin',
#'   surName = 'Smith',
#'   organizationName = 'Environmental Data Initiative',
#'   electronicMailAddress = 'csmith@@wisc.edu',
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Create EML
#' ecocomDP::make_eml(
#'   path = path,
#'   parent.package.id = "knb-lter-sev.29.12",
#'   child.package.id = "edi.101.5",
#'   script = c(
#'     "convert_sev29_to_ecocomDP.R", 
#'     "package_edi_333.R"
#'   ),
#'   script.description = c(
#'     "R script for creating the ecocomDP tables.",
#'     "R script for creating the ecocomDP EML"
#'   ),
#'   cat.vars = catvars,
#'   contact = additional_contact,
#'   user.id = 'csmith',
#'   user.domain = 'LTER',
#' )
#' }
#'
make_eml <- function(path,
                     parent.package.id, 
                     child.package.id, 
                     script,
                     script.description,
                     is.about = NULL,
                     cat.vars,
                     contact,
                     user.id, 
                     user.domain,
                     basis.of.record = NULL,
                     url = NULL) {
  
  message("Creating EML for derived data package (" , child.package.id, ")")
  
  # Validate inputs -----------------------------------------------------------
  
  if (missing(path)) {
    stop(
      call. = FALSE, 
      paste(
        "Input argument 'path' is missing.", "Please specify the directory",
        "to the ecocomDP tables and any associated processing scripts."
      )
    )
  }
  
  # FIXME: Check that the data package exists in the EDI data repository
  if (missing(parent.package.id)) {
    stop(
      call. = FALSE, 
      paste(
        "Input argument 'parent.package.id' is missing. Please specify the",
        "ID of the parent data package from which this ecocomDP was derived.",
        "NOTE: Only data packages in the EDI Data Repository are currently",
        "supported."
      )
    )
  }
  
  # FIXME: Check that the data package ID is in one of the expected formats
  # (i.e. LTER or EDI). Issue warning, not error.
  if (missing(child.package.id)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'child.package.id' is missing. Please specify the",
        "ID of the data package this EML will describe."
      )
    )
  }
  
  if (missing(script)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'script' is missing. Please specifiy any scripts used",
        "to convert and repackage the source data into the ecocomDP."
      )
    )
  }
  
  if (missing(script.description)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'script.description' is missing. Please describe the",
        "scripts listed under the 'script' argument."
      )
    )
  }
  
  script <- EDIutils::validate_file_names(path, script)
  
  if (length(script) != length(script.description)) {
    stop(
      call. = FALSE,
      paste(
        "The number of items listed under the 'script' and",
        "'script.description' arguments must match."
      )
    )
  }
  
  
  if (!is.null(is.about)) {
    names <- names(is.about)
    uris <- unname(is.about)
    if (any(names == "") | any(uris == "")) {
      stop(
        "Input argument 'is.about' must be a named character vector", 
        call. = FALSE)
    }
  }
  
  if (is.null(cat.vars)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'cat.vars' is missing. Create this input using the",
        "function 'ecocomDP::define_variables()'."
      )
    )
  }

  if (!all(c("tableName", "attributeName", "code", "definition", "unit") %in%
      names(cat.vars))) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'cat.vars' is missing required columns. Required",
        "columns are: 'tableName', 'attributeName', 'code', 'definition',",
        "'unit'."
      )
    )
  }

  if (missing(contact)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'contact' is missing. Please specify the contact",
        "information for the creator of this ecocomDP data package. Supply",
        "this information as a data frame with the columns 'givenName',",
        "'surName', 'organizationName', 'electronicMailAddress'."
      )
    )
  }
  
  if (!all(
    c('givenName', 'surName', 'organizationName', 'electronicMailAddress') %in% 
    names(contact))) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'contact' is missing required columns.", 
        "This data frame should have the columns: 'givenName',",
        "'surName', 'organizationName', 'electronicMailAddress'."
      )
    )
  }
  
  # FIXME: Check the input 'user.id' against the list of EDI/LTER users.
  if (missing(user.id)) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'user.id' is missing. Please specify the user ID under",
        "which this data package will be uploaded. NOTE: Only EDI Data",
        "Repository user IDs are currently supported."
      )
    )
  }
  
  if (missing(user.domain)) {
    stop(
      call. = FALSE,
      "Input argument 'user.domain' is missing. Please specify the domain",
      "under which the 'user.id' argument exists. NOTE: Only 'EDI' and 'LTER'",
      "domains are currently supported."
    )
  }
  
  if (!all(user.domain %in% c("LTER", "EDI"))) {
    stop(
      call. = FALSE,
      paste(
        "Input argument 'user.domain' is not one of the currently supported",
        "'EDI' or 'LTER'."
      )
    )
  }
  
  if (length(user.id) != length(user.domain)) {
    stop(
      call. = FALSE,
      paste(
        "The number of items listed under the 'user.id' and",
        "'user.domain' arguments must match."
      )
    )
  }
  
  if (is.null(basis.of.record)) {
    
    # Valuable information a user may want to know.
    warning(
      "No 'basis.of.record' found. Specifiying the Darwin Core ",
      "'basisOfRecord' property as 'HumanObservation' or ",
      "'MachineObservation' enables conversion of this ecocomDP into a ",
      "Darwin Core Archive.", call. = FALSE)
    
    # Check form
    required_terms <- stringr::str_detect(
      basis.of.record, "HumanObservation|MachineObservation")
    if (!any(required_terms)) {
      stop(
        "Input values listed under 'basis.of.record' must be ",
        "'HumanObservation' or 'MachineObservation'", call. = FALSE)
    }
    
  }
  
  # TODO: Check "url"
  
  # Parameterize --------------------------------------------------------------
  
  # Read attributes of ecocomDP tables for reference
  
  attr_tbl <- data.table::fread(
    system.file('validation_criteria.txt', package = 'ecocomDP'))
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  
  # Get table names for this L1 dataset for use in EMLassemblyline::make_eml()
  
  data.table <- unlist(
    lapply(
      unique(attr_tbl$table),
      function(x) {
        ecocomDP_table <- stringr::str_detect(
          list.files(path), 
          paste0("(?<=.{0,10000})", x, "(?=\\.[:alnum:]*$)"))
        if (any(ecocomDP_table)) {
          list.files(path)[ecocomDP_table]
        }
      }))
  
  # Match table names of this L1 to their boiler plate descriptions for use in 
  # EMLassemblyline::make_eml()
  
  descriptions <- data.table::fread(
    system.file("table_descriptions.txt", package = "ecocomDP"))
  
  data.table.description <- descriptions$description[
    match(
      stringr::str_remove_all(
        data.table, 
        "(\\.[:alnum:]*$)"), 
      descriptions$table_name)]
  
  # Map scripts and their descriptions to their EMLassemblyline::make_eml() 
  # equivalents
  
  if (!is.null(script)) {
    other.entity <- script
    other.entity.description <- script.description
  } else {
    other.entity <- NULL
    other.entity.description <- NULL
  }
  
  # Expand url for each data object of this L1 for use in 
  # EMLassemblyline: make_eml()
  
  if (!is.null(url)) {
    if (!is.null(data.table)) {
      data.table.url <- paste0(url, "/", data.table)
    }
    if (!is.null(other.entity)) {
      other.entity.url <- paste0(url, "/", other.entity)
    }
  } else {
    data.table.url <- NULL
    other.entity.url <- NULL
  }

  # Read L0 EML ---------------------------------------------------------------
  
  message("Reading EML of L0 data package ", parent.package.id)
  
  # Create two objects of the same metadata, eml_L0 (emld list object) for
  # editing, and xml_L0 (xml_document) for easy parsing
    
  url_parent <- paste0(
    "https://pasta.lternet.edu/package/metadata/eml/",
    stringr::str_replace_all(parent.package.id, "\\.", "/"))

  eml_L0 <- EML::read_eml(url_parent)

  xml_L0 <- suppressMessages(
    EDIutils::api_read_metadata(parent.package.id))
  
  # Create L1 EML -------------------------------------------------------------
  
  message("Creating EML of L1 data package ", child.package.id)
  
  # This will not be a full EML record, it will only contain sections of the L1 
  # EML to combined with the L0 EML.
  
  # Create list of inputs to EMLassemblyline::make_eml()
  
  eal_inputs <- EMLassemblyline::template_arguments(
    path = system.file("/ecocomDP", package = "ecocomDP"), 
    data.path = path, 
    data.table = data.table,
    other.entity = script)
  
  eal_inputs$path <- system.file("/ecocomDP", package = "ecocomDP")
  eal_inputs$data.path <- path
  eal_inputs$eml.path <- path
  eal_inputs$dataset.title <- "placeholder"
  eal_inputs$data.table <- data.table
  eal_inputs$data.table.description <- data.table.description
  eal_inputs$data.table.url <- data.table.url
  eal_inputs$data.table.quote.character <- rep('"', length(data.table))
  eal_inputs$other.entity <- other.entity
  eal_inputs$other.entity.description <- other.entity.description
  eal_inputs$other.entity.url <- other.entity.url
  eal_inputs$provenance <- parent.package.id
  eal_inputs$package.id <- child.package.id
  eal_inputs$user.id <- user.id
  eal_inputs$user.domain <- user.domain
  eal_inputs$return.obj <- TRUE
  
  
  # Remove unused data table attributes templates. All boiler plate attributes*
  # files are read in with EMLassemblyline::template_arguments() above, but 
  # only the ones being used should be kept and used in 
  # EMLassemblyline::make_eml().
  
  all_attribute_templates <- names(eal_inputs$x$template)[
    stringr::str_detect(
      names(eal_inputs$x$template),
      "attributes_")]
  
  expected_attribute_templates <- paste0(
    "attributes_", 
    stringr::str_remove(data.table, "\\.[:alnum:]*$"), 
    ".txt")
  
  unused_attribute_templates <- all_attribute_templates[
    !(all_attribute_templates %in% expected_attribute_templates)]
  
  eal_inputs$x$template[
    names(eal_inputs$x$template) %in% unused_attribute_templates] <- NULL
  
  # Detect date and time format string directly from each table and add to the
  # corresponding data table attributes template as required by 
  # EMLassemblyline::make_eml().
  
  for (i in expected_attribute_templates) {
    
    date_column <- eal_inputs$x$template[[i]]$content$attributeName[
      eal_inputs$x$template[[i]]$content$class == "Date"]
    
    if (length(date_column) != 0) {
      
      data_table <- which(
        stringr::str_detect(
          names(eal_inputs$x$data.table),
          paste0(
            "^",
            stringr::str_extract(i, "(?<=attributes_).*(?=\\.txt)"),
            "\\.[:alnum:]*$")))
      
      datetime <- eal_inputs$x$data.table[[data_table]]$content[[date_column]]
      
      na_coerced <- suppressWarnings(
        c(
          sum(is.na(lubridate::parse_date_time(datetime, "ymdHMS"))),
          sum(is.na(lubridate::parse_date_time(datetime, "ymdHM"))),
          sum(is.na(lubridate::parse_date_time(datetime, "ymdH"))),
          sum(is.na(lubridate::parse_date_time(datetime, "ymd")))))
      
      datetime_format <- c(
        "YYYY-MM-DD hh:mm:ss",
        "YYYY-MM-DD hh:mm",
        "YYYY-MM-DD hh",
        "YYYY-MM-DD")[
          which(na_coerced == min(na_coerced))[1]]
      
      eal_inputs$x$template[[i]]$content$dateTimeFormatString[
        eal_inputs$x$template[[i]]$content$attributeName == date_column] <- 
        datetime_format
      
    }
  }
  
  # Create categorical variables templates from input "cat.vars" and add to
  # the list of inputs to EMLassemblyline::make_eml()
  
  r <- lapply(
    unique(cat.vars$tableName),
    function(table) {
      categorical_variables_template <- dplyr::select(
        dplyr::filter(cat.vars, tableName == table),
        attributeName, code, definition)
      return(list(content = categorical_variables_template))
    })
  
  names(r) <- paste0("catvars_", unique(cat.vars$tableName), ".txt")
  eal_inputs$x$template <- c(eal_inputs$x$template, r)

  # Create the taxonomic_coverage template used by EMLassemblyline::make_eml()
  # from the taxon table of ecocomDP.

  f <- stringr::str_subset(
    names(eal_inputs$x$data.table),
    "taxon\\.[:alpha:]*$")
  taxon <- eal_inputs$x$data.table[[f]]$content
  
  taxonomic_coverage <- data.frame(
    name = taxon$taxon_name,
    name_type = "scientific",
    name_resolved = taxon$taxon_name,
    authority_system = taxon$authority_system,
    authority_id = taxon$authority_taxon_id,
    stringsAsFactors = FALSE)
  
  eal_inputs$x$template$taxonomic_coverage.txt$content <- taxonomic_coverage

  # The annotations template read in with EMLassemblyline::template_arguments()
  # serves as a map from tables of this L1 to the boilerplate annotations 
  # which are compiled here.
  
  annotations_map <- eal_inputs$x$template$annotations.txt$content
  annotations <- annotations_map[0, ]
  
  annotations <- rbind(
    annotations,
    annotations_map[annotations_map$context %in% "eml", ])
  if (!is.null(is.about)) {
    additional_dataset_annotations <- data.frame(
      id = "/dataset",
      element = "/dataset",
      context = "eml",
      subject = "dataset",
      predicate_label = "is about",
      predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
      object_label = names(is.about),
      object_uri = unname(is.about),
      stringsAsFactors = FALSE)
    annotations <- rbind(annotations, additional_dataset_annotations)
  }
  
  other_entity_annotations <- data.frame(
    id = paste0("/", script),
    element = "/otherEntity",
    context = "dataset",
    subject = script,
    predicate_label = "is about",
    predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
    object_label = "analysis code",
    object_uri = "http://purl.dataone.org/odo/ECSO_00002489",
    stringsAsFactors = FALSE)
  annotations <- rbind(annotations, other_entity_annotations)
  
  for (i in data.table) {
    table <- stringr::str_remove(i, "\\.[:alpha:]*$")
    annotations_subset <- dplyr::filter(
      annotations_map,
      subject %in% table | context %in% table)
    table_annotations <- annotations_subset[
      annotations_subset$subject %in% 
        c(colnames(eal_inputs$x$data.table[[i]]$content), table), ]
    table_annotations$id <- stringr::str_replace(
      annotations_subset$id, 
      paste0("(?<=/)", table, "(?=$|/)"),
      i)
    table_annotations$context <- stringr::str_replace(
      annotations_subset$context, table, i)
    table_annotations$subject <- stringr::str_replace(
      annotations_subset$subject, table, i)
    annotations <- rbind(annotations, table_annotations)
  }
  
  variable_mapping <- stringr::str_subset(
    names(eal_inputs$x$data.table),
    "variable_mapping")
  if (length(variable_mapping) != 0) {
    variable_mappings_annotations <- lapply(
      unique(eal_inputs$x$data.table[[variable_mapping]]$content$table_name),
      function(table) {
        variable_mapping_subset <- dplyr::filter(
          eal_inputs$x$data.table[[variable_mapping]]$content, 
          table_name == table)
        file_name <- stringr::str_subset(
          names(eal_inputs$x$data.table),
          paste0(table, "\\.[:alpha:]*$"))
        annotation <- data.frame(
          id = paste0("/", file_name, "/variable_name"),
          element = "/dataTable/attribute",
          context = file_name,
          subject = "variable_name",
          predicate_label = "is about",
          predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
          object_label = variable_mapping_subset$mapped_label,
          object_uri = variable_mapping_subset$mapped_id,
          stringsAsFactors = FALSE)
        # Remove duplicate annotations or the variable_name attribute (a column
        # containing multiple variables as apart of a "long" table) will have 
        # more than one of the same annotation
        annotation <- dplyr::distinct(
          annotation, 
          object_label, 
          object_uri, 
          .keep_all = TRUE)
        return(annotation)
      })
    annotations <- rbind(
      annotations, 
      data.table::rbindlist(variable_mappings_annotations))
  }
  
  annotations[annotations == ""] <- NA_character_
  annotations <- annotations[complete.cases(annotations), ]
  
  eal_inputs$x$template$annotations.txt$content <- annotations
  
  # Call EMLassemblyline::make_eml()
  
  eml_L1 <- suppressWarnings(
    suppressMessages(
      do.call(
        EMLassemblyline::make_eml,
        eal_inputs[
          names(eal_inputs) %in% names(formals(EMLassemblyline::make_eml))])))
  
  # Update <eml> --------------------------------------------------------------
  
  message("Updating:")
  message("<eml>")
  eml_L0$schemaLocation <- paste0(
    "https://eml.ecoinformatics.org/eml-2.2.0  ",
    "https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd")
  eml_L0$packageId <- child.package.id
  eml_L0$system <- "edi"
  
  # Update <access> -----------------------------------------------------------
  
  # Access control rules are used by some repositories to manage 
  # editing, viewing, downloading permissions. Adding the user.id and 
  # user.domain here expands editing permission to the creator of the DwC-A 
  # data package this EML will be apart of.
  
  eml_L0$access$allow <- unique(
    c(eml_L0$access$allow, 
      eml_L1$access$allow))

  # Update <dataset> ----------------------------------------------------------
  
  # For purposes of annotation references, the <dataset> attribute (which may
  # have been set by the L0 creator) needs to be set to "dataset", which is 
  # expected by the L1 dataset annotation.
  
  eml_L0$dataset$id <- "dataset"
  
  # Remove <alternateIdentifier> ----------------------------------------------
  
  # Some repositories assign a DOI to this element. Not removing it here 
  # an error when uploading to the repository.
  
  message("  <dataset>")
  message("    <alternateIdentifier>")
  eml_L0$dataset$alternateIdentifier <- NULL
  
  # Update <title> ------------------------------------------------------------
  
  # Add notification the user that this is an ecocomDP data package
  
  message("    <title>")
  eml_L0$dataset$title <- paste(
    eml_L0$dataset$title, "(Reformatted to the ecocomDP Design Pattern)")
  
  # Update <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  eml_L0$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")
  
  # Updating <abstract> -------------------------------------------------------
  
  # Add link to L0 data packages and combine L0 and L1 abstracts
  
  eml_L1$dataset$abstract$para[[1]] <- stringr::str_replace(
    eml_L1$dataset$abstract$para[[1]], 
    "L0_PACKAGE_URL", 
    url_parent)
  
  L1_para <- eml_L1$dataset$abstract$para[[1]]
  L0_para <- xml2::xml_text(
    xml2::xml_find_all(xml_L0, ".//abstract//para"))
  eml_L0$dataset$abstract <- NULL
  
  eml_L0$dataset$abstract$para <- c(
    list(L1_para),
    list(L0_para))
  
  # Update <keywordSet> -------------------------------------------------------
  
  # Add ecocomDP specific keywords to the L0 keywords
  
  message("    <keywordSet>")
  
  eml_L0$dataset$keywordSet <- c(
    eml_L0$dataset$keywordSet,
    eml_L1$dataset$keywordSet)
  
  # Add Darwin Core basisOfRecord
  if (!is.null(basis.of.record)) {
    eml_L0$dataset$keywordSet <- c(
      eml_L0$dataset$keywordSet,
      list(
        list(
          keywordThesaurus = "Darwin Core Terms",
          keyword = as.list(basis.of.record))))
  }

  # Update <intellectualRights> -----------------------------------------------
  
  # Use parent intellectual rights or CC0 if none exists
  
  if (is.null(eml_L0$dataset$intellectualRights)) {
    message("    <intellectualRights>")
    eml_L0$dataset$intellectualRights <- eml_L2$dataset$intellectualRights
  }

  # Update <taxonomicCoverage> ------------------------------------------------
  
  # Combine taxonomic coverage of L0 and L1. While this may provide redundant 
  # information, there isn't any harm in this.

  eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification <- c(
    eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification,
    eml_L1$dataset$coverage$taxonomicCoverage$taxonomicClassification)
  
  # Update <contact> ----------------------------------------------------------
  
  # Add ecocomDP creator to list of contacts
  
  message("    <contact>")
  
  eml_L0$dataset$contact <- c(
    list(
      list(
        individualName = list(
          givenName = contact$givenName,
          surName = contact$surName),
        organizationName = contact$organizationName,
        electronicMailAddress = contact$electronicMailAddress)),
    list(eml_L0$dataset$contact))

  # Update <methods> ----------------------------------------------------------
  
  # Update parent methods with ecocomDP creation process and provenance 
  # metadata to provide the user with a full understanding of how these data 
  # were created
  
  message("    <methods>")
  
  # FIXME: Parse L0 methods from xml rather than emld since the latter can be
  # irregular
  
  # Parse components to be reordered and combined for the L1
  methods_L1 <- eml_L1$dataset$methods$methodStep[[1]]
  eml_L1$dataset$methods$methodStep[[1]] <- NULL
  provenance_L1 <- eml_L1$dataset$methods$methodStep
  
  # Combine L1 methods, L0 methods, and L0 provenance
  eml_L0$dataset$methods$methodStep <- c(
    list(methods_L1),
    list(eml_L0$data$methods$methodStep),
    list(provenance_L1))
  
  # Update <dataTable> --------------------------------------------------------
  
  message("    <dataTable>")
  eml_L0$dataset$dataTable <- eml_L1$dataset$dataTable
  
  # Add <otherEntity> ---------------------------------------------------------
  
  message("    <otherEntity>")
  eml_L0$dataset$otherEntity <- eml_L1$dataset$otherEntity
  
  # Update <annotations> ------------------------------------------------------
  
  message("    <annotations>")
  eml_L0$annotations <- eml_L1$annotations
  
  # Write EML -----------------------------------------------------------------

  message("</eml>")
  message("Writing EML")
  
  emld::eml_version("eml-2.2.0")
  EML::write_eml(
    eml_L0, 
    paste0(path, "/", child.package.id, ".xml"))

  # Validate EML --------------------------------------------------------------

  message("Validating EML")
  
  r <- EML::eml_validate(eml_L0)
  if (isTRUE(r)) {
    message("  Validation passed :)")
  } else {
    message("  Validation failed :(")
  }
  message("Done.")
  
  # Return --------------------------------------------------------------------
  
  return(eml_L0)
  
}
