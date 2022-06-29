#' Create EML metadata
#' 
#' @param path (character) Path to the directory containing ecocomDP tables, conversion script, and where EML metadata will be written.
#' @param source_id (character) Identifier of a data package published in a supported repository. Currently, the EDI Data Repository is supported.
#' @param derived_id (character) Identifier of the dataset being created.
#' @param script (character) Name of file used to convert \code{source_id} to \code{derived_id}.
#' @param script_description (character) Description of \code{script}.
#' @param is_about (named character) An optional argument for specifying dataset level annotations describing what this dataset "is about".
#' @param contact (data.frame) Contact information for the person that created this ecocomDP dataset, containing these columns:
#'    \itemize{
#'        \item givenName
#'        \item surName
#'        \item organizationName
#'        \item electronicMailAddress
#'    }
#' @param user_id (character) Identifier of user associated with \code{user_domain}.
#' @param user_domain (character) Domain (data repository) the \code{user_id} belongs to. Currently, EDI is supported.
#' @param basis_of_record (character) An optional argument to facilitate creation of a Darwin Core record from this dataset using \code{convert_to_dwca()}. Use this to define the Darwin Core property \href{https://dwc.tdwg.org/terms/#dwc:basisOfRecord}{basisOfRecord} as \href{http://rs.tdwg.org/dwc/terms/HumanObservation}{HumanObservation} or \href{http://rs.tdwg.org/dwc/terms/MachineObservation}{MachineObservation}.
#' @param url (character) URL to the publicly accessible directory containing ecocomDP tables, conversion script, and EML metadata. This argument supports direct download of the data entities by a data repository and is used for automated revisioning and publication.
#'
#' @return An EML metadata file.
#'
#' @details This function creates an EML record for an ecocomDP by combining metadata from \code{source_id} with boiler-plate metadata describing the ecocomDP model. Changes to the \code{source_id} EML include:
#'     \itemize{
#'         \item \strong{<access>} Adds \code{user_id} to the list of principals granted read and write access to the ecocomDP data package this EML describes.
#'         \item \strong{<title>} Adds a note that this is a derived data package in the ecocomDP format.
#'         \item \strong{<pubDate>} Adds the date this EML was created.
#'         \item \strong{<abstract>} Adds a note that this is a derived data package in the ecocomDP format.
#'         \item \strong{<keywordSet} Adds the "ecocomDP" keyword to enable search and discovery of all ecocomDP data packages in the data repository it is published, and 7 terms from the LTER Controlled vocabulary: "communities", "community composition", "community dynamics", "community patterns", "species composition", "species diversity", and "species richness". Darwin Core Terms listed under \code{basis_of_record} are listed and used by \code{convert_to_dwca()} to create a Darwin Core Archive of this ecocomDP data package.
#'         \item \strong{<intellectualRights>} Keeps intact the original intellectual rights license \code{source_id} was released under, or uses \href{https://creativecommons.org/publicdomain/zero/1.0/legalcode}{CCO} if missing.
#'         \item \strong{<taxonomicCoverage>} Appends to the taxonomic coverage element with data supplied in the ecocomDP taxon table.
#'         \item \strong{<contact>} Adds the ecocomDP creator as a point of contact.
#'         \item \strong{<methodStep>} Adds a note that this data package was created by the \code{script}, and adds provenance metadata noting that this is a derived dataset and describes where the \code{source_id} can be accessed.
#'         \item \strong{<dataTables>} Replaces the \code{source_id} table metadata with descriptions of the the ecocomDP tables.
#'         \item \strong{<otherEntity>} Adds \code{script} and \code{script_description}. otherEntities of \code{source_id} are removed.
#'         \item \strong{<annotations>} Adds boilerplate annotations describing the ecocomDP at the dataset, entity, and entity attribute levels.
#'     }
#'     
#'     Taxa listed in the taxon table, and resolved to one of the supported authority systems (i.e. \href{https://www.itis.gov/}{ITIS}, \href{https://www.marinespecies.org/}{WORMS}, or \href{https://gbif.org}{GBIF}), will have their full taxonomic hierarchy expanded, including any common names for each level.
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' # Create directory with ecocomDP tables for create_eml()
#' mypath <- paste0(tempdir(), "/data")
#' dir.create(mypath)
#' inpts <- c(ants_L1$tables, path = mypath)
#' do.call(write_tables, inpts)
#' file.copy(system.file("extdata", "create_ecocomDP.R", package = "ecocomDP"), mypath)
#' dir(mypath)
#' 
#' # Describe, with annotations, what the source L0 dataset "is about"
#' dataset_annotations <- c(
#'   `species abundance` = "http://purl.dataone.org/odo/ECSO_00001688",
#'   Population = "http://purl.dataone.org/odo/ECSO_00000311",
#'   `level of ecological disturbance` = "http://purl.dataone.org/odo/ECSO_00002588",
#'   `type of ecological disturbance` = "http://purl.dataone.org/odo/ECSO_00002589")
#' 
#' # Add self as contact information incase questions arise
#' additional_contact <- data.frame(
#'   givenName = 'Colin',
#'   surName = 'Smith',
#'   organizationName = 'Environmental Data Initiative',
#'   electronicMailAddress = 'csmith@mail.com',
#'   stringsAsFactors = FALSE)
#' 
#' # Create EML
#' eml <- create_eml(
#'   path = mypath,
#'   source_id = "knb-lter-hfr.118.33",
#'   derived_id = "edi.193.5",
#'   is_about = dataset_annotations,
#'   script = "create_ecocomDP.R",
#'   script_description = "A function for converting knb-lter-hrf.118 to ecocomDP",
#'   contact = additional_contact,
#'   user_id = 'ecocomdp',
#'   user_domain = 'EDI',
#'   basis_of_record = "HumanObservation")
#' 
#' dir(mypath)
#' View(eml)
#' 
#' # Clean up
#' unlink(mypath, recursive = TRUE)
#' }
#'
create_eml <- function(path,
                       source_id, 
                       derived_id, 
                       script,
                       script_description,
                       is_about = NULL,
                       contact,
                       user_id, 
                       user_domain,
                       basis_of_record = NULL,
                       url = NULL) {
  
  message("Creating EML for derived data package (" , derived_id, ")")
  
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
  
  validate_arguments(fun.name = "create_eml", fun.args = as.list(environment()))
  
  # Parameterize --------------------------------------------------------------
  
  # Read attributes of ecocomDP tables for reference
  
  attr_tbl <- read_criteria()
  attr_tbl <- attr_tbl[!is.na(attr_tbl$column), ]
  
  # Get table names for this L1 dataset for use in EAL_make_eml()
  
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
  
  # Arrange tables in the preferred order to be listed in the EML
  
  fext <- unique(tools::file_ext(data.table))
  preferred_order <- c(
    "observation", "observation_ancillary", "location", "location_ancillary", 
    "taxon", "taxon_ancillary", "dataset_summary",  "variable_mapping")
  
  use_i <- preferred_order %in% tools::file_path_sans_ext(data.table)
  data.table <- paste0(preferred_order[use_i], ".", fext)
  
  # Match table names of this L1 to their boiler plate descriptions for use in 
  # EAL_make_eml()
  
  descriptions <- data.table::fread(
    system.file("extdata", "table_descriptions.txt", package = "ecocomDP"))
  
  data.table.description <- descriptions$description[
    match(
      stringr::str_remove_all(
        data.table, 
        "(\\.[:alnum:]*$)"), 
      descriptions$table_name)]
  
  # Map scripts and their descriptions to their EAL_make_eml() 
  # equivalents
  
  if (!is.null(script)) {
    other.entity <- script
    other.entity.description <- script_description
  } else {
    other.entity <- NULL
    other.entity.description <- NULL
  }
  
  # Expand url for each data object of this L1 for use in 
  # EAL_make_eml()
  
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
  
  message("Reading EML of L0 data package ", source_id)
  
  # Create two objects of the same metadata, eml_L0 (emld list object) for
  # editing, and xml_L0 (xml_document) for easy parsing
    
  if (environment == "production") {
    url_parent <- paste0(
      "https://pasta.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  } else if (environment == "staging") {
    url_parent <- paste0(
      "https://pasta-s.lternet.edu/package/metadata/eml/",
      stringr::str_replace_all(source_id, "\\.", "/"))
  }

  eml_L0 <- EML::read_eml(url_parent)
  xml_L0 <- suppressMessages(read_eml(source_id))
  
  # Remove L0 elements that should not be inherited by the L1
  
  eml_L0$dataset$dataTable <- NULL
  eml_L0$dataset$spatialRaster <- NULL
  eml_L0$dataset$spatialVector <- NULL
  eml_L0$dataset$storedProcedure <- NULL
  eml_L0$dataset$view <- NULL
  eml_L0$dataset$otherEntity <- NULL
  
  eml_L0$additionalMetadata <- NULL
  
  # Create L1 EML -------------------------------------------------------------
  
  message("Creating EML of L1 data package ", derived_id)
  
  # This will not be a full EML record, it will only contain sections of the L1 
  # EML to combined with the L0 EML.
  
  # Create list of inputs to EAL_make_eml()
  
  eal_inputs <- EAL_template_arguments(
    path = system.file("extdata", "/ecocomDP", package = "ecocomDP"), 
    data.path = path, 
    data.table = data.table,
    other.entity = script)
  
  eal_inputs$path <- system.file("extdata", "/ecocomDP", package = "ecocomDP")
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
  eal_inputs$package.id <- derived_id
  eal_inputs$user.id <- user_id
  eal_inputs$user.domain <- user_domain
  eal_inputs$return.obj <- TRUE
  
  
  # Remove unused data table attributes templates. All boiler plate attributes*
  # files are read in with EAL_template_arguments() above, but 
  # only the ones being used should be kept and used in 
  # EAL_make_eml().
  
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
  # EAL_make_eml().
  
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
      
      datetime_format <- parse_datetime_frmt_from_vals(datetime)
      if ((is.null(datetime_format)) & (i != "attributes_observation.txt")) { # Default to observation table's datetime format specifier if no date time in ancillary tables. This prevents an EML schema validation error, where datetime attributes must have a format specified
        use_i <- eal_inputs$x$template[["attributes_observation.txt"]]$content$dateTimeFormatString != ""
        datetime_format <- eal_inputs$x$template[["attributes_observation.txt"]]$content$dateTimeFormatString[use_i]
      }
      
      eal_inputs$x$template[[i]]$content$dateTimeFormatString[
        eal_inputs$x$template[[i]]$content$attributeName == date_column] <- 
        datetime_format
      
    }
  }
  
  # Get table attributes and definitions from EML then create catvars templates for each data table of this dataset
  
  defs <- get_attr_defs(xml_L0)
  
  r <- lapply(
    names(eal_inputs$x$data.table),
    function(tbl) {
      has_varname <- "variable_name" %in% colnames(eal_inputs$x$data.table[[tbl]]$content)
      if (has_varname) {
        univars <- unique(eal_inputs$x$data.table[[tbl]]$content$variable_name)
        unidefs <- defs[names(defs) %in% univars]
        if (length(unidefs) == 0) { # FIXME sometimes there's no match, but could use variable_mapping vals if exists
          unidefs <- rep("NA", length(univars))
          catvars_template <- data.frame(
            attributeName = "variable_name",
            code = univars,
            definition = unname(unidefs),
            stringsAsFactors = FALSE)
          return(list(content = catvars_template))
        } else {
          catvars_template <- data.frame(
            attributeName = "variable_name",
            code = names(unidefs),
            definition = unname(unidefs),
            stringsAsFactors = FALSE)
          return(list(content = catvars_template))
        }
      }
    })
  names(r) <- paste0("catvars_", tools::file_path_sans_ext(names(eal_inputs$x$data.table)), ".txt")
  r <- Filter(Negate(is.null), r)
  eal_inputs$x$template <- c(eal_inputs$x$template, r)

  # Create the taxonomic_coverage template used by EAL_make_eml()
  # from the taxon table of ecocomDP.

  f <- stringr::str_subset(
    names(eal_inputs$x$data.table),
    "taxon\\.[:alpha:]*$")
  taxon <- eal_inputs$x$data.table[[f]]$content

  # Handle exceptions
  if (!is.null(taxon$authority_system)) { # Default to taxonRankValue if missing authority cols
    authsys <- taxon$authority_system
  } else {
    authsys <- NA_character_
  }
  if (!is.null(taxon$authority_taxon_id)) {
    authid <- taxon$authority_taxon_id
  } else {
    authid <- NA_character_
  }
  authsys <- ifelse(is.na(authid), NA_character_, authsys) # Default to taxonRankValue if missing any required authority values
  authid <- ifelse(is.na(authsys), NA_character_, authid)
  
  taxonomic_coverage <- data.frame(
    name = taxon$taxon_name,
    name_type = "scientific",
    name_resolved = taxon$taxon_name,
    authority_system = authsys,
    authority_id = authid,
    stringsAsFactors = FALSE)
  
  eal_inputs$x$template$taxonomic_coverage.txt$content <- taxonomic_coverage

  # The annotations template read in with EAL_template_arguments()
  # serves as a map from tables of this L1 to the boilerplate annotations 
  # which are compiled here.
  
  annotations_map <- eal_inputs$x$template$annotations.txt$content
  annotations <- annotations_map[0, ]
  
  annotations <- rbind(
    annotations,
    annotations_map[annotations_map$context %in% "eml", ])
  if (!is.null(is_about)) {
    additional_dataset_annotations <- data.frame(
      id = "/dataset",
      element = "/dataset",
      context = "eml",
      subject = "dataset",
      predicate_label = "is about",
      predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
      object_label = names(is_about),
      object_uri = unname(is_about),
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
      table_annotations$id, 
      paste0("(?<=/)", table, "(?=$|/)"),
      i)
    table_annotations$context <- stringr::str_replace(
      table_annotations$context, table, i)
    table_annotations$subject <- stringr::str_replace(
      table_annotations$subject, paste0("^", table, "$"), i)
    annotations <- rbind(annotations, table_annotations)
  }
  
  variable_mapping <- stringr::str_subset(
    names(eal_inputs$x$data.table),
    "variable_mapping")
  if (length(variable_mapping) != 0) {
    tblnms_varmap <- eal_inputs$x$data.table[[variable_mapping]]$content$table_name # remove missing tables from variable_mapping
    tblnms_input <- tools::file_path_sans_ext(data.table)
    tbls2keep <- tblnms_varmap %in% tblnms_input
    eal_inputs$x$data.table[[variable_mapping]]$content <- eal_inputs$x$data.table[[variable_mapping]]$content[tbls2keep, ]
    variable_mappings_annotations <- lapply(
      unique(eal_inputs$x$data.table[[variable_mapping]]$content$table_name),
      function(table) {
        variable_mapping_subset <- dplyr::filter(
          eal_inputs$x$data.table[[variable_mapping]]$content, 
          table_name == table)
        file_name <- stringr::str_subset(
          names(eal_inputs$x$data.table),
          paste0(table, "\\.[:alpha:]*$"))
        
        if (!is.null(variable_mapping_subset$mapped_label)) { # Handle missing columns
          objlbl <- variable_mapping_subset$mapped_label
        } else {
          objlbl <- ""
        }
        if (!is.null(variable_mapping_subset$mapped_id)) {
          objuri <- variable_mapping_subset$mapped_id
        } else {
          objuri <- ""
        }
        
        annotation <- data.frame(
          id = paste0("/", file_name, "/variable_name"),
          element = "/dataTable/attribute",
          context = file_name,
          subject = "variable_name",
          predicate_label = "is about",
          predicate_uri = "http://purl.obolibrary.org/obo/IAO_0000136",
          object_label = objlbl,
          object_uri = objuri,
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
  annotations <- annotations[stats::complete.cases(annotations), ]
  
  eal_inputs$x$template$annotations.txt$content <- annotations
  
  # Only include metadata for existing columns (attributes)
  for (i in data.table) {
    table <- stringr::str_remove(i, "\\.[:alpha:]*$")
    tmplt <- paste0("attributes_", table, ".txt")
    attrnms <- eal_inputs$x$template[[tmplt]]$content$attributeName
    colnms <- colnames(eal_inputs$x$data.table[[i]]$content)
    attrs_to_keep <- attrnms %in% colnms
    eal_inputs$x$template[[tmplt]]$content <- eal_inputs$x$template[[tmplt]]$content[attrs_to_keep, ]
  }

  # Call EAL_make_eml()
  eml_L1 <- suppressWarnings(
    suppressMessages(
      do.call(
        EAL_make_eml,
        eal_inputs[
          names(eal_inputs) %in% names(formals(EAL_make_eml))])))
  
  # Update <eml> --------------------------------------------------------------
  
  message("Updating:")
  message("<eml>")
  eml_L0$schemaLocation <- paste0(
    "https://eml.ecoinformatics.org/eml-2.2.0  ",
    "https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd")
  eml_L0$packageId <- derived_id
  eml_L0$system <- "edi"
  
  # Update <access> -----------------------------------------------------------
  
  # Access control rules are used by some repositories to manage 
  # editing, viewing, downloading permissions. Adding the user_id and 
  # user_domain here expands editing permission to the creator of the DwC-A 
  # data package this EML will be apart of.
  
  # A patch for EML::read_eml() handling of nodes with 1 child vs > 1 child
  if (!is.null(names(eml_L0$access$allow))) {
    eml_L0$access$allow <- list(eml_L0$access$allow)
  }
  
  
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
  # Two options for combining keyword sets, because of variation in the return 
  # from EML::read_eml() (i.e. lists nodes when length > 1, and unlists when 
  # length = 1).
  if (!is.null(names(eml_L0$dataset$keywordSet))) {
    eml_L0$dataset$keywordSet <- c(list(eml_L0$dataset$keywordSet),
                                   eml_L1$dataset$keywordSet)
  } else {
    eml_L0$dataset$keywordSet <- c(eml_L0$dataset$keywordSet,
                                   eml_L1$dataset$keywordSet)
  }
  
  # Add Darwin Core basisOfRecord
  if (!is.null(basis_of_record)) {
    eml_L0$dataset$keywordSet <- c(
      eml_L0$dataset$keywordSet,
      list(
        list(
          keywordThesaurus = "Darwin Core Terms",
          keyword = as.list(paste0("basisOfRecord: ", basis_of_record)))))
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
  
  # Two options for combining taxonomic classifications, because of variation 
  # in the return from EML::read_eml() (i.e. lists nodes when length > 1, and 
  # unlists when length = 1).
  
  if (!is.null(names(eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification))) {
    eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification <- c(
      list(eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification),
      eml_L1$dataset$coverage$taxonomicCoverage$taxonomicClassification)
  } else {
    eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification <- c(
      eml_L0$dataset$coverage$taxonomicCoverage$taxonomicClassification,
      eml_L1$dataset$coverage$taxonomicCoverage$taxonomicClassification)
  }
  
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
  
  # Parse components to be reordered and combined for the L1
  methods_L1 <- eml_L1$dataset$methods$methodStep
  # Get provenance metadata
  r <- suppressMessages(
    api_get_provenance_metadata(
      package.id = source_id,
      environment = environment))
  xml2::xml_set_attrs(xml2::xml_find_all(r, ".//*[@id]"), c(id = NULL)) # Remove attributes to prevent id clashing and schema invalidation
  r <- EML::read_eml(r)
  provenance_L1 <- list(
    dataSource = r$dataSource,
    description = r$description)
  # Remove any provenance nodes from the L0 metadata, otherwise they will be
  # transferred to the L1 metadata, which would be an inaccurate representation
  # of the provenance chain.
  method_steps <- xml2::xml_find_all(xml_L0, "./dataset/methods/methodStep")
  prov <- unlist(lapply(method_steps, is_prov))
  eml_L0$dataset$methods$methodStep <- eml_L0$dataset$methods$methodStep[!prov]
  # Combine L1 methods, L0 methods, and L0 provenance
  eml_L0$dataset$methods$methodStep <- c(
    list(methods_L1),
    list(eml_L0$dataset$methods$methodStep),
    list(provenance_L1))
  
  # Update <dataTable> --------------------------------------------------------
  
  message("    <dataTable>")
  eml_L0$dataset$dataTable <- eml_L1$dataset$dataTable
  
  # Update <otherEntity> ------------------------------------------------------
  
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
    paste0(path, "/", derived_id, ".xml"))

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
