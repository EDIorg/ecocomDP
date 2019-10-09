#' Make EML for a set of ecocomDP tables
#'
#' @description  
#'     Makes EML for a set of ecocomDP tables.
#'
#' @usage 
#'     make_eml(data.path, code.path = data.path, code.files, 
#'     parent.package.id, sep, cat.vars = NULL, intellectual.rights = NULL, 
#'     child.package.id, user.id, affiliation, access.url = NULL, 
#'     additional.contact)
#'
#' @param data.path 
#'     (character) Path to the directory containing ecocomDP tables.
#' @param code.path
#'     (character) Path to the directory containing scripts that reformat the 
#'     parent data package to the child data package.
#' @param code.files
#'     (character) Path to the directory containing scripts that reformat the 
#'     parent data package to the child data package.
#' @param parent.package.id
#'     (character) Parent data package ID (e.g. "knb-lter-hfr.118.28") in the 
#'     EDI Data Repository.
#' @param sep
#'     (character) Field delimiter used in the ecocomDP tables. Valid options 
#'     are "," or "\\t".
#' @param cat.vars
#'     (data frame) Categorical variables with associated definitions and 
#'     units. Create a cat.vars data frame with the `define_variables` 
#'     function. Variables that can't be defined by the parent package EML 
#'     should be manually supplied to the cat.vars data frame.
#' @param intellectual.rights
#'     (character) The intellectual rights license to be used with the 
#'     ecocomDP. If this argument is NULL, then the parent data package 
#'     intellectual rights will be used. Valid options are "CCO" 
#'     (https://creativecommons.org/publicdomain/zero/1.0/legalcode) and
#'     "CCBY" (https://creativecommons.org/licenses/by/4.0/legalcode).
#' @param child.package.id
#'     (character) Child data package ID (e.g. "edi.53.1") to be uploaded to 
#'     EDI Data Repository.
#' @param user.id
#'     (character) User ID of person uploading the ecocomDP data package to the
#'     EDI Data Repository.
#' @param affiliation
#'     (character) Affiliation of the user.id. Valid options are "EDI" or 
#'     "LTER".
#' @param access.url
#'     (character) The base URL access point to ecocomDP tables and scripts 
#'     that can be downloaded by the EDI Data Repository (e.g.
#'     https://lter.limnology.wisc.edu/sites/default/files/data). Alternatively
#'     tables and scripts can be uploaded through the EDI Data Repository 
#'     Portal.
#' @param additional.contact
#'    (data frame) Contact information for creator of the script that converts
#'    the parent data package to the ecocomDP. The data frame must have these
#'    columns:
#'    \itemize{
#'        \item{givenName}
#'        \item{surName}
#'        \item{organizationName}
#'        \item{electronicMailAddress}
#'    }
#'
#' @return 
#'     An EML metadata record for the ecocomDP tables.
#'
#' @details 
#'     This function creates valid EML for an Ecological Community Data Pattern 
#'     (ecocomDP) using a combination of elements from the parent data package, 
#'     boiler plate metadata specific to the ecocomDP, and user supplied 
#'     defined in the conversion script.
#'     
#'     Run \code{make_eml} after you have:
#'     \itemize{
#'         \item Created ecocomDP tables through a fully scripted process.
#'         \item Validated your ecocomDP with `validate_ecocomDP`.
#'         \item Defined variables contained in your ecocomDP using 
#'         `define_variables`.`
#'     }
#' 
#'     This function uses elements of the parent data package EML to create EML
#'     for the ecocomDP. Since ecocomDP is a fixed format, this function calls
#'     on boiler plate table attributes of the R package inst/ directory and 
#'     imports the metadata of the parent data package. Some new information is
#'     added to the parent data package EML as specified below.
#'     \itemize{
#'         \item \strong{<access>} Appends the ecocomDP creator to the list of 
#'         principals granted read and write access to the ecocomDP data 
#'         package.
#'         \item \strong{<pubDate>} Adds a publication date corresponding to 
#'         when the ecocomDP EML was created.
#'         \item \strong{<keywordSet} Appends two ecocomDP specific keyword 
#'         sets to the keyword sets listed in the parent data package EML.
#'         \item \strong{<intellectualRights>} Keeps intact the intellectual
#'         rights license of the parent data package, or replaces it with one
#'         of the user specified options above.
#'         \item \strong{<taxonomicCoverage>} Updates the taxonomic coverage 
#'         element with data supplied in the taxon table of the ecocomDP.
#'         \item \strong{<contact>} Appends contact information of the ecocomDP
#'         creator to the contacts listed in the parent data package.
#'         \item \strong{<methodStep>} Adds provenance metadata as a method 
#'         step, describing the parent data package from which the ecocomDP 
#'         was created.
#'         \item \strong{<dataTables>} Replaces the parent data package data
#'         tables with the newly created ecocomDP tables. NOTE: All other data
#'         entities are removed from the ecocomDP EML.
#'         \item \strong{<otherEntity>} Describes formatting scripts under the 
#'         other entity element.
#'     }
#'
#' @export
#'
make_eml <- function(data.path, 
                     code.path = data.path, 
                     code.files, 
                     parent.package.id, 
                     sep, 
                     cat.vars = NULL,
                     intellectual.rights = NULL, 
                     child.package.id, 
                     user.id, 
                     affiliation, 
                     access.url = NULL, 
                     additional.contact = NULL,
                     code.file.extension = NULL, 
                     eml.path = NULL){
  
  message(paste('Creating EML for ecocomDP data package', child.package.id))
  
  # Validate and process arguments --------------------------------------------

  if (missing(data.path)){
    stop("Specify path to dataset working directory.")
  }
  
  if (missing(parent.package.id)){
    stop("Specify the parent data package ID.")
  }
  
  if (missing(sep)){
    stop("Specify the field delimiter of the ecocomDP tables.")
  }
  
  if (is.null(cat.vars)){
    warning(paste('Input argument "cat.vars" not found. Use the',
                  'define_variables function to create them.'))
  }
  
  if (missing(child.package.id)){
    stop("Specify the child data package ID.")
  }
  
  if (missing(user.id)){
    stop("Specify a user ID for the data package.")
  }
  if (missing(affiliation)){
    stop("Specify the affiliation associated wiht the user ID.")
  }
  if (!missing(user.id) & !missing(affiliation)){
    if (length(user.id) != length(affiliation)){
      stop(paste('The number of values listed in arguments "user.id" and',
                 '"affiliation" do not match. Each user.id must have a',
                 'corresponding affiliation'))
    }
    if (sum(sum(affiliation == 'LTER'), sum(affiliation == 'EDI')) != length(affiliation)){
      stop(paste('Input argument "affiliation" is not "EDI" or "LTER"! Only',
                 '"EDI" and "LTER" are acceptable values.'))
    }
  }
  
  if (is.null(code.file.extension) & (missing(code.files))){
    stop(paste("Specify the script(s) used to create the ecocomDP tables or the',
               'file extension of the script(s)."))
  }
  
  if (!is.null(additional.contact)){
    use_i <- match(
      colnames(additional.contact),
      c('givenName', 'surName', 'organizationName', 'electronicMailAddress')
    )
    if (sum(is.na(use_i)) > 0){
      stop(paste('The data frame "additional.contact" does not have the',
                 'required columns. Check column presence and spelling.')
           )
    }
  }
  
  # Identify script files. Search directories for files matching the 
  # code.file.extension, or use inputs from the code.files argument.
  
  if (!is.null(code.file.extension)){
    
    match_info <- regexpr(
      "\\.[^\\.]*$", 
      code.file.extension
    )
    
    code.file.extension <- substr(
      code.file.extension,
      start = match_info[1],
      stop = nchar(
        code.file.extension
      )
    ) 
    
    code_files_found <- data.frame(
      files = list.files(
        code.path, 
        pattern = paste0(
          "\\",
          code.file.extension,
          "$"
        ),
        ignore.case=TRUE
      ),
      stringsAsFactors = F
    )
    
    if (length(code_files_found$files) > 1){
      
      print.data.frame(code_files_found)
      message(paste("Which of the above listed code files were used to create",
                    "this ecocomDP?"))
      answer <- readline(paste("Enter respective row numbers. Separate each entry",
                               "with a comma and no space (e.g. 3,12): "))
      code_files <- code_files_found$files[
        as.integer(
          unlist(
            strsplit(
              answer,
              ","
            )
          )
        )
      ]
      message(paste("You selected ...", code_files))
      
    } else if (length(code_files_found$files) == 1){
      
      code_files <- code_files_found$files[1]
      
    }
    
    if (!exists("code_files")){
      stop("No code files were found at the specified code.path. Add files to code.path or revise code.path.")
    }
    
  } else if (!missing(code.files)){
    
    code_files <- EDIutils::validate_file_names(
      code.path, 
      data.files = code.files
    )
    
    if (is.null(code.file.extension)){
      
      code_file_extention <- stringr::str_extract(
        code_files, 
        '\\.[:alpha:]*$'
      )
      
    }

  }
  
  # Parameterize --------------------------------------------------------------
  
  os <- EDIutils::detect_os()
  data_tables_stored <- list()
  
  # Read EML of parent data package -------------------------------------------
  
  message(paste('Reading EML of parent data package', parent.package.id))
  
  pkg_prts <- unlist(
    strsplit(
      parent.package.id, 
      split = ".", 
      fixed = T
    )
  )
  
  scope <- pkg_prts[1]
  identifier <- pkg_prts[2]
  revision <- pkg_prts[3]
  
  read_metadata <- function(parent_eml_file, scope, identifier, revision, 
                            data.path, eml.path = NULL){
    
    if (!is.null(eml.path)){
      eml <- EML::read_eml(
        paste0(
          eml.path,
          "/",
          parent_eml_file
        )
      )
      
    } else {
      
      eml <- try(
        EML::read_eml(
          paste0(
            "https://pasta.lternet.edu/package/metadata/eml",
            "/",
            pkg_prts[1],
            "/",
            identifier,
            "/",
            revision
          )
        )
      )
      
      if (class(eml) == 'try-error'){
        
        eml <- xml2::read_xml(
          paste0(
            "https://pasta.lternet.edu/package/metadata/eml",
            "/",
            pkg_prts[1],
            "/",
            identifier,
            "/",
            revision
          )
        )
        xml2::xml_remove(
          xml2::xml_find_all(
            eml,
            './/dataset/dataTable/constraint'
          )
        )
        xml2::write_xml(
          eml,
          paste0(
            data.path,
            '/metadata.xml'
          )
        )
        eml <- EML::read_eml(
          paste0(
            data.path, 
            '/metadata.xml'
          )
        )
      }
      
      eml
      
    }
    
  }
  
  eml <- suppressWarnings(
    read_metadata(
      parent_eml_file,
      scope,
      identifier,
      revision,
      data.path
    )
  )
  
  # Compile boiler plate attribute metadata for ecocomDP tables ---------------

  attributes_in <- compile_attributes(
    path = data.path,
    delimiter = sep
  )
  
  # Updating nodes ------------------------------------------------------------
  
  message("Updating nodes ...")
  message("<eml>")
  
  # Update <access> -----------------------------------------------------------
  # Update <access> to enable repository upload by ecocomDP 
  # creator(s)/maintainer(s).
  
  message("  <access>")
  
  if (!missing(user.id) & !missing(affiliation)){
    
    for (i in 1:length(user.id)){
      
      if (affiliation[i] == 'LTER'){
        
        eml$access$allow[[length(eml$access$allow) + 1]] <- list(
          principal = paste0(
            'uid=',
            user.id[i],
            ',o=',
            affiliation[i],
            ',dc=ecoinformatics,dc=org'
          ),
          permission = "all"
        )

      } else if (affiliation[i] == 'EDI'){
        
        eml$access$allow[[length(eml$access$allow) + 1]] <- list(
          principal = paste0(
            'uid=',
            user.id[i],
            ',o=',
            affiliation[i],
            ',dc=edirepository,dc=org'
          ),
          permission = "all"
        )
        
      }

    }
    
  }

  # Remove <alternateIdentifier> ----------------------------------------------
  # Remove <alternateIdentifier> to prevent downstream errors

  eml$dataset$alternateIdentifier <- NULL

  # Update <dataset> ----------------------------------------------------------
  
  message("  <dataset>")
  
  # Update <title> ------------------------------------------------------------
  
  message("    <title>")
  
  eml$dataset$title <- paste0(
    eml$dataset$title,
    " (Reformatted to the ecocomDP Design Pattern)"
  )
  
  # Update <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  
  eml$dataset$pubDate <- format(
    Sys.time(),
    "%Y-%m-%d"
  )
  
  # Updating <abstract> -------------------------------------------------------
  
  message("    <abstract>")
  
  src_abstract <- unname(
    unlist(
      stringr::str_remove_all(
        eml$dataset$abstract, 
        "</?para>"
      )
    )
  )

  eml$dataset$abstract <- list()
  eml$dataset$abstract$section <- list()
  eml$dataset$abstract$para <- list()
  
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- 
    "This data package is formatted according to the 'ecocomDP', a data package design pattern for ecological community surveys, and data from studies of composition and biodiversity. For more information on the ecocomDP project see https://github.com/EDIorg/ecocomDP/tree/master, or contact EDI https://environmentaldatainitiative.org."
    
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- 
    paste0(
      'This Level 1 data package was derived from the Level 0 data package found here: ',
      paste0(
        'https://portal.edirepository.org/nis/mapbrowse?scope=',
        scope,
        '&identifier=',
        identifier,
        '&revision=',
        revision
      )
    )
  
  eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- 
    'The abstract below was extracted from the Level 0 data package and is included for context:'

  for (i in 1:length(src_abstract)) {
    eml$dataset$abstract$para[[length(eml$dataset$abstract$para) + 1]] <- 
      src_abstract[i]
  }
  
  # Update <keywordSet> -------------------------------------------------------
  
  message("    <keywordSet>")
  
  keywordSet <- read.table(
    system.file('/controlled_vocabulary.csv', package = 'ecocomDP'),
    header = T,
    sep = ",",
    as.is = T,
    na.strings = "NA"
  )
  
  eml$dataset$keywordSet[[length(eml$dataset$keywordSet) + 1]] <- 
    list(
      keywordThesaurus = keywordSet$keywordThesaurus[1],
      keyword = as.list(keywordSet$keyword)
    )

  # Update <intellectualRights> -----------------------------------------------
  # Don't overwrite existing intellectualRights, as these are the data release
  # terms specified by the source data package author. If source data package 
  # intellectualRights are not specified then add CC0 or CC-BY as specified
  # in the intellectual.rights argument. Default is CC0 if none is supplied.
  
  if (!is.null(eml$dataset$intellectualRights) & 
      (!is.null(intellectual.rights))) {
    
    warning(
      "Source data package intellectual rights license can't be overwritten.", 
      call. = FALSE
    )
    
  } else if (is.null(eml$dataset$intellectualRights) & 
             (!is.null(intellectual.rights))) {
    
    message("    <intellectualRights>")
    
    if (intellectual.rights == "CC0"){
      
      eml$dataset$intellectualRights <- EML::set_TextType(
        system.file(
          'intellectual_rights_cc0_1.txt', 
          package = 'ecocomDP'
        )
      )
      
    } else if (intellectual.rights == "CCBY"){

      eml$dataset$intellectualRights <- EML::set_TextType(
        system.file(
          'intellectual_rights_by_4.0.txt', 
          package = 'ecocomDP'
        )
      )
      
    }
    
  } else if (is.null(eml$dataset$intellectualRights) & 
             (is.null(intellectual.rights))) {
    
    message("    <intellectualRights>")
    
    eml$dataset$intellectualRights <- EML::set_TextType(
      system.file(
        'intellectual_rights_cc0_1.txt', 
        package = 'ecocomDP'
      )
    )

  }
  
  # Update <taxonomicCoverage> ------------------------------------------------
  
  eml$dataset$coverage$taxonomicCoverage <- NULL
  dir_files <- list.files(data.path)
  
  df_table <- read.table(
    paste0(
      data.path,
      "/",
      dir_files[stringr::str_detect(dir_files, "taxon\\b")]
    ),
    header=TRUE,
    sep=sep,
    quote="\"",
    as.is=TRUE,
    comment.char = ""
  )
  
  if (any(!is.na(df_table$authority_taxon_id))) {
    
    message("    <taxonomicCoverage>")
    
    # FIXME: This assumes taxonomyCleanr adoption of authority systems.
    # A solution might reference an authority system URI rather than
    # taxonomyCleanr synonyms. Implement at taxonomyCleanr
    tc <- taxonomyCleanr::make_taxonomicCoverage(
      taxa.clean = df_table$taxon_name,
      authority = df_table$authority_system,
      authority.id = df_table$authority_taxon_id
    )
    
    dataset$coverage$taxonomicCoverage <- tc
    
  }
  
  # Update <contact> ----------------------------------------------------------
  
  if (!is.null(additional.contact)) {
    
    message("    <contact>")
    
    eml$dataset$contact[[length(eml$dataset$contact) + 1]] <- 
      list(
        individualName = list(
          givenName = additional.contact$givenName,
          surName = additional.contact$surName
        ),
        organizationName = additional.contact$organizationName,
        electronicMailAddress = additional.contact$electronicMailAddress
      )

  }

  # Update <methods> ----------------------------------------------------------
  
  message("    <methods>")
  
  # methodStep 1.) Inform user of ecocomDP conversion methods
  ecocomDP_methods <- paste0(
    "The source data package is programmatically converted into an ecocomDP ",
    "data package using the scripts: ", paste(code.files, collapse = ", "),
    ". ", "For more information on the ecocomDP project see ",
    "https://github.com/EDIorg/ecocomDP/tree/master, or contact EDI ",
    "(https://environmentaldatainitiative.org).",
    " Below are the source data methods: "
  )
  
  # methodStep 2.) Inform user of source data methods
  src_methods <- eml$data$methods$methodStep
  
  # methodStep 3.) Inform user of source data location. Creator and contact IDs 
  # are removed to preempt ID clashes. Metadata is written to tempdir() so 
  # EML::read_eml() can apply its unique parsing algorithm.
  provenance <- xml2::read_xml(
    paste0(
      "https://pasta.lternet.edu/package/provenance/eml", "/", pkg_prts[1],
      "/", identifier, "/", revision
    )
  )
  
  xml2::xml_set_attr(
    xml2::xml_find_all(
      provenance,
      './/dataSource/creator'
    ),
    'id', 
    NULL
  )
  
  xml2::xml_set_attr(
    xml2::xml_find_all(
      provenance,
      './/dataSource/contact'
      ),
    'id',
    NULL
  )
  
  xml2::write_xml(
    provenance,
    paste0(
      tempdir(),
      "/provenance.xml"
    )
  )
  
  provenance <- EML::read_eml(
    paste0(
      tempdir(),
      "/provenance.xml"
    )
  )
  
  provenance$`@context` <- NULL
  provenance$`@type` <- NULL
  unlink(paste0(tempdir(), "/provenance.xml"))
  
  # Initialize the methods node and compile the 3 methodSteps
  eml$dataset$methods <- list()
  eml$dataset$methods$methodStep <- list()
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    list(description = list(para = ecocomDP_methods))
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    src_methods
  eml$dataset$methods$methodStep[[length(eml$dataset$methods$methodStep) + 1]] <- 
    provenance
  
  # Update <dataTable> --------------------------------------------------------
  
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
  
  dir_files <- list.files(data.path)
  table_names_found <- list()
  tables_found <- list()
  table_descriptions_found <- list()
  
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
  
  # Loop through each data table
  
  for (i in 1:length(tables_found)){
    
    message("    <dataTable>")
    
    attributes <- attributes_in[[1]][[i]]
    
    df_table <- read.table(
      paste(data.path, "/", tables_found[i], sep = ""),
      header=TRUE,
      sep=sep,
      quote="\"",
      as.is=TRUE,
      comment.char = "")
    
    if (!is.null(cat.vars)){
      use_i <- table_names[i] == cat.vars$tableName
      catvars <- cat.vars[
        table_names[i] == cat.vars$tableName,
        c('attributeName', 'code', 'definition', 'unit')
        ]
    }
    
    if (exists('catvars') & (nrow(catvars) > 0)){
      
      if (dim(catvars)[1] > 0){
        for (j in 1:dim(catvars)[2]){
          catvars[ ,j] <- as.character(catvars[ ,j])
        }
        non_blank_rows <- nrow(catvars) - sum(catvars$attributeName == "")
        catvars <- catvars[1:non_blank_rows, 1:3]
        # Clean extraneous white spaces from catvars tables
        if (dim(catvars)[1] != 0){
          for (j in 1:ncol(catvars)){
            if (class(catvars[ ,j]) == "character" ||
                (class(catvars[ ,j]) == "factor")){
              catvars[ ,j] <- trimws(catvars[ ,j])
            }
          }
        }
      }
      # Clean extraneous white spaces from attributes
      for (j in 1:ncol(attributes)){
        if (class(attributes[ ,j]) == "character" ||
            (class(attributes[ ,j]) == "factor")){
          attributes[ ,j] <- trimws(attributes[ ,j])
        }
      }
      col_classes <- attributes[ ,"columnClasses"]
      attributeList <- suppressWarnings(
        EML::set_attributes(
          attributes[ , c(
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
          factors = catvars,
          col_classes = attributes[ ,"columnClasses"]
        )
      )
      
    } else {
      
      for (j in 1:ncol(attributes)){
        if (class(attributes[ ,j]) == "character" ||
            (class(attributes[ ,j]) == "categorical")){
          attributes[ ,j] <- trimws(attributes[ ,j])
        }
      }
      col_classes <- attributes[ ,"columnClasses"]
      col_classes[col_classes == "factor"] <- "character"
      
      attributeList <- suppressWarnings(
        EML::set_attributes(
          attributes[ , c(
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
          col_classes = attributes[ ,"columnClasses"]
        )
      )
      
    }
    
    # Set physical
    
    physical <- suppressMessages(
      EML::set_physical(
        paste0(data.path, '/', tables_found[i]),
        numHeaderLines = "1",
        recordDelimiter = EDIutils::get_eol(
          path = data.path,
          file.name = tables_found[i],
          os = EDIutils::detect_os()
        ),
        attributeOrientation = "column",
        url = 'placeholder'
      )
    )
    
    if (!is.null(access.url)){
      physical$distribution$online$url[[1]] <- paste0(
        access.url,
        "/",
        tables_found[i][i]
      )
    } else {
      physical$distribution <- list()
    }
    
    # Pull together information for the data table
    
    data_table <- list(
      entityName = table_descriptions[i],
      entityDescription = table_descriptions[i],
      physical = physical,
      attributeList = attributeList,
      numberOfRecords = as.character(dim(df_table)[1])
    )
    
    data_tables_stored[[i]] <- data_table
    
  }
  
  # Compile data tables
  
  eml$dataset$dataTable <- data_tables_stored
  
  # Add <otherEntity> ---------------------------------------------------------
    
  if (length(code_files) > 0) {
    
    list_of_other_entity <- list()
    
    for (i in 1:length(code_files)) {
      
      message("    <otherEntity>")
      
      # Create new other entity element
      
      otherEntity <- list()
      
      # Add entityName
      
      otherEntity$entityName <- code_files[i]
      
      # Add entityDescription
      
      otherEntity$entityDescription <- "A script that converts a parent data package to the ecocomDP (child data package)"
      
      # Add physical
      
      physical <- suppressMessages(
        EML::set_physical(
          paste0(data.path, '/', code_files[i])
        )
      )
      
      physical$dataFormat$textFormat <- NULL
      
      file_extension <- stringr::str_extract(code_files[i], "\\.[:alpha:]*$")
      if (file_extension == ".R"){
        format_name <- "application/R"
        entity_type <- "text/x-rsrc"
      } else if (file_extension == ".m"){
        format_name <- "application/MATLAB"
        entity_type <- "text/x-matlab"
      } else if (file_extension == ".py"){
        format_name <- "application/Python"
        entity_type <- "text/x-python"
      } else {
        format_name <- "unknown"
        entity_type <- "unknown"
      }
      
      physical$dataFormat$externallyDefinedFormat$formatName <- format_name
      
      if (!is.null(access.url)){
        physical$distribution$online$url[[1]] <- paste0(
          access.url,
          "/",
          code_files[i]
        )
      } else {
        physical$distribution <- list()
      }
      
      otherEntity$physical <- physical
      
      # Add entityType
      
      otherEntity$entityType <- entity_type
      
      # Add otherEntity to list
      
      list_of_other_entity[[i]] <- otherEntity
      
    }
    
    eml$dataset$otherEntity <- list_of_other_entity
    
  }

  # Update <eml> --------------------------------------------------------------
  
  eml_out <- list(
    schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
    packageId = child.package.id,
    system = "edi",
    access = eml$access,
    dataset = eml$dataset
  )
  
  message("  </dataset>")
  message("</eml>")
  
  # Write EML -----------------------------------------------------------------

  message("Writing EML")

  EML::write_eml(
    eml_out, 
    paste0(
      data.path,
      "/",
      child.package.id,
      ".xml"
    )
  )

  # Validate EML --------------------------------------------------------------

  message("Validating EML")

  validation_result <- EML::eml_validate(eml_out)

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
# @param delimiter
#     The delimiter of input files. Can be comma (i.e. ",") or tab (i.e. "\\t")
#
# @return 
#     Attributes formatted for make_eml.R
#     
# @export
#
compile_attributes <- function(path, delimiter){
  
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
  
  dir_files <- list.files(path)
  table_names_found <- list()
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
  }
  
  tables_found <- unlist(tables_found)
  table_names <- unlist(table_names_found)
  
  # Loop through each ecocomDP table that is present --------------------------
  
  attributes_stored <- list()
  
  for (i in 1:length(table_names)){
    
    message(table_names[i])
    
    if (delimiter == ","){
      
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
        sep = delimiter,
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
  
  list("attributes" = attributes_stored)
  
}
