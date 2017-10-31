#' Make EML for an ecocomDP
#'
#' @description  
#'     Makes EML for an Ecological Community Data Pattern (ecocomDP).
#'
#' @usage 
#'     make_eml(data.path = "", code.path = "", parent.package.id = "", 
#'     child.package.id = "", sep = "", user.id = "", author.system = "", 
#'     intellectual.rights = "", access.url = "")
#'
#' @param data.path 
#'     A character string specifying the path to the dataset working directory 
#'     containing the validated ecocomDP tables.
#' @param code.path
#'     A character string specifying the path to the directory containing the 
#'     scripts used in processing the L0 data to the L1 data.
#' @param parent.package.id
#'     A character string specifying the identifier of parent data package in the 
#'     EDI data repository (e.g. "knb-lter-hfr.118.28").
#' @param child.package.id
#'     A character string specifying the identifier of child data package in 
#'     the EDI data repository (e.g. "edi.53.1"). If you don't have a 
#'     child.package.id then query EDI for one 
#'     (info@environmentaldatainitiative.org).
#' @param sep
#'     The field separator string. Values within each row of ecocomDP tables
#'     are separated by this string. Valid options are "," or "\\t".
#' @param user.id
#'     A character string specifying the LTER or EDI ID of person publishing 
#'     the ecocomDP data package (e.g. "EDI"). If you don't have a user ID then 
#'     query EDI for one (info@environmentaldatainitiative.org).
#' @param author.system
#'     A character string specifying the author system the user.id is associated 
#'     with (e.g. "edi"). If you don't have an author system specification use 
#'     then query EDI for one (info@environmentaldatainitiative.org).
#' @param intellectual.rights
#'     A character string specifying the intellectual rights license to be used 
#'     with the child ecocomDP data package. Valid arguments are "CCO" 
#'     (https://creativecommons.org/publicdomain/zero/1.0/legalcode),
#'     "CCBY" (https://creativecommons.org/licenses/by/4.0/legalcode), and no 
#'     argument. No input argument indicates the license wil remain the same
#'     as the parent data package.
#' @param access.url
#'     (Not required) A character string specifying the base URL that PASTA uses to upload the 
#'     ecocomDP tables and associated processing scripts. For example, the base 
#'     URL of a table that has a URL of 
#'     https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride/gleon_chloride_concentrations.csv
#'     has a base URL of 
#'     https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride.
#'
#' @return 
#'     An EML metadata file written to the dataset working directory titled 
#'     \emph{packageID.xml}.
#'
#' @details 
#'     Make and validate EML for an Ecological Community Data Pattern 
#'     (ecocomDP) using elements from the parent data package. This function 
#'     does not yet support creation of data packages that are not already 
#'     published in the EDI (Environmental Data Initiative) data repository.
#'     
#'     Run \code{make_eml} after you have completed \emph{addtional_contact.txt},
#'     \emph{custom_units.txt} (if you're ecocomDP has custom units), and have
#'     provided definitions for variables listed in the ecocomDP tables. Use 
#'     \code{define_variables} to identify and define variables.
#' 
#'     This script modifies elements of the parent data package EML:
#'     \itemize{
#'         \item \strong{Access} Modifies access based on user supplied
#'         arguments.
#'         \item \strong{Publicaton date} Changes the publication date to when 
#'         the ecocomDP was created.
#'         \item \strong{Keywords} Adds two keyword sets which include the 
#'         key term "ecocomDP" and a set of keywords relevant to community data
#'         from the LTER controlled vocabulary.
#'         \item \strong{Intellectual rights} Updates the intellectual rights
#'         according to user specified arguments.
#'         \item \strong{Taxon coverage} This has not yet been implemented.
#'         \item \strong{Contact} Adds the ecocomDP creator as a contact.
#'         \item \strong{Provenance} Adds the provenance snippet from the 
#'         parent data package.
#'         \item \strong{Data tables} Adds data table elements for ecocomDP tables
#'         present.
#'         \item \strong{Formatting scripts} Adds formatting scripts used to 
#'         create the ecocomDP as otherEntitie.
#'     }
#'
#' @seealso \code{\link{import_templates}} to import ecocomDP templates.
#' @seealso \code{\link{define_variables}} to identify and define variables 
#'     listed in ecocomDP tables.
#'
#' @export
#'


make_eml <- function(data.path, code.path, parent.package.id, child.package.id, sep, user.id, author.system, intellectual.rights, access.url) {
  
  # Check arguments

  if (missing(data.path)){
    stop("Specify path to dataset working directory.")
  }
  if (missing(code.path)){
    stop("Specify path to the code directory.")
  }
  if (missing(parent.package.id)){
    stop("Specify the parent data package ID.")
  }
  if (missing(child.package.id)){
    stop("Specify the child data package ID.")
  }
  if (missing(sep)){
    stop("Specify the field delimiter of the ecocomDP tables.")
  }
  if (missing(user.id)){
    stop("Specify a user ID for the data package. Default to 'EDI' if unknown")
  }
  if (missing(author.system)){
    stop("Specify an author system for the data package. Default to 'edi' if unknown")
  }
  
  # Parameters ----------------------------------------------------------------

  # Read in metadata
  
  pkg_prts <- unlist(strsplit(parent.package.id, split = ".", fixed = T))
  scope <- pkg_prts[1]
  identifier <- pkg_prts[2]
  revision <- pkg_prts[3]
  
  xml_in <- read_eml(paste("http://pasta.lternet.edu/package/metadata/eml",
                           "/",
                           pkg_prts[1],
                           "/",
                           identifier,
                           "/",
                           revision,
                           sep = ""))

  # Get system information
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  
  # Compile attributes

  attributes_in <- compile_attributes(path = data.path, delimiter = sep)
  
  # Initialize data entity storage (tables)

  data_tables_stored <- list()
  
  # Modify elements -----------------------------------------------------------
  
  # Modify eml-access
  
  print("Modifying <access> ...")

  allow_principals <- c(paste("uid=",
                              user.id,
                              ",o=LTER,dc=ecoinformatics,dc=org",
                              sep = ""),
                        "public")

  allow_permissions <- c("all",
                         "read")

  access_order <- "allowFirst"

  access_scope <- "document"

  access <- new("access",
                scope = access_scope,
                order = access_order,
                authSystem = author.system)

  allow <- list()
  for (i in 1:length(allow_principals)){
    allow[[i]] <- new("allow",
                      principal = allow_principals[i],
                      permission = allow_permissions[i])
  }

  access@allow <- new("ListOfallow",
                      c(allow))
  
  xml_in@access <- access

  # Modify eml-contact
  
  print("Appending <contact> ...")
  
  personinfo <- read.table(paste(data.path,
                                 "/additional_contact.txt",
                                 sep = ""),
                           header = T,
                           sep = "\t",
                           as.is = T,
                           na.strings = "NA")
  
  individualName <- new(
    "individualName",
    givenName = trimws(personinfo["givenName"]),
    surName = trimws(personinfo["surName"]))
  
  contact <- new(
    "contact",
    individualName = individualName,
    organizationName = trimws(personinfo[["organizationName"]]),
    electronicMailAddress = trimws(personinfo[["electronicMailAddress"]]))
  
  xml_in@dataset@contact[[length(xml_in@dataset@contact)+1]] <- contact

  # Modify eml-pubDate

  print("Updating <pubDate> ...")

  xml_in@dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")

  # Modify keywordSet
  
  print("Appending <keywordSet> ...")
  
  keywords <- read.table(paste(path.package("ecocomDP"),
                               "/controlled_vocabulary.csv",
                               sep = ""),
                         sep = ",",
                         header = T,
                         as.is = T)
  
  list_keywordSet <- xml_in@dataset@keywordSet
  
  list_keywordSet[[length(list_keywordSet)+1]] <- new("keywordSet",
                                                      keyword = "ecocomDP")
  
  lter_keywordSet <- list()
  use_i <- keywords[["keywordThesaurus"]] == "LTER Controlled Vocabulary"
  keywords <- keywords[use_i, "keyword"]
  for (i in 1:length(keywords)){
    lter_keywordSet[[i]] <- as(keywords[i], "keyword")
  }
  
  list_keywordSet[[length(list_keywordSet)+1]] <- new("keywordSet",
                                                      lter_keywordSet,
                                                      keywordThesaurus = "LTER Controlled Vocabulary")
  xml_in@dataset@keywordSet <- list_keywordSet
  
  # Modify license
  
  if (exists("intellectual.rights")){
    if (intellectual.rights == "CC0"){
      print("Changing <intellectualRights> to CC0 ...")
      xml_in@dataset@intellectualRights <- as(
        set_TextType(paste(path.package("ecocomDP"),
                           "/intellectual_rights_cc0_1.txt",
                           sep = "")),
        "intellectualRights")
    } else if (intellectual.rights == "CCBY"){
      print("Changing <intellectualRights> to CCBY ...")
      xml_in@dataset@intellectualRights <- as(
        set_TextType(paste(path.package("ecocomDP"),
                           "/intellectual_rights_by_4.0.txt",
                           sep = "")),
        "intellectualRights")
    }
  }
  
  # Modify methods
  
  print("Adding provenance ...")
  
  provenance <- read_eml(paste("https://pasta.lternet.edu/package/provenance/eml",
                               "/",
                               pkg_prts[1],
                               "/",
                               identifier,
                               "/",
                               revision,
                               sep = ""))
  
  methods_step <- xml_in@dataset@methods@methodStep
  
  methods_step[[length(methods_step)+1]] <- provenance
  
  xml_in@dataset@methods@methodStep <- methods_step

  
  # Loop through tables -------------------------------------------------------

  table_patterns <- c("observation\\b", "event\\b", "sampling_location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b", "location\\b", "taxon\\b")
  table_names <- c("observation", "event", "sampling_location_ancillary", "taxon_ancillary", "dataset_summary", "location", "taxon")
  dir_files <- list.files(data.path)
  table_names_found <- list()
  tables_found <- list()
  for (i in 1:length(table_patterns)){
    tables_found[[i]] <- dir_files[grep(paste("^(?=.*", table_patterns[i], ")(?!.*variables)", sep = ""), dir_files, perl=TRUE)]
    if (!identical(tables_found[[i]], character(0))){
      table_names_found[[i]] <- table_names[i]
    }
  }
  tables_found <- unlist(tables_found)
  table_names <- unlist(table_names_found)
  
  for (i in 1:length(tables_found)){

    print(paste(
      table_names[i],
      "data table ..."))

    attributes <- attributes_in[[1]][[i]]

    # Read data table

    df_table <- read.table(
      paste(data.path, "/", tables_found[i], sep = ""),
      header=TRUE,
      sep=sep,
      quote="\"",
      as.is=TRUE,
      comment.char = "")

    # Read catvars file

    catvar <- grep(paste(table_names[i], "_variables.txt", sep = ""), dir_files, value = T)
    
    if (!identical(catvar, character(0))){

      catvars <- read.table(
        paste(
          data.path,
          "/",
          catvar, sep = ""),
        header=TRUE,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "")

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

      # Get the column classes into a vector

      col_classes <- attributes[ ,"columnClasses"]

      # Create the attributeList element

      attributeList <- set_attributes(attributes,
                                      factors = catvars,
                                      col_classes = col_classes)

    } else {

      # Clean extraneous white spaces from attributes

      for (j in 1:ncol(attributes)){
        if (class(attributes[ ,j]) == "character" ||
            (class(attributes[ ,j]) == "categorical")){
          attributes[ ,j] <- trimws(attributes[ ,j])
        }
      }

      # Get the column classes into a vector

      col_classes <- attributes[ ,"columnClasses"]

      # Create the attributeList element

      attributeList <- set_attributes(attributes,
                                      col_classes = col_classes)

    }

    # Set physical

    if (sep == "\t"){
      physical <- set_physical(tables_found[i],
                               numHeaderLines = "1",
                               recordDelimiter = "\\r\\n",
                               attributeOrientation = "column",
                               fieldDelimiter = "\\t",
                               quoteCharacter = "\"")
    } else if (sep == ","){
      physical <- set_physical(tables_found[i],
                               numHeaderLines = "1",
                               recordDelimiter = "\\r\\n",
                               attributeOrientation = "column",
                               fieldDelimiter = ",",
                               quoteCharacter = "\"")
    }
    




    physical@size <- new("size",
                         unit = "byte",
                         as.character(
                           file.size(
                             paste(data.path,
                                   "/",
                                   tables_found[i],
                                   sep = ""))))
    
    if (exists(access.url)){
      
      data_table_urls <- paste(access.url, "/", tables_found[i], sep = "") 
      
      distribution <- new("distribution",
                          online = new("online",
                                       url = data_table_urls))
      
      physical@distribution <- new("ListOfdistribution",
                                   c(distribution))
    
      }

    if (os == "mac"){

      command_certutil <- paste("md5 ",
                                data.path,
                                "/",
                                tables_found[i],
                                sep = "")

      certutil_output <- system(command_certutil, intern = T)

      checksum_md5 <- gsub(".*= ", "", certutil_output)

      authentication <- new("authentication",
                            method = "MD5",
                            checksum_md5)

      physical@authentication <- as(list(authentication),
                                    "ListOfauthentication")

    } else if (os == "win"){

      command_certutil <- paste("CertUtil -hashfile ",
                                data.path,
                                "\\",
                                tables_found[i],
                                " MD5",
                                sep = "")

      certutil_output <- system(command_certutil, intern = T)

      checksum_md5 <- gsub(" ", "", certutil_output[2])

      authentication <- new("authentication",
                            method = "MD5",
                            checksum_md5)

      physical@authentication <- as(list(authentication),
                                    "ListOfauthentication")

    }

    # Get number of records

    number_of_records <- as.character(dim(df_table)[1])

    # Pull together information for the data table

    data_table <- new("dataTable",
                      entityName = tables_found[i],
                      entityDescription = substr(tables_found[i], 1, nchar(tables_found[i])-4),
                      physical = physical,
                      attributeList = attributeList,
                      numberOfRecords = number_of_records)

    data_tables_stored[[i]] <- data_table

  }


  # Compile datatables --------------------------------------------------------

  # Are custom units present in these tables?

  if (file.exists(paste(data.path,"/","custom_units.txt",sep = ""))){

    custom_units_df <- read.table(
      paste(data.path,
            "/",
            "custom_units.txt",
            sep = ""),
      header = TRUE,
      sep = "\t",
      as.is = TRUE,
      na.strings = "")

    if (nrow(custom_units_df) < 1){
      custom_units <- "no"
    } else {
      custom_units <- "yes"
      unitsList <- set_unitList(custom_units_df)
    }

    # # Clean white spaces from custom_units and units_types
    # 
    # if (custom_units == "yes"){
    # 
    #   for (j in 1:ncol(custom_units_df)){
    #     if (class(custom_units_df[ ,j]) == "character" ||
    #         (class(custom_units_df[ ,j]) == "factor")){
    #       custom_units_df[ ,j] <- trimws(custom_units_df[ ,j])
    #     }
    #   }
    # 
    #   unitsList <- set_unitList(custom_units_df)
    # }


  } else {

    custom_units <- "no"

  }

  # Compile data tables

  xml_in@dataset@dataTable <- new("ListOfdataTable",
                                  data_tables_stored)
  
  # Build EML
  
  # print("Updating packageId ...")
  # 
  # xml_in@packageId@.Data <- child.package.id
  
  if (custom_units == "yes"){
    
    print("Adding <customUnits>")
    
    xml_in@additionalMetadata <- as(unitsList, "additionalMetadata")
    
  }
  
  # Add R code ------------------------------------------------------------------
  # This section needs to accomodate other code types.
  
  print("Adding formatting scripts as <otherEntity> ...")
  
  # Name of script files
  
  match_info <- regexpr("[^\\]*$", code.path)
  directory_name <- substr(code.path, start = match_info[1], stop = nchar(code.path))
  
  files <- list.files(path = code.path)
  
  code_names <- files[attr(regexpr("*.R$", files), "match.length") != -1]
  
  list_of_other_entity <- list()
  
  if (!identical(code_names, character(0))){
    
    for (i in 1:length(code_names)){
      
      # Create new other entity element
      
      other_entity <- new("otherEntity")
      
      # Add code file names
      
      other_entity@entityName <- code_names[i]
      
      # Add description
      
      code_description <- "Converts parent data package to ecocomDP (child data package)"
      
      other_entity@entityDescription <- code_description
      
      #  Build physical
      
      num_header_lines_code <- trimws(c("0"))
      record_delimeter_code <- trimws(c("\\r\\n"))
      attribute_orientation_code <- trimws(c("column"))
      field_delimeter_code <- ","
      quote_character <- trimws(c("\""))
      code_urls <- paste(access.url, "/", code_names[i], sep = "")
      entity_type_code <- "R code"
      
      physical <- set_physical(code_names[i],
                               numHeaderLines = num_header_lines_code,
                               recordDelimiter = record_delimeter_code,
                               attributeOrientation = attribute_orientation_code,
                               fieldDelimiter = field_delimeter_code,
                               quoteCharacter = quote_character)
      
      physical@size <- new("size", unit = "bytes", as(as.character(file.size(paste(code.path, "\\", code_names[i], sep = ""))), "size"))
      
      distribution <- new("distribution",
                          online = new("online",
                                       url = code_urls))
      
      physical@distribution <- new("ListOfdistribution",
                                   c(distribution))
      
      if (os == "mac"){
        
        command_certutil <- paste("md5 ",
                                  code.path,
                                  "/",
                                  code_names[i],
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(".*= ", "", certutil_output)
        
        authentication <- new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- as(list(authentication),
                                      "ListOfauthentication")
        
      } else if (os == "win"){
        
        command_certutil <- paste("CertUtil -hashfile ",
                                  code.path,
                                  "\\",
                                  code_names[i],
                                  " MD5",
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(" ", "", certutil_output[2])
        
        authentication <- new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- as(list(authentication),
                                      "ListOfauthentication")
        
      }
      
      other_entity@physical <- as(c(physical), "ListOfphysical")
      
      # Add entity type
      
      other_entity@entityType <- entity_type_code
      
      # Add other entity to list
      
      list_of_other_entity[[i]] <- other_entity
      
    }
    
    print("Adding formatting scripts as <otherEntity> ...")
    
    xml_in@dataset@otherEntity <- new("ListOfotherEntity",
                                      list_of_other_entity)
    
  }
  
  
  
  # Recompile EML
  
  if (custom_units == "yes"){
    eml <- new("eml",
               schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
               packageId = child.package.id,
               system = author.system,
               access = xml_in@access,
               dataset = xml_in@dataset,
               additionalMetadata = xml_in@additionalMetadata)
    
  } else {
    eml <- new("eml",
               schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
               packageId = child.package.id,
               system = author.system,
               access = xml_in@access,
               dataset = xml_in@dataset)
  }
  
  # Write EML

  print("Writing to file ...")

  write_eml(eml, paste(data.path, "/", child.package.id, ".xml", sep = ""))

  # Validate EML

  print("Validating EML ...")

  validation_result <- eml_validate(eml)

  if (validation_result == "TRUE"){

    print("EML passed validation!")

  } else {

    print("EML validaton failed. See warnings for details.")

  }

  
}


