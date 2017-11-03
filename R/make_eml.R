#' Make EML for an ecocomDP
#'
#' @description  
#'     Makes EML for an Ecological Community Data Pattern (ecocomDP).
#'
#' @usage 
#'     make_eml(data.path = "", code.path = "", eml.path = "", 
#'     parent.package.id = "", child.package.id = "", sep = "", user.id = "", 
#'     author.system = "", intellectual.rights = "", access.url = "",
#'     datetime.format = "", code.file.extension = "")
#'
#' @param data.path 
#'     A character string specifying the path to the dataset working directory 
#'     containing the validated ecocomDP tables.
#' @param code.path
#'     A character string specifying the path to the directory containing the 
#'     scripts used in processing the L0 data to the L1 data.
#' @param eml.path
#'     A character string specifying the path to the directory containing the
#'     EML file of the parent data package. This argument is required if the EML is 
#'     not located in the EDI data repository.
#' @param parent.package.id
#'     A character string specifying the identifier of parent data package 
#'     stored in the EDI data repository (e.g. "knb-lter-hfr.118.28"), or a 
#'     character string specifying the file name of the parent data package EML
#'     stored at eml.path. If the latter, do not include a file extension. The 
#'     identifier will be used to read in the respective EML file from the EDI
#'     data repository or from the eml.path.
#' @param child.package.id
#'     A character string specifying the identifier of child data package to be
#'     uploaded to the EDI data repository (e.g. "edi.53.1"). If you don't have a 
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
#' @param datetime.format 
#'     A character string specifying the date time format 
#'     string used throughout your ecocomDP tables. 
#'     Valid date time formats are a combination of date, time, and time 
#'     zone strings.
#'         \itemize{
#'             \item \strong{Date format strings:} YYYY-MM-DD; where YYYY is year, MM is 
#'             month, DD is day of month.
#'             \item \strong{Time format strings:} hh:mm:ss.sss, hhmmss.sss,
#'             hh:mm:ss, hhmmss, hh:mm, hhmm, hh; where hh is hour (in 24 hr
#'             clock), mm is minute, ss is second, and ss.sss is decimal 
#'             second.
#'             \item\strong{Time zone format strings:} Z, +hh:mm, +hhmm, +hh,
#'             -hh:mm, -hhmm, -hh; where Z (capitalized) is Coordinated 
#'             Universal Time, and + and - denote times ahead and behind UTC
#'             respectively.
#'         }
#'    If reporting a date without time, use the date format 
#'    string. If reporting a date and time, select the date and one time 
#'    format string and combine with a single space (e.g. 
#'    YYYY-MM-DD hh:mm) or with a "T" (e.g. YYYY-MM-DDThh:mm). If 
#'    reporting a date and time, it is recommended that a time zone 
#'    specifier be appended without a space (e.g. YYYY-MM-DD hh:mm-hh:mm, 
#'    or YYYY-MM-DDThh:mm-hh:mm).
#' @param code.file.extension
#'    A character string specifying the extension of the processing script(s) 
#'    used to create this ecocomDP. For example, enter ".R" if the processing script(s)
#'    are written in the R, or enter ".py" if the processing script(s) are
#'    written in Python.
#'
#' @return 
#'     An EML metadata file written to the dataset working directory titled 
#'     \emph{packageID.xml}.
#'
#' @details 
#'     Make and validate EML for an Ecological Community Data Pattern 
#'     (ecocomDP) using elements from the parent data package and additional 
#'     user supplied metadata.
#'     
#'     Run \code{make_eml} after you have:
#'     \itemize{
#'         \item Validated your ecocomDP with \code{validate_ecocomDP}.
#'         \item Imported and completed template files to provide additional 
#'         metadata to the ecocomDP EML. Run \code{import_templates} to import
#'         template files, \emph{addtional_contact.txt} (adds you as an 
#'         additional contact for this ecocomDP), \emph{custom_units.txt} 
#'         (adds any custom units you need to report for your ecocomDP), 
#'         \emph{provenance_metadata.txt} (adds provenance metadata to your 
#'         ecocomDP if the parent data package does not originate from the EDI
#'         data repository).
#'         \item Defined variables contained in your ecocomDP using 
#'         \code{define_variables}.
#'     }
#' 
#'     This function uses elements of the parent data package EML to create EML
#'     for the ecocomDP. Since ecocomDP is a fixed format, this function calls
#'     on boiler plate table attributes of the R package inst/ directory and 
#'     imports the metadata of the parent data package. Some new information is
#'     added to the parent data package EML as specified below.
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
#'         parent data package with general description of how the ecocomDP was
#'         created.
#'         \item \strong{Data tables} Adds data table elements for ecocomDP tables
#'         present.
#'         \item \strong{Formatting scripts} Adds formatting scripts used to 
#'         create the ecocomDP as otherEntity.
#'     }
#'
#' @seealso \code{\link{validate_ecocomDP}} to validate your ecocomDP.
#' @seealso \code{\link{import_templates}} to import ecocomDP templates.
#' @seealso \code{\link{define_variables}} to identify and define variables 
#'     listed in ecocomDP tables.
#'
#' @export
#'


make_eml <- function(data.path, code.path, eml.path, parent.package.id, child.package.id, sep, user.id, author.system, intellectual.rights, access.url, datetime.format, code.file.extension) {
  
  # Check arguments and input requirements ------------------------------------

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
  # if (missing(intellectual.rights)){
  #   int.rts <- "no change"
  # }
  if (missing(datetime.format)){
    stop("Specify a datetime format used throughout tables of this ecocomDP.")
  }
  if (missing(code.file.extension)){
    stop("Specify the code file extension.")
  }
  
  # Specify which code files were used (if more than one exists)
  
  code_files_found <- data.frame(files = list.files(code.path, 
                                                    pattern = paste("\\",
                                                                    code.file.extension,
                                                                    "$",
                                                                    sep = "")),
                                 stringsAsFactors = F)
  if (length(code_files_found$files) > 1){
    print.data.frame(code_files_found)
    print("Which of the above listed code files were used to create this ecocomDP?")
    answer <- readline("Enter respective row numbers. Separate each entry with a comma and no space (e.g. 3,12): ")
    code_files <- code_files_found$files[as.integer(unlist(strsplit(answer, ",")))]
    print(paste("You selected ...", code_files))
  } else if (length(code_files_found$files) == 1){
    code_files <- code_files_found$files[1]
  }
  if (!exists("code_files")){
    stop("No code files were found at the specified code.path. Add files to code.path or revise code.path.")
  }
  
  # Specify which .xml file is the parent EML (if more than one .xml file exists)
  
  if (!missing(eml.path)){
    xml_files_found <- data.frame(files = list.files(eml.path, pattern = "\\.xml$"),
                                  stringsAsFactors = F)
    if (length(xml_files_found$files) > 1){
      print.data.frame(xml_files_found)
      print("Which of the above listed .xml files are the parent EML of this ecocomDP?")
      answer <- readline("Enter the row number: ")
      parent_eml_file <- xml_files_found$files[as.integer(unlist(strsplit(answer, ",")))]
      print(paste("You selected ...", parent_eml_file))
    } else {
      if (length(list.files(eml.path, pattern = paste(parent.package.id, ".xml", sep = ""))) == 1){
        parent_eml_file <- paste(parent.package.id, ".xml", sep = "")
      }
    }
    if (!exists("parent_eml_file")){
      stop("No .xml files were found at the specified eml.path. Add .xml files to eml.path or revise eml.path. Alternatively you may have entered parent.package.id incorrectly.")
    }
  }
  
  # Deconstruct parent package ID
  
  pkg_prts <- unlist(strsplit(parent.package.id, split = ".", fixed = T))
  scope <- pkg_prts[1]
  identifier <- pkg_prts[2]
  revision <- pkg_prts[3]
    
  # Parameterize --------------------------------------------------------------

  # Read in metadata
  
  if (!missing(eml.path)){
    
    xml_in <- read_eml(paste(eml.path,
                             "/",
                             parent_eml_file,
                             sep = ""))
  } else {

    xml_in <- read_eml(paste("http://pasta.lternet.edu/package/metadata/eml",
                             "/",
                             pkg_prts[1],
                             "/",
                             identifier,
                             "/",
                             revision,
                             sep = ""))
  }

  # Get system information
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  
  # Compile attributes and add datetime.format string

  attributes_in <- compile_attributes(path = data.path, delimiter = sep)
  
  for (i in 1:length(attributes_in[[1]])){
    use_i <- attributes_in[[1]][[i]]$columnClasses == "Date"
    attributes_in[[1]][[i]]$formatString[use_i] <- datetime.format
  }
  
  # Initialize data entity storage (tables)

  data_tables_stored <- list()
  
  # Modify elements of parent EML ---------------------------------------------
  
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
  
  if (!missing(intellectual.rights)){
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
  
  # Add provenance metadata
  
  additional_provenance_file <- paste(data.path,
                                      "/",
                                      "additional_provenance.txt",
                                      sep = "")
  
  if (file.exists(additional_provenance_file)){

    # Check for file content

    additional_provenance <- read.table(additional_provenance_file,
                                        header=TRUE,
                                        sep = "\t",
                                        quote="\"",
                                        as.is=TRUE,
                                        comment.char = "")
    ncol_expected <- ncol(additional_provenance)
    
    ncol_found <- ncol(as.data.frame(additional_provenance[,colSums(is.na(additional_provenance))<nrow(additional_provenance)]))

    if (ncol_found == ncol_expected){

      print("Adding provenance from additional_provenance.txt ...")

      # provenance <- read_eml(additional_provenance_file)
      # 
      # methods_step <- xml_in@dataset@methods@methodStep
      # 
      # methods_step[[length(methods_step)+1]] <- provenance
      # 
      # xml_in@dataset@methods@methodStep <- methods_step

    } else {

      # Import provenance from PASTA (identical to below)
      
      provenance <- read_eml(paste("https://pasta.lternet.edu/package/provenance/eml",
                                   "/",
                                   pkg_prts[1],
                                   "/",
                                   identifier,
                                   "/",
                                   revision,
                                   sep = ""))
      
      additional_metadata <- read.table(paste(path.package("ecocomDP"),
                                              "/additional_provenance.txt",
                                              sep = ""),
                                        header = T,
                                        sep = "\t",
                                        as.is = T,
                                        na.strings = "NA")
      
      provenance@description <- as(
        set_TextType(text = additional_provenance$methodDescription),
        "description")

      methods_step <- xml_in@dataset@methods@methodStep
      
      methods_step[[length(methods_step)+1]] <- provenance
      
      xml_in@dataset@methods@methodStep <- methods_step

    }

  } else {

    # Import provenance from PASTA (identical to above)

    provenance <- read_eml(paste("https://pasta.lternet.edu/package/provenance/eml",
                                 "/",
                                 pkg_prts[1],
                                 "/",
                                 identifier,
                                 "/",
                                 revision,
                                 sep = ""))
    
    additional_metadata <- read.table(paste(path.package("ecocomDP"),
                                            "/additional_provenance.txt",
                                            sep = ""),
                                      header = T,
                                      sep = "\t",
                                      as.is = T,
                                      na.strings = "NA")
    
    provenance@description <- as(
      set_TextType(text = additional_provenance$methodDescription),
      "description")
    
    methods_step <- xml_in@dataset@methods@methodStep
    
    methods_step[[length(methods_step)+1]] <- provenance
    
    xml_in@dataset@methods@methodStep <- methods_step

  }

  # Make EML for ecocomDP tables  ---------------------------------------------

  table_patterns <- c("observation\\b", "observation_ancillary\\b", "location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b", "location\\b", "taxon\\b")
  table_names <- c("observation", "observation_ancillary", "location_ancillary", "taxon_ancillary", "dataset_summary", "location", "taxon")
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
    
    if (exists("access.url")){
      
      data_table_urls <- paste(access.url, "/", tables_found[i], sep = "") 
      
      distribution <- new("distribution",
                          online = new("online",
                                       url = data_table_urls))
      
      physical@distribution <- new("ListOfdistribution",
                                   c(distribution))
    
    } else {
      
      distribution <- new("distribution",
                          offline = new("offline",
                                       mediumName = "Hard drive"))
      
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
  
  # Add processing scripts ----------------------------------------------------
  
  print("Adding processing scripts as <otherEntity> ...")
  
  list_of_other_entity <- list()
  
  if (!identical(code_files, character(0))){
    
    for (i in 1:length(code_files)){
      
      # Create new other entity element
      
      other_entity <- new("otherEntity")
      
      # Add code file names
      
      other_entity@entityName <- code_files[i]
      
      # Add description
      
      code_description <- "A script that converts parent data package to ecocomDP (child data package)"
      
      other_entity@entityDescription <- code_description
      
      #  Build physical
      
      num_header_lines_code <- trimws(c("0"))
      record_delimeter_code <- trimws(c("\\r\\n"))
      attribute_orientation_code <- trimws(c("column"))
      field_delimeter_code <- ","
      quote_character <- trimws(c("\""))
      entity_type_code <- "Processing script"
      
      physical <- set_physical(code_files[i],
                               numHeaderLines = num_header_lines_code,
                               recordDelimiter = record_delimeter_code,
                               attributeOrientation = attribute_orientation_code,
                               fieldDelimiter = field_delimeter_code,
                               quoteCharacter = quote_character)
      
      physical@size <- new("size", unit = "bytes", as(as.character(file.size(paste(code.path, "/", code_files[i], sep = ""))), "size"))
      
      if (exists("access.url")){
        code_urls <- paste(access.url, "/", code_files[i], sep = "") 
        distribution <- new("distribution",
                            online = new("online",
                                         url = code_urls))
        physical@distribution <- new("ListOfdistribution",
                                     c(distribution))
      } else {
        distribution <- new("distribution",
                            offline = new("offline",
                                          mediumName = "Hard drive"))
        physical@distribution <- new("ListOfdistribution",
                                     c(distribution))
      }
      
      if (os == "mac"){
        
        command_certutil <- paste("md5 ",
                                  code.path,
                                  "/",
                                  code_files[i],
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
                                  code_files[i],
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

  print("Writing EML to file ...")

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


