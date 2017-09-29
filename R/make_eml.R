#' Make EML (WARNING: this function is not yet functional)
#'
#' @description  
#'     Make EML for an Ecological Community Data Pattern (ecocomDP) using 
#'     elements from the parent data package.
#'
#' @usage 
#'     make_eml(path, parent.package.id, child.package.id)
#'
#' @param path 
#'     A path to the dataset working directory containing the validated 
#'     ecocomDP tables.
#'     
#' @param parent.package.id
#'     Identifier of parent data package (e.g. knb-lter-hfr.118.28).
#'     
#' @param child.package.id
#'     Identifier of child data package (e.g. edi.53.1).
#'
#' @return 
#'     EML for the ecocomDP tables to the input path.
#'     
#' @details 
#'     Details.
#'
#' @export
#'


make_eml <- function(path, parent.package.id, child.package.id, delimiter, user.id, author.system, intellectual.rights) {
  
  # Check arguments

  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  if (missing(parent.package.id)){
    stop("Specify the parent data package ID.")
  }
  if (missing(child.package.id)){
    stop("Specify the child data package ID.")
  }
  if (missing(delimiter)){
    stop("Specify the delimiter of child data tables.")
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

  # Compile attributes

  attributes_in <- compile_attributes(path = path, delimiter = delimiter)
  
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

  # Modify eml-creators

  print("Modifying <creator> ...")
  
  personinfo <- read.table(paste(path,
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
  
  creator <- new(
    "creator",
    individualName = individualName,
    organizationName = trimws(personinfo[["organizationName"]]),
    electronicMailAddress = trimws(personinfo[["electronicMailAddress"]]))

  xml_in@dataset@creator[[length(xml_in@dataset@creator)+1]] <- creator
  
  # Modify eml-contact
  
  print("Modifying <contact> ...")
  
  individualName <- new(
    "individualName",
    givenName = trimws(personinfo["givenName"]),
    surName = trimws(personinfo["surName"]))
  
  contact <- new(
    "creator",
    individualName = individualName,
    organizationName = trimws(personinfo[["organizationName"]]),
    electronicMailAddress = trimws(personinfo[["electronicMailAddress"]]))
  
  xml_in@dataset@contact[[length(xml_in@dataset@contact)+1]] <- contact

  # Modify eml-pubDate

  print("Modifying <pubDate> ...")

  xml_in@dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")

  # Modify keywordSet
  
  print("Modifying <keywordSet> ...")
  
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
  
  if (intellectual.rights == "CC0"){
    
    print("Modifying <intellectualRights> to CC0 ...")
    
    xml_in@dataset@intellectualRights <- as(
      set_TextType(paste(path.package("ecocomDP"),
                         "/intellectual_rights_cc0_1.txt",
                         sep = "")),
      "intellectualRights")
     
  } else if (intellectual.rights == "CCBY"){
    
    print("Modifying <intellectualRights> to CCBY ...")
    
    xml_in@dataset@intellectualRights <- as(
      set_TextType(paste(path.package("ecocomDP"),
                         "/intellectual_rights_by_4.0.txt",
                         sep = "")),
      "intellectualRights")
    
  }
  
  
  # # Loop through tables -------------------------------------------------------
  # 
  # for (i in 1:length(table_names)){
  #   
  #   print(paste(
  #     table_names[i],
  #     "data table ..."))
  #   
  #   attributes <- attributes_in[[1]][[i]]
  #   
  #   # Read data table
  #   
  #   if (field_delimeter[i] == "comma"){
  #     
  #     df_table <- read.table(
  #       paste(path, "/", table_names[i], sep = ""),
  #       header=TRUE,
  #       sep=",",
  #       quote="\"",
  #       as.is=TRUE,
  #       comment.char = "")
  #     
  #   } else if (field_delimeter[i] == "tab"){
  #     
  #     df_table <- read.table(
  #       paste(path, "/", table_names[i], sep = ""),
  #       header=TRUE,
  #       sep="\t",
  #       quote="\"",
  #       as.is=TRUE,
  #       comment.char = "")
  #     
  #   }
  #   
  #   # Read catvars file
  #   
  #   if (!is.na(match(fname_table_catvars[i], list.files(path)))){
  #     
  #     catvars <- read.table(
  #       paste(
  #         path,
  #         "/",
  #         fname_table_catvars[i], sep = ""),
  #       header=TRUE,
  #       sep="\t",
  #       quote="\"",
  #       as.is=TRUE,
  #       comment.char = "")
  #     
  #     if (dim(catvars)[1] > 0){
  #       
  #       for (j in 1:dim(catvars)[2]){
  #         catvars[ ,j] <- as.character(catvars[ ,j])
  #       }
  #       
  #       non_blank_rows <- nrow(catvars) - sum(catvars$attributeName == "")
  #       catvars <- catvars[1:non_blank_rows, 1:3]
  #       
  #       # Clean extraneous white spaces from catvars tables
  #       
  #       if (dim(catvars)[1] != 0){
  #         for (j in 1:ncol(catvars)){
  #           if (class(catvars[ ,j]) == "character" ||
  #               (class(catvars[ ,j]) == "factor")){
  #             catvars[ ,j] <- trimws(catvars[ ,j])
  #           }
  #         }
  #       }
  #       
  #     }
  #     
  #     # Clean extraneous white spaces from attributes
  #     
  #     for (j in 1:ncol(attributes)){
  #       if (class(attributes[ ,j]) == "character" ||
  #           (class(attributes[ ,j]) == "factor")){
  #         attributes[ ,j] <- trimws(attributes[ ,j])
  #       }
  #     }
  #     
  #     # Get the column classes into a vector
  #     
  #     col_classes <- attributes[ ,"columnClasses"]
  #     
  #     # Create the attributeList element
  #     
  #     attributeList <- set_attributes(attributes,
  #                                     factors = catvars,
  #                                     col_classes = col_classes)
  #     
  #   } else {
  #     
  #     # Clean extraneous white spaces from attributes
  #     
  #     for (j in 1:ncol(attributes)){
  #       if (class(attributes[ ,j]) == "character" ||
  #           (class(attributes[ ,j]) == "categorical")){
  #         attributes[ ,j] <- trimws(attributes[ ,j])
  #       }
  #     }
  #     
  #     # Get the column classes into a vector
  #     
  #     col_classes <- attributes[ ,"columnClasses"]
  #     
  #     # Create the attributeList element
  #     
  #     attributeList <- set_attributes(attributes,
  #                                     col_classes = col_classes)
  #     
  #   }
  #   
  #   # Set physical
  #   
  #   if (field_delimeter[i] == "comma"){
  #     fd <- ","
  #   } else if (field_delimeter[i] == "tab"){
  #     fd <- "\t"
  #   }
  #   
  #   if (nchar(quote_character[i]) > 0){
  #     
  #     physical <- set_physical(table_names[i],
  #                              numHeaderLines = num_header_lines[i],
  #                              recordDelimiter = record_delimeter[i],
  #                              attributeOrientation = attribute_orientation[i],
  #                              fieldDelimiter = fd,
  #                              quoteCharacter = quote_character[i])
  #     
  #   } else {
  #     
  #     physical <- set_physical(table_names[i],
  #                              numHeaderLines = num_header_lines[i],
  #                              recordDelimiter = record_delimeter[i],
  #                              attributeOrientation = attribute_orientation[i],
  #                              fieldDelimiter = fd)
  #     
  #   }
  #   
  #   
  #   
  #   physical@size <- new("size",
  #                        unit = "byte",
  #                        as.character(
  #                          file.size(
  #                            paste(path,
  #                                  "/",
  #                                  table_names[i],
  #                                  sep = ""))))
  #   
  #   if (nchar(data_table_urls[i]) > 1){
  #     distribution <- new("distribution",
  #                         online = new("online",
  #                                      url = data_table_urls[i]))
  #   } else {
  #     distribution <- new("distribution",
  #                         offline = new("offline",
  #                                       mediumName = storage_type[i]))
  #   }
  #   
  #   physical@distribution <- new("ListOfdistribution",
  #                                c(distribution))
  #   
  #   
  #   if (os == "mac"){
  #     
  #     command_certutil <- paste("md5 ",
  #                               path,
  #                               "/",
  #                               table_names[i],
  #                               sep = "")
  #     
  #     certutil_output <- system(command_certutil, intern = T)
  #     
  #     checksum_md5 <- gsub(".*= ", "", certutil_output)
  #     
  #     authentication <- new("authentication",
  #                           method = "MD5",
  #                           checksum_md5)
  #     
  #     physical@authentication <- as(list(authentication),
  #                                   "ListOfauthentication")
  #     
  #   } else if (os == "win"){
  #     
  #     command_certutil <- paste("CertUtil -hashfile ",
  #                               path,
  #                               "\\",
  #                               table_names[i],
  #                               " MD5",
  #                               sep = "")
  #     
  #     certutil_output <- system(command_certutil, intern = T)
  #     
  #     checksum_md5 <- gsub(" ", "", certutil_output[2])
  #     
  #     authentication <- new("authentication",
  #                           method = "MD5",
  #                           checksum_md5)
  #     
  #     physical@authentication <- as(list(authentication),
  #                                   "ListOfauthentication")
  #     
  #   }
  #   
  #   # Get number of records
  #   
  #   number_of_records <- as.character(dim(df_table)[1])
  #   
  #   # Pull together information for the data table
  #   
  #   data_table <- new("dataTable",
  #                     entityName = table_names[i],
  #                     entityDescription = data_table_descriptions[i],
  #                     physical = physical,
  #                     attributeList = attributeList,
  #                     numberOfRecords = number_of_records)
  #   
  #   data_tables_stored[[i]] <- data_table
  #   
  # }
  # 
  # 
  # # Compile datatables --------------------------------------------------------
  # 
  # # Are custom units present in these tables?
  # 
  # if (file.exists(paste(path,"/",substr(template, 1, nchar(template) - 14),"_custom_units.txt",sep = ""))){
  #   
  #   print("custom units ...")
  #   
  #   custom_units_df <- read.table(
  #     paste(path,
  #           "/",
  #           substr(template, 1, nchar(template) - 14),
  #           "_custom_units.txt",
  #           sep = ""),
  #     header = TRUE,
  #     sep = "\t",
  #     as.is = TRUE,
  #     na.strings = "")
  #   
  #   if (nrow(custom_units_df) < 1){
  #     custom_units <- "no"
  #   } else {
  #     custom_units <- "yes"
  #   }
  #   
  #   # Clean white spaces from custom_units and units_types
  #   
  #   if (custom_units == "yes"){
  #     
  #     for (j in 1:ncol(custom_units_df)){
  #       if (class(custom_units_df[ ,j]) == "character" ||
  #           (class(custom_units_df[ ,j]) == "factor")){
  #         custom_units_df[ ,j] <- trimws(custom_units_df[ ,j])
  #       }
  #     }
  #     
  #     unitsList <- set_unitList(custom_units_df)
  #   }
  #   
  #   
  # } else {
  #   
  #   custom_units <- "no"
  #   
  # }
  # 
  # 
  # # Compile data tables
  # 
  # dataset@dataTable <- new("ListOfdataTable",
  #                          data_tables_stored)
  # 
  # # Build EML
  # 
  # print("Compiling EML ...")
  # 
  # 
  # if (custom_units == "yes"){
  #   eml <- new("eml",
  #              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
  #              packageId = data_package_id,
  #              system = root_system,
  #              access = access,
  #              dataset = dataset,
  #              additionalMetadata = as(unitsList, "additionalMetadata"))
  # } else {
  #   eml <- new("eml",
  #              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
  #              packageId = data_package_id,
  #              system = root_system,
  #              access = access,
  #              dataset = dataset)
  # }
  # 
  # # Write EML
  # 
  # print("Writing EML ...")
  # 
  # write_eml(eml, paste(path, "/", data_package_id, ".xml", sep = ""))
  # 
  # # Validate EML
  # 
  # print("Validating EML ...")
  # 
  # validation_result <- eml_validate(eml)
  # 
  # if (validation_result == "TRUE"){
  #   
  #   print("EML passed validation!")
  #   
  # } else {
  #   
  #   print("EML validaton failed. See warnings for details.")
  #   
  # }
  # 
  
}


