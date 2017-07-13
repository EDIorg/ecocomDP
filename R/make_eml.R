#' Make EML
#'
#' @description  
#'     Translate user supplied metadata into the EML schema for the Ecological 
#'     Community Data Pattern (ecocomDP), validate the schema, and write to 
#'     file.
#'
#' @usage 
#'     make_eml(path, import.eml = FALSE)
#'
#' @param path 
#'     A path to the dataset working directory containing the completed 
#'     metadata templates, \emph{eml_configuration.R}, 
#'     \emph{datasetname_datatablename_catvars.txt} (if categorical variables 
#'     are present), and \emph{geographic_coverage.txt} (if reporting detailed 
#'     geographic coverage).
#'     
#' @param import.eml
#'     If TRUE then EML for the raw dataset is read in from the working 
#'     directory and top level elements are used and data table specifc 
#'     elements are replaced with those for the ecocomDP tables.
#'
#' @return 
#'     Validation results printed to the \emph{Console}.
#'     
#'     An EML metadata file written to the dataset working directory titled 
#'     \emph{packageID.xml}.
#'     
#' @details 
#'     If validation fails, open the EML document in a .xml editor to identify 
#'     the source of error. Often the error is small and quickly resolved with 
#'     the aid of an editors schema congruence checker.
#'
#' @export
#'


make_eml <- function(path) {
  
  # # Check arguments
  # 
  # if (missing(path)){
  #   stop("Specify path to dataset working directory.")
  # }
  # 
  # # Get system information
  # 
  # sysinfo <- Sys.info()["sysname"]
  # if (sysinfo == "Darwin"){
  #   os <- "mac"
  # } else {
  #   os <- "win"
  # }
  # 
  # # Load the datasets configuration file
  # 
  # print("Loading configuration file ...")
  # 
  # source(paste(path, "/eml_configuration.R", sep = ""))
  # 
  # template <- paste(dataset_name,
  #                   "_template.docx",
  #                   sep = "")
  # 
  # # Compile attributes
  # 
  # attributes_in <- compile_attributes(path = path)
  # 
  # # Set file names
  # 
  # fname_abstract <- paste(path,
  #                         "/",
  #                         substr(template, 1, nchar(template) - 14),
  #                         "_abstract.txt",
  #                         sep = "")
  # 
  # fname_additional_info <- paste(path,
  #                                "/",
  #                                substr(template, 1, nchar(template) - 14),
  #                                "_additional_info.txt",
  #                                sep = "")
  # 
  # fname_personnel <- paste(path,
  #                          "/",
  #                          substr(template, 1, nchar(template) - 14),
  #                          "_personnel.txt",
  #                          sep = "")
  # 
  # fname_intellectual_rights <- paste(path,
  #                                    "/",
  #                                    substr(template, 1,
  #                                           nchar(template) - 14),
  #                                    "_intellectual_rights.txt", sep = "")
  # 
  # fname_methods <- paste(path,
  #                        "/",
  #                        substr(template, 1, nchar(template) - 14),
  #                        "_methods.txt",
  #                        sep = "")
  # 
  # fname_table_catvars <- c()
  # for (i in 1:length(table_names)){
  #   fname_table_catvars[i] <- paste(
  #     substr(table_names[i], 1, nchar(table_names[i]) - 4),
  #     "_catvars.txt",
  #     sep = "")
  # }
  # 
  # # Initialize data entity storage (tables)
  # 
  # data_tables_stored <- list()
  # 
  # # Load helper function to set personnel roles
  # 
  # set_person <- function(info_row, person_role){
  #   
  #   if (person_role == "contact"){
  #     
  #     individualName <- new(
  #       "individualName",
  #       givenName = trimws(personinfo[info_row,"givenName"]),
  #       surName = trimws(personinfo[info_row,"surName"]))
  #     
  #     contact <- new(
  #       "contact",
  #       individualName = individualName,
  #       organizationName = trimws(personinfo[info_row,"organizationName"]),
  #       electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
  #     
  #     contact
  #     
  #   } else if (person_role == "creator"){
  #     
  #     individualName <- new(
  #       "individualName",
  #       givenName = trimws(personinfo[info_row,"givenName"]),
  #       surName = trimws(personinfo[info_row,"surName"]))
  #     
  #     creator <- new(
  #       "creator",
  #       individualName = individualName,
  #       organizationName = trimws(personinfo[info_row,"organizationName"]),
  #       electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
  #     
  #     if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
  #       userId <- new("userId")
  #       userId@directory <- new("xml_attribute", "http://orcid.org")
  #       userId@.Data <- trimws(personinfo[info_row,"userId"])
  #       creator@userId <- new("ListOfuserId", c(userId))
  #     }
  #     
  #     creator
  #     
  #   } else if (person_role == "pi"){
  #     
  #     individualName <- new(
  #       "individualName",
  #       givenName = trimws(personinfo[info_row,"givenName"]),
  #       surName = trimws(personinfo[info_row,"surName"]))
  #     
  #     principal_investigator <- as.person(
  #       paste(
  #         trimws(personinfo[info_row, "givenName"]),
  #         " ",
  #         trimws(personinfo[info_row, "surName"]),
  #         " <",
  #         trimws(personinfo[info_row, "electronicMailAddress"]),
  #         ">",
  #         sep = ""))
  #     
  #     rp_personnel <- as(principal_investigator, "personnel")
  #     
  #     if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
  #       
  #       rp_userId <- new("userId")
  #       rp_userId@directory <- new("xml_attribute", "http://orcid.org")
  #       rp_userId@.Data <- paste("http://orcid.org/",
  #                                trimws(personinfo[info_row, "userId"]),
  #                                sep = "")
  #       rp_personnel@userId <- new("ListOfuserId", c(rp_userId))
  #       
  #     }
  #     
  #     role <- new("role", "Principal Investigator")
  #     rp_personnel@role <- new("ListOfrole", c(role))
  #     
  #     rp_personnel
  #     
  #   } else {
  #     
  #     individualName <- new(
  #       "individualName",
  #       givenName = trimws(personinfo[info_row,"givenName"]),
  #       surName = trimws(personinfo[info_row,"surName"]))
  #     
  #     associated_party <- new(
  #       "associatedParty",
  #       individualName = individualName,
  #       organizationName = trimws(personinfo[info_row,"organizationName"]),
  #       electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
  #     
  #     if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
  #       userId <- new("userId")
  #       userId@directory <- new("xml_attribute", "http://orcid.org")
  #       userId@.Data <- trimws(personinfo[info_row,"userId"])
  #       associated_party@userId <- new("ListOfuserId", c(userId))
  #     }
  #     
  #     role <- new("role", trimws(personinfo[info_row,"role"]))
  #     associated_party@role <- new("role", c(role))
  #     
  #     associated_party
  #     
  #   }
  #   
  # }
  # 
  # # Read personnel file
  # 
  # print("Read personnel file ...")
  # 
  # personinfo <- read.table(
  #   fname_personnel,
  #   header=TRUE,
  #   sep="\t",
  #   quote="\"",
  #   as.is=TRUE,
  #   comment.char = "",
  #   colClasses = rep("character", 6))
  # 
  # colnames(personinfo) <- c("givenName",
  #                           "surName",
  #                           "organizationName",
  #                           "electronicMailAddress",
  #                           "userId",
  #                           "role")
  # 
  # # Build modules--------------------------------------------------------------
  # 
  # print("Building EML:")
  # 
  # # Create EML
  # 
  # # Build eml-access module
  # 
  # print("access ...")
  # 
  # allow_principals <- c(paste("uid=",
  #                             user_id,
  #                             ",o=LTER,dc=ecoinformatics,dc=org",
  #                             sep = ""),
  #                       "public")
  # 
  # allow_permissions <- c("all",
  #                        "read")
  # 
  # access_order <- "allowFirst"
  # 
  # access_scope <- "document"
  # 
  # access <- new("access",
  #               scope = access_scope,
  #               order = access_order,
  #               authSystem = author_system)
  # 
  # allow <- list()
  # for (i in 1:length(allow_principals)){
  #   allow[[i]] <- new("allow",
  #                     principal = allow_principals[i],
  #                     permission = allow_permissions[i])
  # }
  # 
  # access@allow <- new("ListOfallow",
  #                     c(allow))
  # 
  # # Build dataset module
  # 
  # print("dataset ...")
  # 
  # dataset <- new("dataset",
  #                title = dataset_title)
  # 
  # # Add creators
  # 
  # print("creators ...")
  # 
  # personinfo <- read.table(
  #   paste(substr(fname_personnel, 1, nchar(fname_personnel) - 4),
  #         ".txt",
  #         sep = ""),
  #   header = TRUE,
  #   sep = "\t",
  #   as.is = TRUE,
  #   na.strings = "NA",
  #   colClasses = "character")
  # 
  # useI <- which(personinfo$role == "creator")
  # 
  # creator_list <- list()
  # for (j in 1:length(useI)){
  #   creator_list[[j]] <- set_person(info_row = useI[j],
  #                                   person_role = "creator")
  # }
  # 
  # dataset@creator <- as(creator_list, "ListOfcreator")
  # 
  # # Add publicaton date
  # 
  # print("publication date ...")
  # 
  # dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")
  # 
  # # Add abstract
  # 
  # print("abstract ...")
  # 
  # dataset@abstract <- as(set_TextType(fname_abstract), "abstract")
  # 
  # # Add keywords
  # 
  # print("keywords ...")
  # 
  # dataset@keywordSet <- new("ListOfkeywordSet", c(new("keywordSet", keywords)))
  # 
  # # Add intellectual rights
  # 
  # print("intellectual rights ...")
  # 
  # dataset@intellectualRights <- as(
  #   set_TextType(fname_intellectual_rights),
  #   "intellectualRights")
  # 
  # # Add coverage
  # 
  # print("coverage ...")
  # 
  # dataset@coverage <- set_coverage(begin = begin_date,
  #                                  end = end_date,
  #                                  geographicDescription = geographic_location,
  #                                  west = coordinate_west,
  #                                  east = coordinate_east,
  #                                  north = coordinate_north,
  #                                  south = coordinate_south)
  # 
  # # Add maintenance
  # 
  # print("maintenance ...")
  # 
  # maintenance <- new("maintenance")
  # maintenance@description <- as(maintenance_description, "description")
  # dataset@maintenance <- maintenance
  # 
  # # Add contacts
  # 
  # print("contacts ...")
  # 
  # personinfo <- read.table(
  #   paste(substr(fname_personnel, 1, nchar(fname_personnel) - 4),
  #         ".txt",
  #         sep = ""),
  #   header = TRUE,
  #   sep = "\t",
  #   as.is = TRUE,
  #   na.strings = "NA",
  #   colClasses = "character")
  # 
  # useI <- which(personinfo$role == "contact")
  # 
  # contact_list <- list()
  # for (j in 1:length(useI)){
  #   contact_list[[j]] <- set_person(info_row = useI[j],
  #                                   person_role = "contact")
  # }
  # 
  # dataset@contact <- as(contact_list, "ListOfcontact")
  # 
  # # Add methods
  # 
  # print("methods ...")
  # 
  # dataset@methods <- set_methods(fname_methods)
  # 
  # if (file.exists(paste(path, "/", "geographic_coverage.txt", sep = ""))){
  #   
  #   print("sampling coordinates ...")
  #   
  #   df_geographic_coverage <- read.table(paste(path,
  #                                              "/",
  #                                              "geographic_coverage.txt",
  #                                              sep = ""),
  #                                        sep = "\t",
  #                                        header = TRUE,
  #                                        as.is = TRUE,
  #                                        quote="",
  #                                        na.strings = "NA",
  #                                        colClasses = c("numeric","numeric","character"),
  #                                        comment.char = "#",
  #                                        fileEncoding = "UTF-8")
  #   
  #   df_geographic_coverage$latitude <- as.character(df_geographic_coverage$latitude)
  #   
  #   df_geographic_coverage$longitude <- as.character(df_geographic_coverage$longitude)
  #   
  #   df_geographic_coverage$site <- as.character(df_geographic_coverage$site)
  #   
  #   list_of_coverage <- list()
  #   
  #   for (i in 1:dim(df_geographic_coverage)[1]){
  #     
  #     coverage <- new("coverage")
  #     
  #     geographic_description <- new("geographicDescription", df_geographic_coverage$site[i])
  #     
  #     bounding_coordinates <- new("boundingCoordinates",
  #                                 westBoundingCoordinate = df_geographic_coverage$longitude[i],
  #                                 eastBoundingCoordinate = df_geographic_coverage$longitude[i],
  #                                 northBoundingCoordinate = df_geographic_coverage$latitude[i],
  #                                 southBoundingCoordinate = df_geographic_coverage$latitude[i])
  #     
  #     geographic_coverage <- new("geographicCoverage",
  #                                geographicDescription = geographic_description,
  #                                boundingCoordinates = bounding_coordinates)
  #     
  #     coverage@geographicCoverage <- as(list(geographic_coverage), "ListOfgeographicCoverage")
  #     
  #     list_of_coverage[[i]] <- coverage
  #     
  #   }
  #   
  #   sampling <- new("sampling")
  #   
  #   sampling@studyExtent@coverage <- as(list_of_coverage, "ListOfcoverage")
  #   
  #   sampling@samplingDescription <- as("Geographic coordinates of sampling sites", "samplingDescription")
  #   
  #   dataset@methods@sampling <- as(list(sampling), "ListOfsampling")
  #   
  # }
  # 
  # # Add project and funding
  # 
  # print("project and funding ...")
  # 
  # useI <- which(personinfo$role == "pi")
  # 
  # pi_list <- list()
  # for (j in 1:length(useI)){
  #   pi_list[[j]] <- set_person(info_row = useI[j],
  #                              person_role = "pi")
  # }
  # 
  # project <- new("project",
  #                title = funding_title,
  #                personnel = pi_list,
  #                funding = funding_grants)
  # 
  # dataset@project <- project
  # 
  # # Add associated parties
  # 
  # print("associated parties ...")
  # 
  # useI <- which(personinfo$role != "pi" &
  #                 personinfo$role != "creator" &
  #                 personinfo$role != "contact")
  # 
  # if (length(useI) != 0){
  #   associated_party_list <- list()
  #   for (j in 1:length(useI)){
  #     associated_party_list[[j]] <- set_person(info_row = useI[j],
  #                                              person_role = "")
  #   }
  #   dataset@associatedParty <- as(associated_party_list, "ListOfassociatedParty")
  # }
  # 
  # # Add additional information
  # 
  # print("additional info ...")
  # 
  # if (file.exists(fname_additional_info)){
  #   
  #   additional_info <- as(set_TextType(fname_additional_info), "additionalInfo")
  #   
  #   dataset@additionalInfo <- as(list(additional_info), "ListOfadditionalInfo")
  #   
  # }
  # 
  # 
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


