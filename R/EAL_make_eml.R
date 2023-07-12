# Make EML metadata
#
# @description  
#     Render the contents of metadata templates into EML, validate, and write
#     to file.
#     
# @param path 
#     (character) Path to the metadata template directory.
# @param data.path
#     (character) Path to the data directory.
# @param eml.path
#     (character) Path to the EML directory, where EML files are written.
# @param dataset.title
#     (character) Title of the dataset.
# @param temporal.coverage
#     (character) Beginning and ending dates of the dataset in the format 
#     "YYYY-MM-DD" (e.g. 
#     \code{temporal.coverage = c('2012-05-01', '2014-11-30')}).
# @param geographic.description
#     (character) Description of datasets geographic extent. Don't use this 
#     argument if geographic coverage is supplied by geographic_coverage.txt.
# @param geographic.coordinates
#     (character) Coordinates of datasets geographic extent. Coordinates are
#     listed in this order: North, East, South, West (e.g. 
#     \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).
#     Longitudes west of the prime meridian and latitudes south of the equator 
#     are negative. Don't use this argument if geographic coverage is supplied
#     by geographic_coverage.txt.
# @param maintenance.description
#     (character) A description of data collection status (e.g. "ongoing", 
#     "complete"), communicating the frequency of updates.
# @param data.table
#     (character; optional) Table file name. If more than one, then supply 
#     as a vector of character strings (e.g. 
#     \code{data.table = c("nitrogen.csv", "decomp.csv")}).
# @param data.table.name
#     (character; optional) A short descriptive name for the table. Defaults
#     to \code{data.table}. If more than one, then supply as a vector of 
#     character strings in the same order as listed in \code{data.table}.
# @param data.table.description
#     (character; optional) Table description. If more than one, then supply 
#     as a vector of character strings in the same order as listed in 
#     \code{data.table}.
# @param data.table.quote.character
#     (character; optional) Quote character used in \code{data.table}. If 
#     more than one, then supply as a vector of character strings in the same 
#     order as listed in \code{data.table}. If the quote character is a quotation, 
#     then enter \code{'"'}. If the quote character is an apostrophe, then 
#     enter \code{"'"}. If wanting to include quote characters for some but 
#     not all \code{data.table}, then use a "" for those that don't have a 
#     quote character (e.g. \code{data.table.quote.character = 
#     c("'", "")}).
# @param data.table.url
#     (character; optional) The publicly accessible URL from which 
#     \code{data.table} can be downloaded. If more than one, then supply as 
#     a vector of character strings in the same order as listed in 
#     \code{data.table}. If wanting to include URLs for some but not all
#     \code{data.table}, then use a "" for those that don't have a URL
#     (e.g. \code{data.table.url = c("", "/url/to/decomp.csv")}).
# @param other.entity
#     (character; optional) Name of \code{other.entity}(s) in this 
#     dataset. Use \code{other.entity} for all non-\code{data.table} files. 
#     \code{other.entity}(s) should be stored at \code{data.path}. If more 
#     than one, then supply as a vector of character strings (e.g. 
#     \code{other.entity = c('ancillary_data.zip', 'quality_control.R')}).
# @param other.entity.name
#     (character; optional) A short descriptive name for the other.entity. 
#     Defaults to \code{other.entity}. If more than one, then supply as a 
#     vector of character strings in the same order as listed in 
#     \code{other.entity}.
# @param other.entity.description
#     (character; optional) Description(s) of \code{other.entity}(s). If more 
#     than one, then supply as a vector of descriptions in the same order as 
#     listed in \code{other.entity}.
# @param other.entity.url
#     (character; optional) The publicly accessible URL from which 
#     \code{other.entity} can be downloaded. If more than one, then supply as 
#     a vector of character strings in the same order as listed in 
#     \code{other.entity}. If wanting to include URLs for some but not all
#     \code{other.entity}, then use a "" for those that don't have a URL
#     (e.g. \code{other.entity.url = c("", "/url/to/quality_control.R")}).
# @param provenance
#     (character; optional) EDI Data Repository Data package ID(s) 
#     corresponding to parent datasets from which this dataset was created 
#     (e.g. \code{knb-lter-cap.46.3}).
# @param user.id
#     (character; optional) Repository user identifier. If more than one, 
#     then enter as a vector of character strings (e.g. 
#     \code{c("user_id_1", "user_id_2")}). \code{user.id} sets the 
#     /eml/access/principal element for all \code{user.domain} except
#     "KNB", "ADC", and if \code{user.domain = NULL}.
# @param user.domain
#     (character; optional) Repository domain associated with 
#     \code{user.id}. Currently supported values are "EDI" 
#     (Environmental Data Initiative), "LTER" (Long-Term Ecological Research 
#     Network), "KNB" (The Knowledge Network for Biocomplexity), "ADC" (The 
#     Arctic Data Center). If you'd like your system supported please contact
#     maintainers of the ecocomDP R package. If using more than one 
#     \code{user.domain}, then enter as a vector of character strings (e.g. 
#     \code{c("user_domain_1", "user_domain_2")}) in the same order as 
#     corresponding \code{user.id}. If \code{user.domain} is missing then a 
#     default value "unknown" is assigned. \code{user.domain} sets the EML 
#     header "system" attribute and for all \code{user.domain}, except "KNB" 
#     and "ADC", sets the /eml/access/principal element attributes and values.
# @param package.id
#     (character; optional) Data package ID for the dataset described by this
#     EML. Ask your data repository for a package ID. Missing \code{package.id}
#     is assigned a UUID.
# @param write.file
#     (logical; optional) Whether to write the EML file.
# @param return.obj
#     (logical; optional) Whether to return the EML as an R object of class \code{EML object}. This EML object can be modified ad written to file according to the \href{https://github.com/ropensci/EML}{EML R library}.
# @param x
#     (named list; optional) Alternative input to 
#     \code{create_eml()}. Use \code{EAL_template_arguments()} 
#     to create \code{x}.
#     
# @return 
#     \itemize{
#         \item \strong{EML file} written to \code{eml.path}.
#         \item \strong{EML object} when \code{return.obj = TRUE}.
#     }
#     
# @details 
#     \code{create_eml()} reads the contents of metadata templates, 
#     auto-extracts additional metadata from the data entities, appends value 
#     added content (e.g. resolving keywords to controlled vocabularies), and 
#     adds all the metadata content to locations in the EML schema according 
#     with best practice recommendations of scientists, data managers, and 
#     data repositories. The EML is then validated against the schema and 
#     written to file.
#     
#     Character encodings in tabular metadata templates are converted to UTF-8 
#     via \code{enc2utf8()}. Characters in TextType metadata templates are not
#     yet converted. Note: This may lead to an inaccuracy and disconnect 
#     between values in the data objects and what is reported in the EML (e.g.
#     a categorical variable listed in the EML may not be the same as it's 
#     corresponding value in the data object). For this reason it's important 
#     to work with UTF-8 encoded data and metadata.
#
EAL_make_eml <- function(
  path,
  data.path = path,
  eml.path = path, 
  dataset.title = NULL,
  temporal.coverage = NULL,
  geographic.description = NULL, 
  geographic.coordinates = NULL, 
  maintenance.description = NULL, 
  data.table = NULL, 
  data.table.name = data.table,
  data.table.description = NULL, 
  data.table.quote.character = NULL, 
  data.table.url = NULL,
  other.entity = NULL,
  other.entity.name = other.entity,
  other.entity.description = NULL,
  other.entity.url = NULL,
  provenance = NULL,
  user.id = NULL,
  user.domain = NULL,
  package.id = NULL,
  write.file = TRUE,
  return.obj = FALSE,
  x = NULL
) {
  
  # Parameterize --------------------------------------------------------------
  
  # Get attributes of template files
  
  attr_tmp <- read_template_attributes()
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments().
  # When not using x, inputs are expected from path, data.path, and 
  # eml.path. When using x, only data.path is required unless write.file = TRUE
  # in which case eml.path is required.
  
  if (is.null(x) & missing(path)) {
    stop("Input argument 'path' is missing.")
  } else if (!is.null(x) & missing(path)) {
    path <- NULL
    if (missing(data.path)) {
      stop("Input argument 'data.path' is missing.")
    }
    if (isTRUE(write.file) & missing(eml.path)) {
      stop("Input argument 'write.file = TRUE' but 'eml.path' is missing.")
    } else if (!isTRUE(write.file) & missing(eml.path)) {
      eml.path <- NULL
    }
  }
  
  # Pass remaining arguments to validate_arguments()
  validate_arguments(
    fun.name = "make_eml",
    fun.args = as.list(environment()))
  
  # Handle deprecated arguments
  
  if (!is.null(provenance)) {
    warning(
      paste0(
        "Argument 'provenance' is deprecated; please use ",
        "'template_provanence()' instead."),
      call. = F)
  }
  
  # Read templates and data ---------------------------------------------------
  # The reading function (EAL_template_arguments()) ignores empty templates located
  # at path, thereby simplifying logic required to populate EML nodes below.
  
  if (is.null(x)) {
    if (is.null(data.table) & is.null(other.entity)) {      
      x <- EAL_template_arguments(
        path = path,
        data.path = data.path)$x
    } else if (!is.null(data.table) & is.null(other.entity)) {
      table_names <- suppressWarnings(
        validate_file_names(
          path = data.path, 
          data.files = data.table))
      x <- EAL_template_arguments(
        path = path,
        data.path = data.path,
        data.table = table_names)$x
    } else if (!is.null(data.table) & !is.null(other.entity)) {
      table_names <- suppressWarnings(
        validate_file_names(
          path = data.path, 
          data.files = data.table))
      x <- EAL_template_arguments(
        path = path,
        data.path = data.path,
        data.table = table_names,
        other.entity = other.entity)$x
    } else if (is.null(data.table) & !is.null(other.entity)) {
      x <- EAL_template_arguments(
        path = path,
        data.path = data.path,
        other.entity = other.entity)$x
    }
    data_read_2_x <- TRUE
  }
  
  # Clean templates of extraneous NA values -----------------------------------
  # Users often add NAs to templates where "" is expected. This 
  # removes NAs from where they shouldn't be and replaces them with "". NOTE:
  # Any value listed in the missingValueCode field is interpreted "as is" when
  # attributes.txt templates are input as files. In contrast, when inputs are 
  # supplied by the argument "x" and when is.na() returns TRUE for values in
  # the missingValueCode field, these NAs are converted to "".
  
  for (k in names(x$template)) {
    if (is.data.frame(x$template[[k]]$content)) {
      for (m in names(x$template[[k]]$content)) {
        if (m == "missingValueCode") {
          if (!exists("data_read_2_x")) {
            x$template[[k]]$content[[m]][
              is.na(x$template[[k]]$content[[m]])] <- ""
          }
        } else {
          x$template[[k]]$content[[m]][
            is.na(x$template[[k]]$content[[m]])] <- ""
        }
      }
    }
  }
  
  # Validate templates --------------------------------------------------------
  
  x <- remove_empty_templates(x)
  x <- validate_templates("make_eml", x)
  
  # Modify templates ----------------------------------------------------------
  # Modification of some template content helps with downstream processes.
  
  # catvars.txt:
  # - Remove incomplete cases
  # - Remove white space
  
  use_i <- stringr::str_detect(
    names(x$template), 
    attr_tmp$regexpr[attr_tmp$template_name == "catvars"])
  if (any(use_i)) {
    for (i in which(use_i)) {
      use_i <- (x$template[[i]]$content$attributeName == "") |
        (x$template[[i]]$content$code == "") |
        (x$template[[i]]$content$definition == "")
      x$template[[i]]$content <- x$template[[i]]$content[!use_i, ]
      x$template[[i]]$content <- as.data.frame(
        lapply(
          x$template[[i]]$content,
          trimws), 
        stringsAsFactors = F)
    }
  }
  
  # custom_units.txt:
  # - Remove white space
  
  if (!is.null(x$template$custom_units.txt)) {
    x$template$custom_units.txt$content <- as.data.frame(
      lapply(
        x$template$custom_units.txt$content,
        trimws), 
      stringsAsFactors = F)
  }
  
  # keywords.txt: 
  # - Remove blank keywords to reduce errors when matching to controlled 
  # vocabularies
  
  x$template$keywords.txt$content <- x$template$keywords.txt$content[
    x$template$keywords.txt$content$keyword != "", ]
  
  # personnel.txt:
  # - Make all roles lowercase for string matching and remove mistankenly 
  # entered white spaces
  
  x$template$personnel.txt$content$role <- tolower(
    x$template$personnel.txt$content$role)
  x$template$personnel.txt$content <- as.data.frame(
    lapply(
      x$template$personnel.txt$content,
      trimws), 
    stringsAsFactors = F)
  
  # personnel.txt: 
  # - Set default project title and funding elements when none are provided 
  # - Combine projectTitle and fundingNumber into new "funding" field
  
  x$template$personnel.txt$content$projectTitle[
    x$template$personnel.txt$content$projectTitle == ""] <- 
    "No project title to report"
  x$template$personnel.txt$content$funding <- trimws(
    paste(
      x$template$personnel.txt$content$fundingAgency,
      x$template$personnel.txt$content$fundingNumber))
  x$template$personnel.txt$content$funding[
    x$template$personnel.txt$content$funding == ""] <- "No funding to report"
  
  # table_attributes.txt:
  # - Convert contents in the class field to a consistent case
  # - Set units in non-numeric classes to ""
  # - Ignore dateTimeFormatString when not classified as date
  # - Remove white space
  
  use_i <- stringr::str_detect(
    names(x$template), 
    attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
  if (any(use_i)) {
    for (i in which(use_i)) {
      x$template[[i]]$content$class <- tolower(x$template[[i]]$content$class)
      x$template[[i]]$content$unit[
        x$template[[i]]$content$class != "numeric"] <- ""
      x$template[[i]]$content$dateTimeFormatString[
        x$template[[i]]$content$class != "date"] <- ""
      x$template[[i]]$content <- as.data.frame(
        lapply(
          x$template[[i]]$content,
          trimws), 
        stringsAsFactors = F)
    }
  }
  
  # taxonomic_coverage.txt
  # - Convert "" to NA as expected by txcl_make_taxonomicCoverage()
  # - Add unresolved names to the name_resolved field so they will be listed in 
  # the output EML (with a rank value of "unknown").
  
  if (!is.null(x$template$taxonomic_coverage.txt$content)) {
    x$template$taxonomic_coverage.txt$content[
      x$template$taxonomic_coverage.txt$content == "" ] <- NA_character_
    use_i <- is.na(x$template$taxonomic_coverage.txt$content$name_resolved)
    x$template$taxonomic_coverage.txt$content$name_resolved[use_i] <- 
      x$template$taxonomic_coverage.txt$content$name[use_i]
  }
  
  # Load helper funcitions ----------------------------------------------------
  
  # A function to set personnel roles: contact, creator, Principal 
  # investigator, associated party (other)
  
  set_person <- function(info_row, person_role) {
    
    if (person_role == "contact") {
      
      # If the contact only has givenName then the contact is an organization, 
      # otherwise it is a person.
      if ((x$template$personnel.txt$content[info_row, "givenName"] != "") & 
          (x$template$personnel.txt$content[info_row, "middleInitial"] == "") & 
          (x$template$personnel.txt$content[info_row, "surName"] == "")) {
        contact <- list(
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          positionName = stringr::str_to_title(x$template$personnel.txt$content[info_row,"givenName"]),
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19) {
          contact$userId <- list(
            directory = "https://orcid.org",
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
      } else {
        contact <- list(
          individualName = list(
            givenName = list(
              x$template$personnel.txt$content[info_row,"givenName"],
              x$template$personnel.txt$content[info_row,"middleInitial"]),
            surName = x$template$personnel.txt$content[info_row,"surName"]),
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          contact$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        # Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        contact <- rapply(
          contact,
          function(x){
            if (x == ""){
              x <- NULL
            } else {
              x
            }
          },
          how = c("replace"))
      }
      contact
      
    } else if (person_role == "creator") {
      
      creator <- list(
        individualName = list(
          givenName = list(
            x$template$personnel.txt$content[info_row,"givenName"],
            x$template$personnel.txt$content[info_row,"middleInitial"]),
          surName = x$template$personnel.txt$content[info_row,"surName"]),
        organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
        electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
      if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
        creator$userId <- list(
          directory = 'https://orcid.org',
          paste0(
            "https://orcid.org/", 
            x$template$personnel.txt$content[info_row,"userId"]))
      }
      # Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      creator <- rapply(
        creator,
        function(x){
          if (x == ""){
            x <- NULL
          } else {
            x
          }
        },
        how = c("replace"))
      creator
      
    } else if (person_role == "pi") {
      
      rp_personnel <- list(
        individualName = list(
          givenName = list(
            x$template$personnel.txt$content[info_row,"givenName"],
            x$template$personnel.txt$content[info_row,"middleInitial"]),
          surName = x$template$personnel.txt$content[info_row,"surName"]),
        organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
        electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
        role = 'Principal Investigator')
      if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
        rp_personnel$userId <- list(
          directory = 'https://orcid.org',
          paste0(
            "https://orcid.org/", 
            x$template$personnel.txt$content[info_row,"userId"]))
      }
      # Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      rp_personnel <- rapply(
        rp_personnel,
        function(x){
          if (x == ""){
            x <- NULL
          } else {
            x
          }
        },
        how = c("replace"))
      rp_personnel
      
    } else if (person_role == "publisher") {
      
      rp_personnel <- list(
        individualName = list(
          givenName = list(
            x$template$personnel.txt$content[info_row,"givenName"],
            x$template$personnel.txt$content[info_row,"middleInitial"]),
          surName = x$template$personnel.txt$content[info_row,"surName"]),
        organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
        electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
      if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
        rp_personnel$userId <- list(
          directory = 'https://orcid.org',
          paste0(
            "https://orcid.org/", 
            x$template$personnel.txt$content[info_row,"userId"]))
      }
      # Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      rp_personnel <- rapply(
        rp_personnel,
        function(x){
          if (x == ""){
            x <- NULL
          } else {
            x
          }
        },
        how = c("replace"))
      rp_personnel
      
    }else {
      
      # If givenName, middleName, and surName are blank then the 
      # associatedParty is an organization, otherwise the associatedParty 
      # is a person
      if ((x$template$personnel.txt$content[info_row, "givenName"] == "") & 
          (x$template$personnel.txt$content[info_row, "middleInitial"] == "") & 
          (x$template$personnel.txt$content[info_row, "surName"] == "")) {
        associated_party = list(
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
          role = stringr::str_to_title(x$template$personnel.txt$content[info_row,"role"]))
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        associated_party
      } else {
        associated_party <- list(
          individualName = list(
            givenName = list(
              x$template$personnel.txt$content[info_row,"givenName"],
              x$template$personnel.txt$content[info_row,"middleInitial"]
            ),
            surName = x$template$personnel.txt$content[info_row,"surName"]
          ),
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
          role = stringr::str_to_title(x$template$personnel.txt$content[info_row,"role"])
        )
        
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        # Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        associated_party <- rapply(
          associated_party,
          function(x){
            if (x == ""){
              x <- NULL
            } else {
              x
            }
          },
          how = c("replace"))
        associated_party
      }
      
    }
    
  }
  
  # Create <eml> --------------------------------------------------------------
  
  message("Making EML ...")
  message("<eml>")
  
  # Set default package.id
  
  if (is.null(package.id)) {
    warning("Missing 'package.id', assigning a UUID", call. = FALSE)
    package.id <- uuid::UUIDgenerate()
  }
  
  # Define the EML system attribute from user.domain
  
  if (("edi" %in% tolower(user.domain)) | ("lter" %in% tolower(user.domain))) {
    sys <- "edi"
  } else if ("knb" %in% tolower(user.domain)) {
    sys <- "knb"
  } else if ("adc" %in% tolower(user.domain)) {
    sys <- "https://arcticdata.io"
  } else {
    sys <- "unknown"
  }
  
  # Construct EML header
  
  eml <- list(
    schemaLocation = "https://eml.ecoinformatics.org/eml-2.2.0  https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd",
    packageId = package.id,
    system = sys)
  
  # Create <access> -----------------------------------------------------------
  # Create the access node for systems/repositories (i.e. user.domain) that 
  # use it, and if a user.id is specified.
  
  if ((eml$system != "knb") & 
      (eml$system != "https://arcticdata.io") &
      (!is.null(user.id))) {
    
    message("  <access>")
    
    # Set access attributes
    
    if (eml$system == "edi") {
      auth_sys <- "https://pasta.edirepository.org/authentication"
    } else {
      auth_sys <- "unknown"
    }
    
    eml$access <- list(
      scope = "document",
      order = "allowFirst",
      authSystem = auth_sys,
      allow = list())
    
    # Extrapolate user.id and user.domain if unequal lengths.
    
    if (!is.null(user.id) & !is.null(user.domain)) {
      if (length(user.id) != length(user.domain)) {
        user.id <- data.frame(
          user.id, user.domain, stringsAsFactors = FALSE)$user.id
        user.domain <- data.frame(
          user.id, user.domain, stringsAsFactors = FALSE)$user.domain
        warning(
          "The number of 'user.id' and 'user.domain' inputs differ. ",
          "Extrapolating to make equal:\nuser.id = ", 
          paste(user.id, collapse = ", "), "\nuser.domain = ", 
          paste(user.domain, collapse = ", "), call. = FALSE)
      }
    }
    
    # Set permissions to "all" for the principal and "read" for public
    
    r <- lapply(
      seq_along(user.id),
      function(k) {
        if (auth_sys == "https://pasta.edirepository.org/authentication") {
          if (tolower(user.domain[k]) == "lter") {
            principal <- paste0(
              "uid=", user.id[k], ",o=", user.domain[k], ",dc=ecoinformatics,dc=org")
          } else if (tolower(user.domain[k]) == "edi") {
            principal <- paste0(
              "uid=", user.id[k], ",o=", user.domain[k], ",dc=edirepository,dc=org")
          }
        } else {
          principal <- user.id[k]
        }
        permission <- "all"
        list(principal = principal, permission = permission)
      })
    r[[length(r)+1]] <- list(principal = "public", permission = "read")
    eml$access$allow <- r
    
  }
  
  # Create <dataset> ----------------------------------------------------------
  # Initialize the dataset list to which sub-nodes will be added
  
  message("  <dataset>")
  dataset <- list()
  
  # Create <title> ------------------------------------------------------------
  
  if (!is.null(dataset.title)) {
    message("    <title>")
    eml$dataset$title <- dataset.title
  }
  
  # Create <creator> ----------------------------------------------------------
  
  if (!is.null(x$template$personnel.txt)) {
    eml$dataset$creator <- lapply(
      which(x$template$personnel.txt$content$role == "creator"),
      function(k) {
        message("    <creator>")
        set_person(info_row = k, person_role = "creator")
      })
  }
  
  # Create <associatedParty> --------------------------------------------------
  
  if (!is.null(x$template$personnel.txt)) {
    eml$dataset$associatedParty <- lapply(
      which(
        stringr::str_detect(
          x$template$personnel.txt$content$role,
          "[^pi|^creator|^contact]")),
      function(k) {
        message("    <associatedParty>")
        set_person(info_row = k, person_role = "")
      })
  }
  
  # Create <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  eml$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")
  
  # Create <abstract> ---------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), 'abstract'))) {
    message("    <abstract>")
    eml$dataset$abstract <- x$template[[
      names(x$template)[stringr::str_detect(names(x$template), 'abstract')]
      ]]$content
  }
  
  # Create <keywordSet> -------------------------------------------------------
  # Try resolving keywords without a listed thesaurus to the LTER Controlled 
  # Vocabulary, then create a separate keywordSet for each thesaurus + keyword
  # group.
  
  if (!is.null(x$template$keywords.txt)) {
    
    use_i <- x$template$keywords.txt$content$keywordThesaurus == ""
    if (any(use_i)) {
      r <- try(
        vocab_resolve_terms(
          x = x$template$keywords.txt$content$keyword[use_i],
          cv = "lter"),
        silent = T)
      if (is.data.frame(r)) {
        x$template$keywords.txt$content[
          match(
            r[r$controlled_vocabulary != "", ]$term, 
            x$template$keywords.txt$content$keyword), ] <- 
          r[r$controlled_vocabulary != "", ]
      }
    }
    
    eml$dataset$keywordSet <- lapply(
      unique(x$template$keywords.txt$content$keywordThesaurus),
      function(k) {
        message("    <keywordSet>")
        if (k == "") {
          list(
            keyword = as.list(
              x$template$keywords.txt$content$keyword[
                x$template$keywords.txt$content$keywordThesaurus == k]))
        } else {
          list(
            keyword = as.list(
              x$template$keywords.txt$content$keyword[
                x$template$keywords.txt$content$keywordThesaurus == k]),
            keywordThesaurus = unique(
              x$template$keywords.txt$content$keywordThesaurus[
                x$template$keywords.txt$content$keywordThesaurus == k]))
        }
      })
    
  }
  
  # Create <additionalInfo> ---------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), "additional_info"))) {
    message('    <additionalInfo>')
    eml$dataset$additionalInfo <- x$template[[
      names(x$template)[
        stringr::str_detect(names(x$template), "additional_info")]]]$content
  }
  
  # Create <intellectualRights> -----------------------------------------------
  
  if (!is.null(stringr::str_detect(names(x$template), "intellectual_rights"))) {
    message("    <intellectualRights>")
    eml$dataset$intellectualRights <- x$template$intellectual_rights.txt$content
  }
  
  # Create <coverage> ---------------------------------------------------------
  
  message('    <coverage>')
  
  eml$dataset$coverage <- list()
  
  # Create <geographicCoverage> -----------------------------------------------
  
  # Check for multiple geographic coverage inputs.
  if (is.null(geographic.coordinates) & !any(stringr::str_detect(
    names(x$template), 
    "geographic_coverage.txt|bounding_boxes.txt"))) {
    
    warning("Geographic coverage is recommended.", call. = FALSE)
    
  } else {
    
    if (!is.null(x$template$geographic_coverage.txt)) {
      # Create the geographicCoverage node
      o <- x$template$geographic_coverage.txt$content
      eml$dataset$coverage$geographicCoverage <- lapply(
        seq_len(nrow(o)),
        function(k) {
          message('        <geographicCoverage>')
          list(
            geographicDescription = o$geographicDescription[k],
            boundingCoordinates = list(
              westBoundingCoordinate = o$westBoundingCoordinate[k],
              eastBoundingCoordinate = o$eastBoundingCoordinate[k],
              northBoundingCoordinate = o$northBoundingCoordinate[k],
              southBoundingCoordinate = o$southBoundingCoordinate[k]))
        })
    }
  }
  
  # Create <temporalCoverage> -------------------------------------------------
  
  if (!is.null(temporal.coverage)) {
    message("        <temporalCoverage>")
    eml$dataset$coverage$temporalCoverage <- list(
      rangeOfDates = list(
        beginDate = list(calendarDate = temporal.coverage[1]),
        endDate = list(calendarDate = temporal.coverage[2])))
  }
  
  # Create <taxonomicCoverage> ------------------------------------------------
  # Two sources of taxonomic coverage are supported: 
  #
  # 1.) The taxonomicCoverage EML node as an .xml file, which is read and 
  # inserted into the emld list object that make_eml() creates.
  #
  # 2.) The taxonomic_coverage.txt template listing taxa and authorities. 
  # Attempts are made to get the full hierarchy of taxonomic rank values for 
  # each taxa and render to EML.
  # Create methods for adding taxonomic authorities. Only ITIS is 
  # currently supported.
  # Allow taxonomic hierarchies to be supplied as a table (i.e. align
  # taxonomic_coverage.txt with the taxonomicCoverage option of 
  # EML::set_coverage()).
  
  if (!is.null(x$template$taxonomicCoverage.xml)) {
    message("        <taxonomicCoverage>")
    eml$dataset$coverage$taxonomicCoverage <- 
      x$template$taxonomicCoverage.xml$content
  } else if (!is.null(x$template$taxonomic_coverage.txt)) {
    message("        <taxonomicCoverage>")
    # Check for suggested packages
    if (!requireNamespace("ritis", quietly = TRUE)) {
      warning("Package 'ritis' is required for taxonomic rank expansion but is not installed", call. = FALSE)
    }
    if (!requireNamespace("taxize", quietly = TRUE)) {
      warning("Package 'taxize' is required for taxonomic rank expansion but is not installed", call. = FALSE)
    }
    if (!requireNamespace("worrms", quietly = TRUE)) {
      warning("Package 'worrms' is required for taxonomic rank expansion but is not installed", call. = FALSE)
    }
    
    # Expand taxonomic ranks and create taxonomic coverage
    tc <- try(
      suppressMessages(
        txcl_make_taxonomicCoverage(
          taxa.clean = x$template$taxonomic_coverage.txt$content$name_resolved,
          authority = x$template$taxonomic_coverage.txt$content$authority_system,
          authority.id = x$template$taxonomic_coverage.txt$content$authority_id,
          write.file = F)),
      silent = T)
    if (!("try-error" %in% class(tc))) {
      eml$dataset$coverage$taxonomicCoverage$taxonomicClassification <- tc$taxonomicClassification
    }
  }
  
  # Create <maintenance> ------------------------------------------------------
  
  if (!is.null(maintenance.description)) {
    message("    <maintenance>")
    eml$dataset$maintenance$description <- maintenance.description
  }
  
  # Create <contact> ----------------------------------------------------------
  
  if (!is.null(x$template$personnel.txt)) {
    eml$dataset$contact <- lapply(
      which(x$template$personnel.txt$content$role == "contact"),
      function(k) {
        message("    <contact>")
        set_person(info_row = k, person_role = "contact")
      })
  }
  
  # Create <publisher> --------------------------------------------------------
  
  eml$dataset$publisher <- lapply(
    which(x$template$personnel.txt$content$role == "publisher"),
    function(k) {
      message("    <publisher>")
      set_person(info_row = k, person_role = "publisher")
    })
  
  # Create <methods> ----------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), "methods"))) {
    message("    <methods>")
    # eml$dataset$methods$methodStep <- list(
    #   x$template[[
    #     names(x$template)[stringr::str_detect(names(x$template), "methods")]
    #     ]]$content$methodStep)
    
    eml$dataset$methods$methodStep$description$para <- 
      x$template[[
        names(x$template)[stringr::str_detect(names(x$template), "methods")]
        ]]$content$para[[1]]
  }
  
  # Create <methodStep> (provenance) ------------------------------------------
  # Get provenance metadata from supported systems (repositories) and external 
  # sources (everything else), then combine as a list of methodStep.
  
  if (!is.null(x$template$provenance.txt$content)) {
    
    # Identify internal sources
    internal_sources <- x$template$provenance.txt$content[
      x$template$provenance.txt$content$dataPackageID != "", ]
    
    # Parse internal sources
    if (nrow(internal_sources) != 0) {
      data_package_identifiers <- unique(internal_sources$dataPackageID)
      for (k in data_package_identifiers) {
        message("      <methodStep> (provenance metadata)")
        r <- httr::GET(
          paste0(
            url_env("production"),
            ".lternet.edu/package/provenance/eml/",
            stringr::str_replace_all(k, '\\.', '/')))
        if (r$status_code == 200) {
          prov <- httr::content(r, encoding = 'UTF-8')
          # Remove IDs from creator and contact to preempt ID + reference
          # errors
          xml2::xml_set_attr(
            xml2::xml_find_all(prov, './/dataSource/creator'),
            'id', NULL)
          xml2::xml_set_attr(
            xml2::xml_find_all(prov, './/dataSource/contact'),
            'id', NULL)
          # Write .xml to tempdir() and read back in as an emld list object
          # to be added to the dataset emld list under construction here
          xml2::write_xml(prov, paste0(tempdir(), "/provenance_metadata.xml"))
          prov <- EML::read_eml(paste0(tempdir(), "/provenance_metadata.xml"))
          prov$`@context` <- NULL
          prov$`@type` <- NULL
          eml$dataset$methods$methodStep[[
            length(eml$dataset$methods$methodStep)+1]] <- prov
          suppressMessages(file.remove(paste0(tempdir(), "/provenance_metadata.xml")))
        } else {
          message("Unable to get provenance metadata.")
        }
      }
    }
    
    # Identify external sources
    external_sources <- x$template$provenance.txt$content[
      x$template$provenance.txt$content$dataPackageID == "", ]
    
    # Parse external sources.
    # Some elements require an explicit NULL value to prevent them from 
    # being displayed in the returned EML as closing tags (i.e. </tag>). This 
    # is an issue in the EML R package. Is there a more concise way of handling
    # this issue than implemented here?
    
    if (nrow(external_sources) != 0) {
      source_titles <- unique(external_sources$title)
      provenance <- lapply(
        source_titles,
        function(title) {
          message("      <methodStep> (provenance metadata)")
          d <- x$template$provenance.txt$content[
            x$template$provenance.txt$content$title == title, ]
          # Initialize onlineDescription
          online_description <- d$onlineDescription[1]
          if (online_description == "") {
            online_description <- NULL
          }
          # Initialize provenance node
          
          out <- list(
            dataSource = list(
              title = title,
              creator = NULL,
              distribution = list(
                online = list(
                  onlineDescription = online_description,
                  url = d[1, "url"])),
              contact = NULL),
            description = "This provenance metadata does not contain entity specific information.")
          for (i in 1:nrow(d)) {
            # Initialize individualName
            if (trimws(d$middleInitial[i]) != "") {
              individual_name <- list(
                givenName = list(trimws(d$givenName[i]), trimws(d$middleInitial[i])),
                surName = d$surName[i])
            } else {
              individual_name <- list(
                givenName = list(trimws(d$givenName[i])),
                surName = d$surName[i])
            }
            if (individual_name$givenName[[1]] == "" &
                individual_name$surName == "") {
              individual_name <- NULL
            }
            # Initialize organizationName
            organization_name <- d$organizationName[i]
            if (organization_name == "") {
              organization_name <- NULL
            }
            # Initialize electronicMailAddress
            email <- d$email[i]
            if (email == "") {
              email <- NULL
            }
            # Add creator
            if (d$role[i] == "creator") {
              out$dataSource$creator[[length(out$dataSource$creator) + 1]] <- 
                list(
                  individualName = individual_name,
                  organizationName = organization_name,
                  electronicMailAddress = email)
            }
            # Add contact
            if (d$role[i] == "contact") {
              out$dataSource$contact[[length(out$dataSource$contact) + 1]] <- 
                list(
                  individualName = individual_name,
                  organizationName = organization_name,
                  electronicMailAddress = email)
            }
          }
          out
        })
      # Add to eml
      eml$dataset$methods$methodStep <- c(eml$dataset$methods$methodStep, provenance)
    }
    
  }
  
  # Create <project> ----------------------------------------------------------
  # The project metadata corresponding to the first "pi" listed in 
  # personnel.txt will become the primary project. Project metadata listed 
  # under supbsequent "pi" will become related projects in the order they are 
  # listed.
  
  if (!is.null(x$template$personnel.txt)) {
    use_i <- x$template$personnel.txt$content$role == "pi"
    if (any(use_i)){
      for (k in which(use_i)) {
        if (k == min(which(use_i))) {
          message("    <project>")
          eml$dataset$project <- list(
            title = x$template$personnel.txt$content$projectTitle[k],
            personnel = set_person(info_row = k, person_role = "pi"),
            funding = x$template$personnel.txt$content$funding[k])
        } else {
          message("      <relatedProject>")
          eml$dataset$project$relatedProject[[
            length(eml$dataset$project$relatedProject)+1]] <- list(
              title = x$template$personnel.txt$content$projectTitle[k],
              personnel = set_person(info_row = k, person_role = "pi"),
              funding = x$template$personnel.txt$content$funding[k])
        }
      }
    }
  }
  
  # Create <dataTable> --------------------------------------------------------
  
  if (!is.null(x$data.table)) {
    
    eml$dataset$dataTable <- lapply(
      names(x$data.table),
      function(k) {
        message(k)
        
        # Get corresponding table_attributes.txt
        tbl_attr <- x$template[[
          paste0("attributes_", tools::file_path_sans_ext(k), ".txt")]]$content
        
        # Break if no table_attributes.txt
        if (is.null(tbl_attr)) {
          return()
        }
        
        message(paste0("    <dataTable> (", k, ")"))
        
        # Add attributes.txt contents to the data frame input expected by 
        # EML::set_attributes().
        # Order of attributes listed in the template may not be match 
        # the order listed in the table so ... reorder attributes according
        # to the data table listing.
        
        attributes <- data.frame(
          attributeName = tbl_attr$attributeName,
          formatString = tbl_attr$dateTimeFormatString,
          unit = tbl_attr$unit,
          numberType = "",
          definition = "",
          attributeDefinition = tbl_attr$attributeDefinition,
          columnClasses = tbl_attr$class,
          minimum = NA,
          maximum = NA,
          missingValueCode = tbl_attr$missingValueCode,
          missingValueCodeExplanation = tbl_attr$missingValueCodeExplanation,
          stringsAsFactors = F)
        
        # Update missingValueCode - NA must be "NA" otherwise it will not be 
        # listed in the EML
        
        attributes$missingValueCode[is.na(attributes$missingValueCode)] <- "NA"
        
        # Update non-numeric attributes - categorical and character classes must 
        # have a numberType of "character" and their attributeDefinition must be 
        # listed under "defintion. date and categorical classes must be listed as 
        # "Date" and "factor" respectively
        
        use_i <- (attributes$columnClasses == "categorical") | 
          (attributes$columnClasses == "character")
        attributes$numberType[use_i] <- "character"
        attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
        attributes$columnClasses[
          attributes$columnClasses == "date"] <- "Date"
        attributes$columnClasses[
          attributes$columnClasses == "categorical"] <- "factor"
        
        # Update numeric attributes - Remove NA and missingValueCode for 
        # calculations. Get minimum and maximum values. Infer numberType
        
        for (i in which(tbl_attr$class == "numeric")) {
          a <- x$data.table[[k]]$content[[tbl_attr$attributeName[i]]][
            !is.na(x$data.table[[k]]$content[[tbl_attr$attributeName[i]]])]
          if (methods::is(a, "integer64")) { # An exception for integer64 - Does not behave like a "numeric". Can remove this if we remove the data.table lib dependency
            a <- as.numeric(a)
          }
          a <- a[a != tbl_attr$missingValueCode[i]]
          if (all(is.na(a))) {
            attributes$minimum[i] <- NA
            attributes$maximum[i] <- NA
          } else {
            attributes$minimum[i] <- as.numeric(min(a, na.rm = T))
            attributes$maximum[i] <- as.numeric(max(a, na.rm = T))
          }
          is_integer <-function(x, tol = .Machine$double.eps^0.5) {
            abs(x - round(x)) < tol
          }
          if (any(!is_integer(a))) {
            attributes$numberType[i] <- "real"
          } else if (!any(a < 1)) {
            attributes$numberType[i] <- "natural"
          } else if (any(a < 0)) {
            attributes$numberType[i] <- "integer"
          } else {
            attributes$numberType[i] <- "whole"
          }
          # Calculate precision for numeric attributes. Alternatively
          # this should be added to the attributes template so precision can
          # be manually defined by the metadata creator.
        }
        
        # Create attributeList
        
        if (paste0("catvars_", tools::file_path_sans_ext(k), ".txt") %in% 
            names(x$template)) {
          attributeList <- suppressWarnings(
            EML::set_attributes(
              attributes[ , c(
                "attributeName", 
                "formatString",
                "unit",
                "numberType",
                "definition",
                "attributeDefinition",
                "minimum",
                "maximum",
                "missingValueCode",
                "missingValueCodeExplanation")],
              factors = x$template[[
                paste0(
                  "catvars_", 
                  tools::file_path_sans_ext(k), 
                  ".txt")]]$content,
              col_classes = attributes$columnClasses))
        } else {
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
              col_classes = attributes$columnClasses))
        }
        
        # Set physical
        physical <- suppressMessages(
          EML::set_physical(
            paste0(data.path, "/", k),
            numHeaderLines = "1",
            recordDelimiter = get_eol(
              path = data.path,
              file.name = k),
            attributeOrientation = "column",
            url = "placeholder"))
        
        if (!is.null(data.table.quote.character)) {
          # data.table.quote.character isn't required for each data table, but 
          # must have a non-NULL entry if other data tables have a quote 
          # character. A "" or NA indicates to skip assignment.
          quote_character <- data.table.quote.character[
            which(k == names(x$data.table))]
          if ((quote_character == "") | is.na(quote_character)) {
            physical$dataFormat$textFormat$simpleDelimited$quoteCharacter <- 
              NULL
          } else {
            physical$dataFormat$textFormat$simpleDelimited$quoteCharacter <- 
              quote_character
          }
        }
        
        if (!is.null(data.table.url)) {
          # data.table.url isn't required for each data table, but must have a 
          # non-NULL entry in the data.table.url if other data tables have a 
          # URL. A "" or NA indicates to skip URL assignment.
          url <- data.table.url[which(k == names(x$data.table))]
          if ((url == "") | is.na(url)) {
            physical$distribution <- list()
          } else {
            physical$distribution$online$url <- url
          }
        } else {
          physical$distribution <- list()
        }
        
        fdlim <- detect_delimiter(
          path = data.path,
          data.files = k,
          os = detect_os())
        if (fdlim == "\t") {
          fdlim <- "\\t" # requires escape char to be written, otherwise is blank
        }
        physical$dataFormat$textFormat$simpleDelimited$fieldDelimiter <- fdlim
        
        # Create dataTable
        data_table <- list(
          entityName = data.table.name[which(k == names(x$data.table))],
          entityDescription = data.table.description[which(k == names(x$data.table))],
          physical = physical,
          attributeList = attributeList,
          numberOfRecords = as.character(nrow(x$data.table[[k]]$content)))
        
        # EML v2.0.0 handles absense of missingValue codes differently
        # than EML v 1.0.3. This fixes the issue here, though it may be better
        # to implement the fix in EML v2.0.0.
        for (j in seq_along(data_table$attributeList$attribute)){
          if (data_table$attributeList$attribute[[j]]$missingValueCode$code == ''){
            data_table$attributeList$attribute[[j]]$missingValueCode$code <- NA_character_
            data_table$attributeList$attribute[[j]]$missingValueCode$codeExplanation <- NA_character_
          }
        }
        
        data_table
        
      }
    )
  }
  
  # Create <otherEntity> ------------------------------------------------------
  
  if (!is.null(x$other.entity)) {
    eml$dataset$otherEntity <- lapply(
      names(x$other.entity),
      function(k) {
        message(paste0("    <otherEntity> (", k, ")"))
        # Set physical
        # Some sub-routine in EML::set_physical() doesn't like the .zip 
        # file extension, so we suppress the warning here so users aren't 
        # uneccessarily burdened by it. Not a great solution since helpful 
        # warnings will be lost.
        physical <- suppressWarnings(
          suppressMessages(
            EML::set_physical(
              paste0(data.path, "/", k))))
        physical$dataFormat$textFormat <- NULL
        if (!requireNamespace("mime", quietly = TRUE)) {
          warning("Package 'mime' is required for detecting the script mime type but is not installed", call. = FALSE)
          physical$dataFormat$externallyDefinedFormat$formatName <- "Unknown"
        } else {
          physical$dataFormat$externallyDefinedFormat$formatName <- mime::guess_type(
            file = k, unknown = "Unknown", empty = "Unknown")
        }
        if (!is.null(other.entity.url)) {
          # other.entity.url isn't required for each data table, but must have a 
          # non-NULL entry in the other.entity.url if other data tables have a 
          # URL. A "" or NA indicates to skip URL assignment.
          url <- other.entity.url[which(k == names(x$other.entity))]
          if ((url == "") | is.na(url)) {
            physical$distribution <- list()
          } else {
            physical$distribution$online$url <- url
          }
        } else {
          physical$distribution <- list()
        }
        # Create otherEntity
        list(
          entityName = other.entity.name[
            which(k == names(x$other.entity))],
          entityDescription = other.entity.description[
            which(k == names(x$other.entity))],
          physical = physical,
          entityType = "unknown")
      })
  }
  
  # Create <additionalMetadata> -----------------------------------------------
  
  if (!is.null(x$template$custom_units.txt)) {
    message("  <additionalMetadata>")
    eml$additionalMetadata[[
      length(eml$dataset$additionalMetadata)+1]]$metadata$unitList <- EML::set_unitList(
        x$template$custom_units.txt$content)
  }
  
  # Create <annotations> ------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), "annotations.txt"))) {
    message("  <annotations>")
    eml <- suppressMessages(
      annotate_eml(
        annotations = x$template$annotations.txt$content,
        eml.in = eml))
  }
  
  # Write EML -----------------------------------------------------------------
  
  message("</eml>")
  if (isTRUE(write.file)) {
    message(paste0("Writing EML (", package.id, ".xml)"))
    emld::eml_version("eml-2.2.0")
    EML::write_eml(eml, paste0(eml.path, "/", package.id, ".xml"))
    # Fix TextType nodes - EML::set_TextType() and EML::set_methods() replace
    # special XML characters &, <, >, with their encoded representations. These
    # characters are encoded a second time by EML::write_eml() resulting in 
    # replacement of "&" by "&amp;" resulting in "&amp;gt;" etc. The below code
    # block patches this issue.
    txt <- readLines(paste0(eml.path, "/", package.id, ".xml"), encoding = "UTF-8")
    for (x in seq_along(txt)) {
      txt[x] <- stringr::str_replace_all(txt[x], "&amp;gt;", "&gt;")
      txt[x] <- stringr::str_replace_all(txt[x], "&amp;lt;", "&lt;")
      txt[x] <- stringr::str_replace_all(txt[x], "&amp;amp;", "&amp;")
    }
    writeLines(txt, paste0(eml.path, "/", package.id, ".xml"), useBytes = T)
  }
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")
  r <- EML::eml_validate(eml)
  if (isTRUE(r)) {
    message("  Validation passed :)")
  } else {
    message("  Validation failed :(")
  }
  message("Done.")
  
  if (isTRUE(return.obj)) {
    eml
  }
  
}
