#' make_eml_neon
#'
#' @description  
#'     Make an EML metadata record for a NEON data product that has been 
#'     reformatted into the ecocomDP.
#'
#' @usage make_eml_neon(eml, x)
#' 
#' @param eml
#'     (eml) An EML record for a NEON data product.
#' @param x
#'     (list of data frames) The ecocomDP tables for the NEON data product.
#'     Each item of the list must be named after the ecocomDP tables (e.g.
#'     observation, taxon, dataset_summary, etc.).
#' @param protocols
#'     (character) Vector of character strings listing the NEON sampling 
#'     protocols.
#' @param dp.id 
#'     (character) NEON data product ID
#' @param eml.path
#'     (character) Path to which EML will be written to.
#'     
#' @return 
#'     EML metadata for a NEON data product.
#'         
#' @export
#'

make_eml_neon <- function(
  eml,
  x,
  protocols,
  dp.id,
  eml.path
  ){
  
  # Check input arguments -----------------------------------------------------
  
  # Edit title ----------------------------------------------------------------
  
  message('Editing:')
  
  message('<title>')
  
  title <- unlist(eml@dataset@title)
  
  title <- stringr::str_replace(
    title,
    ' at.*',
    ' (Reformatted to ecocomDP Design Pattern)'
    )
  
  short_name <- stringr::str_replace(
    title,
    ' at.*',
    ' (Reformatted to ecocomDP Design Pattern)'
  )
  
  title <- new(
    "title",
    title
    )
  
  eml@dataset@title <- as(list(title), "ListOftitle")
  
  # Edit abstract -------------------------------------------------------------
  
  message('<abstract>')
  
  lns <- paste0('This data package is formatted according to the "ecocomDP", a data package design pattern for ecological community surveys, and data from studies of composition and biodiversity. For more information on the ecocomDP project see https://github.com/EDIorg/ecocomDP/tree/master, or contact EDI https://environmentaldatainitiative.org.',
                '\n',
                '\n',
                'This metadata is updated periodically (last update ',
                format(Sys.time(), "%Y-%m-%d"),
                ').',
                '\n',
                '\n',
                'This data package was derived from the NEON data product found here: ',
                paste0(
                  'http://data.neonscience.org/data-product-view?dpCode=',
                  dp.id
                  )
                )
  
  abstract <- as(set_TextType(text = lns), "abstract")
  
  eml@dataset@abstract <- abstract
  
  # Edit short name -----------------------------------------------------------
  
  message('<shortName')
  
  eml@dataset@shortName <- short_name
  
  # Edit publication date -----------------------------------------------------
  
  message("<pubDate>")
  
  eml@dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")
  
  # Edit distribution ---------------------------------------------------------
  
  message("<distribution>")
  
  null_distribution <- list(NULL)
  
  eml@dataset@distribution <- as(null_distribution, "ListOfdistribution")
  
  # Edit geographic coverage --------------------------------------------------
  
  message('<geographicCoverage>')
  
  # Get bounding box info from location table
  
  locations <- unique.data.frame(
    select(
      x$location,
      location_id,
      latitude,
      longitude
      )
    )[complete.cases(x$location), ]
  
  geocov_data <- list(
    bounding_box = list(
      box_west = min(locations$longitude),
      box_east = max(locations$longitude),
      box_north = max(locations$latitude),
      box_south = min(locations$latitude)
      ),
    sites = locations
    )
  
  # Create geographic coverage
  
  list_of_coverage <- list()

  set_geo_coverage <- function(site, west, east, north, south){
    geographic_description <- new("geographicDescription", site)
    bounding_coordinates <- new("boundingCoordinates",
                                westBoundingCoordinate = as.character(west),
                                eastBoundingCoordinate = as.character(east),
                                northBoundingCoordinate = as.character(north),
                                southBoundingCoordinate = as.character(south))
    geographic_coverage <- new("geographicCoverage",
                               geographicDescription = geographic_description,
                               boundingCoordinates = bounding_coordinates)
    geographic_coverage
  }
  
  list_of_coverage <- mapply(
    set_geo_coverage,
    site = geocov_data$sites$location_id,
    west = geocov_data$sites$longitude,
    east = geocov_data$sites$longitude,
    north = geocov_data$sites$latitude,
    south = geocov_data$sites$latitude
  )
  
  geographic_description <- new("geographicDescription", 'Bounding box of sampling sites.')
  bounding_coordinates <- new("boundingCoordinates",
                              westBoundingCoordinate = as.character(geocov_data$bounding_box$box_west),
                              eastBoundingCoordinate = as.character(geocov_data$bounding_box$box_east),
                              northBoundingCoordinate = as.character(geocov_data$bounding_box$box_north),
                              southBoundingCoordinate = as.character(geocov_data$bounding_box$box_south))
  geographic_coverage <- new("geographicCoverage",
                             geographicDescription = geographic_description,
                             boundingCoordinates = bounding_coordinates)
  list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
  
  eml@dataset@coverage@geographicCoverage <- as(list_of_coverage, "ListOfgeographicCoverage")
  
  # Edit temporal coverage --------------------------------------------------
  
  message('<temporalCoverage>')
  
  dates <- lubridate::ymd_hm(x$observation$observation_datetime)
  
  temp <- set_coverage(
    begin = substr(floor_date(min(dates), unit = 'day'), 1, 10),
    end = substr(ceiling_date(max(dates), unit = 'day'), 1, 10)
  )
  eml@dataset@coverage@temporalCoverage <- as(
    temp@temporalCoverage,
    'ListOftemporalCoverage'
    )
  
  # Edit taxonomic coverage ---------------------------------------------------
  
  taxa_table <- select(spread(
    data = x$taxon, 
    key = taxon_rank, 
    value = taxon_name
    ),
    -taxon_id
  )
  if (sum(colnames(test) == '<NA>') > 0){
    taxa_table <- test[ , !colnames(test) == '<NA>']
  }
  
  taxcov <- list()
  for (i in 1:nrow(taxa_table)){
    taxa.row <- taxa_table[i, ]
    use_i <- !is.na(as.character(taxa.row))
    if (sum(use_i) > 0){
      taxcov[[i]] <- set_taxonomicCoverage(
        as.list(
          # taxa.row[1 , use_i, drop = F]
          taxa.row[use_i]
        )
      )
    } 
  }

  eml@dataset@coverage@taxonomicCoverage <- as(
    taxcov,
    'ListOftaxonomicCoverage'
  )
  
  # Edit methods --------------------------------------------------------------
  
  message('Add methods')
  lns <- paste0(
    'These NEON sampling protocols were used in creation of this dataset: ',
    paste(protocols, collapse = ', '),
    '. ',
    'Access these protocols at http://data.neonscience.org/documents'
    )
  
  ms <- new('methodStep', description = as(lns, 'description'))
  eml@dataset@methods@methodStep <- as(list(ms), 'ListOfmethodStep')
  
  # Edit attributes -----------------------------------------------------------

  # Compile attributes
  
  attributes_in <- compile_attributes_neon(x)
  data_tables_stored <- list()
  datetime.format <- 'YYYY-MM-DDThh:mmZ' #!!! This may vary with NEON data product
  
  for (i in 1:length(attributes_in[[1]])){
    use_i <- attributes_in[[1]][[i]]$columnClasses == "Date"
    attributes_in[[1]][[i]]$formatString[use_i] <- datetime.format
  }
  
  # Read table_descriptions.txt
  
  table_descriptions <- read.table(
    system.file('table_descriptions.txt',
                package = 'ecocomDP'
    ),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA",
    colClasses = rep("character", 2)
  )
  
  for (i in 1:length(x)){
    
    message(paste(
      "Adding",
      names(x)[i],
      "<dataTable>"))
    
    attributes <- attributes_in[[1]][[i]]
    
    # Read data table
    
    df_table <- as.data.frame(x[[i]])
    
    # Read catvars file
    # !!!This should be extracted from the NEON EML and will vary by data product
    
    # catvar <- grep(paste(table_names[i], "_variables.txt", sep = ""), dir_files, value = T)
    
    if ((dp.id == 'DP1.20120.001') & (names(x)[i] == 'observation')){
      
      catvars <- data.frame(
        attributeName = c('variable_name'),
        code = c('density'),
        definition = c('Count per square meter.'),
        stringsAsFactors = F
      )
      
      # Clean up
      
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
      
      for (j in 1:ncol(attributes)){
        if (class(attributes[ ,j]) == "character" ||
            (class(attributes[ ,j]) == "categorical")){
          attributes[ ,j] <- trimws(attributes[ ,j])
        }
      }
      
      # Get the column classes into a vector
      
      col_classes <- attributes[ ,"columnClasses"]
      
      # !!! This code should be replace by something more targeted
      col_classes[col_classes == "factor"] <- "character"
      # !!!
      
      # Create the attributeList element
      
      attributeList <- set_attributes(attributes,
                                      col_classes = col_classes)
      
      
    }
    

    # Set physical
    
    physical <- set_physical(names(x)[i],
                             numHeaderLines = "1",
                             recordDelimiter = "\\r\\n", # !!!This information is not accurate
                             attributeOrientation = "column",
                             fieldDelimiter = "\\t",
                             quoteCharacter = "\"")
    
    
    
    
    
    
    physical@size <- new("size",
                         unit = "byte",
                         as.character(
                           object.size(x[[i]])
                           )
                         )
    
    # Get number of records
    
    number_of_records <- as.character(dim(df_table)[1])
    
    # Pull together information for the data table
    
    data_table <- new("dataTable",
                      entityName = names(x)[i],
                      entityDescription = table_descriptions$description[match(names(x)[i], table_descriptions$table_name)],
                      physical = physical,
                      attributeList = attributeList,
                      numberOfRecords = number_of_records)
    
    data_tables_stored[[i]] <- data_table
    
  }
  
  # Compile data tables
  
  eml@dataset@dataTable <- new("ListOfdataTable",
                                  data_tables_stored)
  
  # Build EML -----------------------------------------------------------------
  
  eml <- new("eml",
             schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
             packageId = 'This data product is synthesized on demand from the NEON data portal and does not have a package ID.',
             system = 'These data are not stored in a repository.',
             access = eml@access,
             dataset = eml@dataset)
  
  # Validate EML --------------------------------------------------------------
  
  # message("Validating EML")
  # 
  # validation_result <- eml_validate(eml)
  # 
  # if (validation_result == "TRUE"){
  #   
  #   message("EML passed validation!")
  #   
  # } else {
  #   
  #   message("EML validaton failed. See warnings for details.")
  #   
  # }
  
  write_eml(
    eml,
    paste0(
      eml.path,
      '/',
      dp.id,
      '_neon.xml'
    ))

}










# compile_attributes_neon
#
# @description  
#     This is a helper function for `make_eml_neon.`` It compiles attributes, 
#     retrieves minimum and maximum values for numeric data and reformats the 
#     attributes table.
#
# @usage 
#     make_eml_neon(x)
#
# @param x
#     (list of data frames) The ecocomDP tables for the NEON data product.
#     Each item of the list must be named after the ecocomDP tables (e.g.
#     observation, taxon, dataset_summary, etc.).
#
# @return 
#     Attributes formatted for `make_eml_neon`.
#     
# @export
#
compile_attributes_neon <- function(x){
  
  # Loop through each table that is present -----------------------------------
  
  message('Compiling table attributes:')
  
  attributes_stored <- list()
  
  for (i in 1:length(x)){
    
    message(names(x)[i])
    
    # Read data table
    
    df_table <- as.data.frame(x[[i]])
    
    # Read attributes_draft table
    
    df_attributes <- read.table(system.file(
      paste0(
        'attributes_',
        names(x)[i],
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
    
    use_i <- match(colnames(df_table), df_attributes[["attributeName"]])
    
    df_attributes <- df_attributes[use_i, ]
    
    # Initialize outgoing attribute table 
    
    rows <- nrow(df_attributes)
    attributes <- data.frame(attributeName = character(rows),
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
                             stringsAsFactors = FALSE)
    
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
          
          attributes$minimum[is_numeric[j]] <- round(min(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
          
          attributes$maximum[is_numeric[j]] <- round(max(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
          
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
    
    attributes_stored[[i]] <- attributes
    
  }
  
  list("attributes" = attributes_stored)
  
}
