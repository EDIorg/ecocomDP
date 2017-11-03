#' Compile attributes
#'
#' @description  
#'     This is a helper function for make_eml.R. It compiles attributes, 
#'     retrieves minimum and maximum values for numeric data and reformats the 
#'     attributes table.
#'
#' @usage 
#'     make_eml(path, parent.package.id, child.package.id)
#'
#' @param path 
#'     A path to the dataset working directory containing the validated 
#'     ecocomDP tables.
#'     
#' @param delimiter
#'     The delimiter of input files. Can be comma (i.e. ",") or tab (i.e. "\\t")
#'
#' @return 
#'     Attributes formatted for make_eml.R
#'     
#' @export
#'

compile_attributes <- function(path, delimiter){

  # Set parameters ------------------------------------------------------------
  
  table_patterns <- c("observation\\b", "observation_ancillary\\b", "location_ancillary\\b", "taxon_ancillary\\b", "dataset_summary\\b", "location\\b", "taxon\\b")
  table_names <- c("observation", "observation_ancillary", "location_ancillary", "taxon_ancillary", "dataset_summary", "location", "taxon")
  dir_files <- list.files(path)
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
  
  # Loop through each table that is present -----------------------------------
  
  attributes_stored <- list()

  for (i in 1:length(table_names)){

    print(paste("Compiling", table_names[i], "attributes ..."))

    # Read data table
    
    df_table <- read.table(paste(path,
                                "/",
                                tables_found[i],
                                sep = ""),
                          header = T,
                          sep = delimiter,
                          as.is = T,
                          na.strings = "NA")
    
    # Read attributes_draft table
    
    df_attributes <- read.table(paste(path.package("ecocomDP"),
                                      "/attributes_",
                                      table_names[i],
                                      ".txt",
                                      sep = ""),
                                header = T,
                                sep = "\t",
                                as.is = T,
                                na.strings = "NA",
                                colClasses = rep("character", 7))
    
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
