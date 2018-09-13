#' compile_attributes_neon
#'
#' @description  
#'     This is a helper function for `make_eml_neon.`` It compiles attributes, 
#'     retrieves minimum and maximum values for numeric data and reformats the 
#'     attributes table.
#'
#' @usage 
#'     make_eml_neon(x)
#'
#' @param x
#'     (list of data frames) The ecocomDP tables for the NEON data product.
#'     Each item of the list must be named after the ecocomDP tables (e.g.
#'     observation, taxon, dataset_summary, etc.).
#'
#' @return 
#'     Attributes formatted for `make_eml_neon`.
#'     
#' @export
#'

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
