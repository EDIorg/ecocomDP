#' Convert L0 codes to definitions
#'
#' @description  
#'     A function to convert categorical codes to definitions. Occasionally 
#'     L1 content is locked up in L0 code definitions (e.g. species names). 
#'     This function converts these codes to the associated definitions.
#'
#' @usage 
#'     L0_codes_to_definitions(x, col.name, x.object.name, eml)
#'
#' @param x
#'     (data frame) L0 table containing the column to convert.
#' @param col.name
#'     (character) Name of column to convert.
#' @param x.object.name
#'     (character) Object name in EML of the table to convert (i.e. file name).
#' @param eml
#'     (xml_document, xml_node) EML metadata. Use 
#'     `EDIutils::api_read_metadata()` or `xml2::read_xml()` to read in the 
#'     EML.
#'
#' @return 
#'     (character) Vector of definitions corresponding to codes.
#'     
#' @export
#'


L0_codes_to_definitions <- function(x, col.name, x.object.name, eml){
  
  # Get data to convert
  
  x <- x[ , col.name]
  
  # Get attribute codes and definitions
  
  col_codes <- EDIutils::col_code(
    col.name = col.name, 
    object.name = x.object.name,
    eml = eml
  )
  
  col_definitions <- EDIutils::col_code_definition(
    col.name = col.name,
    object.name = x.object.name,
    eml = eml
  )
  
  # If categorial codes exist ...
  
  if (length(col_codes > 0)){
    
    # For each code ...
    
    for (i in 1:length(col_codes)){
      
      # Replace codes with definitions
      
      i_code <- x == col_codes[i]
      
      x[i_code] <- col_definitions[i]
      
    }
    
  }
  
  # Return
  
  x
  
}

