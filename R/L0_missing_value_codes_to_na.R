#' Convert L0 missing value codes to NA
#'
#' @description  
#'     A function to convert missing value codes to NA. Converting missing 
#'     value codes to a single type simplifies downstream usage.
#'
#' @usage 
#'     L0_missing_value_codes_to_na(x, x.object.name, eml)
#'
#' @param x
#'     (data frame) L0 table with missing value codes to convert.
#' @param x.object.name
#'     (character) Object name in EML of the table to convert (i.e. file name).
#' @param eml
#'     (xml_document, xml_node) EML metadata. Use 
#'     `EDIutils::api_read_metadata()` or `xml2::read_xml()` to read in the 
#'     EML.
#'
#' @return 
#'     (data frame) `x` with missing value codes converted to NA.
#'     
#' @export
#'

L0_missing_value_codes_to_na <- function(x, x.object.name, eml){
  
  # For each column ...
  
  for (i in 1:ncol(x)){
    
    # Get missing value codes
    
    col_codes <- EDIutils::col_missing_value_code(
      col.name = colnames(x)[i], 
      object.name = x.object.name,
      eml = eml
    )
    
    # If missing value codes are present ...
    
    if (!is.null(col_codes)){
      
      # Replace missing value codes with NA_character_ or NA
      
      use_i <- x[ , colnames(x)[i]] == col_codes
      
      if (is.character(x[ , colnames(x)[i]])){
        
        x[use_i, colnames(x)[i]] <- NA_character_
        
      } else if (is.numeric(x[ , colnames(x)[i]])){
        
        x[use_i, colnames(x)[i]] <- NA
        
      }
      
    }
    
  }
  
  # Return
  
  x
  
}
