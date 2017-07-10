#' View the standard unit dictionary
#'
#' @description  
#'     View the standard unit dictionary in the RStudio source window.
#'
#' @usage 
#'     view_unit_dictionary()
#'     
#' @details
#'     Use the search field to find the unit of interest.     
#'     
#' @export     
#'     


view_unit_dictionary <- function(){
  
  standardUnits <- get_unitList()
  View(standardUnits$units)

}
