#' Update an ecocomDP dataset
#' 
#' @description 
#'     A function for ecocomDP dataset maintainers that helps recreate the 
#'     L0 to L1 conversion when an L0 has been updated.
#'
#' @param id
#'     (character) Identifier of L0 data package archived in the Environmental
#'     Data Initiative Repository that has at least one revision that has been
#'     converted to the ecocomDP and has a corresponding script that accords
#'     with ecocomDP conversion best practices.
#' @param path
#'     (character) Path to which the revised ecocomDP tables, EML metadata, 
#'     and message log will be written.
#'
#' @return
#' \item{message log}{(.txt file) A report of the conversion process 
#' the results of the conversion process and highlighting any issues, 
#' or differences between L0 revisions that may have impact on the conversion 
#' accuracy and that helps maintainers quickly idenfity any manual changes 
#' that need to occur.}
#' \item{tables}{(delimited files) The L0 data in L1 formatted tables using the
#' same record delimiter as used in the L0.}
#' \item{metadata}{(.xml) Metadata in the EML format.}
#'     
#' @export
#'
update_data <- function(id, path) {
  
  # TODO: Validate arguments
  
  # TODO: For id, get the corresponding L0 package identifier + revision number
  # and continue if a newer L0 revision exists, otherwise break as there is no 
  # work to be done.
  
  # TODO: Download new L0 into list of data.frames, and emld list object.
  # TODO: Download newest L0-to-L1 conversion script and EML generation + 
  # upload script.
  # TODO: Download old L0 into list of data.frames, and emld list object. This
  # is the L0 verion that corresponds with the newest L0-to-L1 conversion 
  # script.
  
  # TODO: Compare new L0 to old L0 while noting differences.
  
  # TODO: Attempt conversion.
  # TODO: Attempt building metadata
  # TODO: Attempt upload
  
  # TODO: Return message log
  
}