#' reformat_neon
#'
#' @description  
#'     Reformat NEON data products into the ecocomDP.
#'
#' @usage reformat_neon(dp.id)
#' 
#' @param dp.id 
#'     (character) NEON data product ID (e.g. 'DP1.20120.001').
#'     
#' @return 
#'     Core ecocomDP tables for a user supplied NEON data product.
#'         
#' @export
#'

reformat_neon <- function(dp.id){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(dp.id)){
    stop('Input argument "dp.id" is missing!')
  }
  
  # Begin ------------
  
  
  
  #
  
  
}