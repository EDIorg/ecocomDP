#' Aggregate ecocomDP data packages (deprecated in favor of read_data())
#'
#' @description  
#'     Aggregate ecocomDP data packages.
#'
#' @usage 
#'     aggregate_ecocomDP(
#'       package.id, 
#'       path = NULL
#'     )
#'     
#' @param package.id
#'     (character) IDs of data packages to aggregate. Use 
#'     \code{view_all_ecocomDP} to get package IDs of available ecocomDP 
#'     datasets. Previous data package revisions are valid. It's OK to if only
#'     one data package is entered.
#' @param path
#'     (character; optional) Path to the directory in which the aggregated 
#'     ecocomDP will be written.
#' @param neon.sites
#'     (character) NEON sites to use the data of. THIS ARGUMENT IS A WIP.
#'     
#' @return 
#'     \itemize{
#'         \item{(list) A named list of tibbles (one for each ecocomDP table)
#'         containing data from aggregated \code{package.id}s}
#'         \item{(.csv files) .csv files (one for each ecocomDP table) 
#'         containing data from aggregated \code{package.id}s}
#'     }
#'         
#' @export
#'

aggregate_ecocomDP <- function(package.id, path = NULL, neon.sites = NULL){

  .Deprecated(
    new = "read_data",
    package = "ecocomDP",
    old = "aggregate_ecocomDP")

}


