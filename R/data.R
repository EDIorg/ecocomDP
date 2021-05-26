#' Joined and flat version of EDI data package knb-lter-hfr.118.32
#'
#' A fully joined and flat version of EDI data package knb-lter-hfr.118.32 (Ant Assemblages in Hemlock Removal Experiment at Harvard Forest since 2003) with all relevant ecocomDP L1 identifiers and content added. Use this dataset as an input to the \code{L0_flat} argument of the "create" functions.
#'
#' @format A data frame with 2931 rows and 45 variables:
#' \describe{
#'   \item{datetime}{Sample dates}
#'   \item{block}{Sample location}
#'   ...
#' }
#' @source \url{https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118&revision=32}
"ants_L0_flat"








#' The ecocomDP version of EDI data package knb-lter-hfr.118.32
#'
#' The the ecocomDP (L1) formatted version of EDI data package knb-lter-hfr.118.32 (Ant Assemblages in Hemlock Removal Experiment at Harvard Forest since 2003) read from the EDI API with \code{read_data(id = "edi.194.4")}. Use this dataset as an input to data "use" functions.
#'
#' @format A list of:
#' \describe{
#'     \itemize{
#'       \item{edi.193.4 - The dataset identifier}
#'         \itemize{
#'           \item{metadata - See source url for metadata}
#'           \item{tables - A list of data frames, each an ecocomDP table}
#'           \item{validation_issues - Is NULL because there are no validation issues for this dataset}
#'       }
#'     }
#' }
#' @source \url{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=193&revision=4}
"ants_L1"