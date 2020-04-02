#' Get summaries of all available data packages in the ecocomDP format
#'     
#' @return 
#'     (tibble) A tibble with fields:
#'     \itemize{
#'         \item \strong{package_id}: Data package ID
#'         \item \strong{title}: Data package title
#'         \item \strong{original_package_id}: The parent data package ID
#'         \item \strong{length_of_survey_years}: The number of years surveyed
#'         \item \strong{number_of_years_sampled}: Number of years sampled
#'         \item \strong{std_dev_interval_betw_years}: Standard deviation interval between years
#'         \item \strong{max_num_taxa}: Number of unique taxa observed
#'         \item \strong{geo_extent_bounding_box_m2}: Area (square meters) over which samples were taken.
#'     }
#'     
#' @examples
#' \dontrun{
#' view_all_ecocomDP()
#' }
#'         
#' @export
#'
view_all_ecocomDP <- function() {
  .Deprecated(
    new = 'search_data',
    package = 'ecocomDP',
    old = 'view_all_ecocomDP')
}