#' view_all_ecocomDP
#'
#' @description  
#'     View list of all available ecocomDP with high level metadata.
#'
#' @usage view_all_ecocomDP()
#'     
#' @return 
#'     A data frame with fields:
#'     \itemize{
#'         \item package_id: Data package ID
#'         \item title: Data package title
#'         \item original_package_id: The parent data package ID
#'         \item length_of_survey_years: The number of years surveyed
#'         \item number_of_years_sampled: Number of years sampled
#'         \item std_dev_interval_betw_years: Standard deviation interval between years
#'         \item max_num_taxa: Number of unique taxa observed
#'         \item geo_extent_bounding_box_m2: Area (square meters) over which samples were taken.
#'     }
#'         
#' @export
#'

view_all_ecocomDP <- function(){

  # Query the EDI Data Repository ---------------------------------------------
  
  # Get package IDs and titles
  
  query <- httr::GET('https://pasta.lternet.edu/package/search/eml?q=keyword:ecocomDP&fl=packageid,title&rows=1000')
  
  if (status_code(query) == 200){
    xml_in <- read_xml(query)
    pkg_ids <- xml2::xml_text(
      xml2::xml_find_all(
        xml_in,
        './/document/packageid'
        )
      )
    pkg_titles <- xml2::xml_text(
      xml2::xml_find_all(
        xml_in,
        './/document/title'
      )
    )
  }
  
    
  
  # - Get:
  #   - Package identifiers of listed data packages
  # - Data package titles
  # - Short descriptions of data packages
  # - dataset_summary table
  # - URLs to data package landing pages. Can these be hyperlinked
  # - store in named list of single line data frames
  # 
  # - Get: 
  #   - data product IDs from neon_data_products_for_ecocomDP.txt listed in /inst 
  # - dataset_summary.txt for each data product ID
  # - Create function call to open locally stored HTML (so user can copy and paste into RStudio)
  # - store in named list of single line data frames
  # 
  # - Convert 2 lists into a data frame
  # - Create leading column as ecocomDP_id
  # - Create data frame and export to local workspace from function (this will be used by view_metadata.R).
  # - View data frame in RStudio
  
}