#' View a summary of all ecocomDP data packages
#'
#' @description  
#'     View a summary of all ecocomDP data packages.
#'
#' @usage view_all_ecocomDP()
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
#' @export
#'

view_all_ecocomDP <- function(){
  
  message('Looking for ecocomDP datasets')

  # Search EDI ----------------------------------------------------------------
  
  message('Searching EDI ...')
  
  query <- httr::GET('https://pasta.lternet.edu/package/search/eml?q=keyword:ecocomDP&fl=packageid,title&rows=1000')
  
  # If HTTP request is successful ...
  
  if (status_code(query) == 200){
    
    # Parse query
    
    xml_in <- xml2::read_xml(query)
    
    # Read package IDs
    
    pkg_ids <- xml2::xml_text(
      xml2::xml_find_all(
        xml_in,
        './/document/packageid'
      )
    )
    
    # Read package titles
    
    pkg_titles <- xml2::xml_text(
      xml2::xml_find_all(
        xml_in,
        './/document/title'
      )
    )
    
    # Read observation table
    
    edi_data <- lapply(pkg_ids, read_table_dataset_summary)
    
    message('Done')

  # If HTTP request fails ...
    
  } else {
    
    warning('EDI cannot be reached at this time. Please try your call again later')
    
  }

  # Search NEON ---------------------------------------------------------------
  # NEON data products are read into R and converted to ecocomDP on demand.
  # Available products are manually listed in 
  # "neon_data_products_for_ecocomDP.txt" and the EML manually created and 
  # stored in the ecocomDP R package.
  
  message('Searching NEON ...')
  
  # Get package IDs and titles
  
  criteria <- suppressWarnings(
    read.table(
      system.file(
        'neon_data_products_for_ecocomDP.txt', 
        package = 'ecocomDP'),
      header = T,
      sep = "\t",
      as.is = T,
      na.strings = "NA"
    )
  )
  
  neon_pkg_ids <- criteria$data_product_id[
    is.na(criteria$tables_to_download)
  ]
  
  neon_pkg_titles <- criteria$title[
    is.na(criteria$tables_to_download)
  ]
  
  # Get dataset_summary tables
  
  neon_data <- lapply(neon_pkg_ids, read_table_dataset_summary_neon)
  
  message('Done')
  
  # Aggregate dataset_summary tables ------------------------------------------
  
  message('Summarizing data ...')
  
  # Bind EDI and NEON data and add columns
  
  output <- dplyr::bind_rows(
    c(edi_data, neon_data)
  )
  
  output <- dplyr::select(
    output,
    -package_id
  )
  
  output <- tibble::add_column(
    output,
    package_id = c(pkg_ids, neon_pkg_ids),
    title = c(pkg_titles, neon_pkg_titles),
    .before = 'original_package_id'
  )
  
  # Return
  
  View(output)
  
  output
 
}




#' Read the ecocomDP dataset_summary table (from EDI)
#'
#' @description  
#'     Use this function to read the dataset_summary table of an ecocomDP data 
#'     package from the EDI Data Repository.
#'
#' @usage
#'     read_table_dataset_summary(
#'       package.id
#'     )
#' 
#' @param package.id
#'     (character) Data package identifier of an ecocomDP dataset in the EDI
#'     data repository.
#'     
#' @return 
#'     (tibble) The dataset_summary table
#'         
#' @export
#'

read_table_dataset_summary <- function(package.id){
  
  message('Found ', package.id)
  
  # Read EML
  
  eml <- suppressMessages(
    EDIutils::api_read_metadata(package.id)
  )
  
  # Get entity names
  
  entity_name <- xml2::xml_text(
    xml2::xml_find_all(
      eml,
      './/dataset/dataTable/physical/objectName'
    )
  )
  
  # If the dataset_summary table is present (this is expected) ...
  
  if (any(stringr::str_detect(entity_name, 'dataset_summary'))){
    
    # Get field delimiter
    
    entity_delimiter <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter'
      )
    )[stringr::str_detect(entity_name, 'dataset_summary')]
    
    # Get entity url
    
    entity_url <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/distribution/online/url'
      )
    )[stringr::str_detect(entity_name, 'dataset_summary')]
    
    # Read dataset_summary table
    
    if (entity_delimiter == ','){
      
      output <-readr::read_csv(
        entity_url, 
        col_types = c(
          package_id = col_character(),
          original_package_id = col_character(),
          length_of_survey_years = col_double(),
          number_of_years_sampled = col_double(),
          std_dev_interval_betw_years = col_double(),
          max_num_taxa = col_integer(),
          geo_extent_bounding_box_m2 = col_double()
        )
      )
      
    } else if (entity_delimiter == '\\t'){
      
      output <-readr::read_tsv(
        entity_url, 
        col_types = c(
          package_id = col_character(),
          original_package_id = col_character(),
          length_of_survey_years = col_double(),
          number_of_years_sampled = col_double(),
          std_dev_interval_betw_years = col_double(),
          max_num_taxa = col_integer(),
          geo_extent_bounding_box_m2 = col_double()
        )
      )
      
    }
    
  # If the dataset_summary table is missing ...
    
  } else {
    
    output <- NULL
    
  }
  
  # Return
  
  output
  
}




#' Read the ecocomDP dataset_summary table (from NEON)
#'
#' @description  
#'     Use this function to read the dataset_summary table of an ecocomDP data 
#'     package from the NEON Data Portal.
#'
#' @usage
#'     read_table_dataset_summary_neon(
#'       package.id
#'     )
#' 
#' @param package.id
#'     (character) Data package identifier of an ecocomDP dataset in the EDI
#'     data repository.
#'     
#' @return 
#'     (tibble) The dataset_summary table
#'         
#' @export
#'

read_table_dataset_summary_neon <- function(package.id){
  
  message(paste0('Found ', package.id))
  
  # Locate dataset_summary table in the ecocomDP package
  
  use_i <- stringr::str_detect(
    list.files(
      system.file(
        package = 'ecocomDP'
      )
    ),
    pattern = paste0(
      package.id,
      '_neon_dataset_summary'
    )
  )
  
  # Get the field delimiter
    
  entity_delimiter <- EDIutils::detect_delimeter(
    path = file.path(system.file(package = 'ecocomDP')),
    data.files = list.files(system.file(package = 'ecocomDP'))[use_i],
    os = EDIutils::detect_os()
  )
  
  # Read the table
  
  if (entity_delimiter == ','){
    
    output <-readr::read_csv(
      list.files(
        system.file(
          package = 'ecocomDP'
        ),
        full.names = TRUE
      )[use_i], 
      col_types = c(
        package_id = col_character(),
        original_package_id = col_character(),
        length_of_survey_years = col_double(),
        number_of_years_sampled = col_double(),
        std_dev_interval_betw_years = col_double(),
        max_num_taxa = col_integer(),
        geo_extent_bounding_box_m2 = col_double()
      )
    )
    
  } else if (entity_delimiter == '\t'){
    
    output <-readr::read_tsv(
      list.files(
        system.file(
          package = 'ecocomDP'
        ),
        full.names = TRUE
      )[use_i], 
      col_types = c(
        package_id = col_character(),
        original_package_id = col_character(),
        length_of_survey_years = col_double(),
        number_of_years_sampled = col_double(),
        std_dev_interval_betw_years = col_double(),
        max_num_taxa = col_integer(),
        geo_extent_bounding_box_m2 = col_double()
      )
    )
    
  }
  
  # Return
  
  output
  
}
