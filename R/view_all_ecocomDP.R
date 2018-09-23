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
  
  message('Searching EDI ...')
  
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
  
  # Get package EML
  
  get_eml <- function(package.id){
    message(paste0('found ', package.id))
    pkg_prts <- unlist(strsplit(package.id, split = ".", fixed = T))
    metadata <- XML::xmlParse(paste0("http://pasta.lternet.edu/package/metadata/eml",
                                    "/",
                                    pkg_prts[1],
                                    "/",
                                    pkg_prts[2],
                                    "/",
                                    pkg_prts[3]))
    metadata
  }
  eml <- lapply(pkg_ids, FUN = get_eml)
  
  # Get dataset observation tables
  
  get_dataset_observation <- function(package.id, eml){
    entity_names <- unlist(
      xmlApply(eml["//dataset/dataTable/entityName"], 
               xmlValue)
    )
    entity_delimiters <- unlist(
      xmlApply(eml["//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"], 
               xmlValue)
    )
    entity_urls <- unlist(
      xmlApply(eml["//dataset/dataTable/physical/distribution/online/url"], 
               xmlValue)
    )
    if (length(grep('dataset_summary', entity_names)) != 0){
      file_name <- entity_names[grep('dataset_summary', entity_names)]
      file_delimiter <- entity_delimiters[grep('dataset_summary', entity_names)]
      file_url <- entity_urls[grep('dataset_summary', entity_names)]
      
      
      if (file_delimiter == ","){
        file_data <-read.table(sub("^https", "http", file_url),
                                      header = T, 
                                      sep=",",
                                      as.is = T,
                                      fill = T)
      } else if (file_delimiter == "\\t"){
        file_data <-read.table(sub("^https", "http", file_url),
                                      header = T, 
                                      sep="\t",
                                      as.is = T,
                                      fill = T)
      }
    } else {
      file_data <- NULL
    }
    list(file_data)
  }
  message('summarizing EDI data')
  edi_data <- mapply(get_dataset_observation, pkg_ids, eml)
  
  message('... done.')
  
  # Get available NEON data products  -----------------------------------------
  
  message('Searching NEON ...')
  
  # Get package IDs and titles
  
  criteria <- suppressWarnings(read.table(
    system.file('neon_data_products_for_ecocomDP.txt', package = 'ecocomDP'),
    header = T,
    sep = "\t",
    as.is = T,
    na.strings = "NA"))
  
  neon_pkg_ids <- criteria$data_product_id[is.na(criteria$tables_to_download)]
  neon_pkg_titles <- criteria$title[is.na(criteria$tables_to_download)]
  
  # Get dataset observation tables
  
  get_dataset_observation_neon <- function(package.id){
    message(paste0('found ', package.id))
    use_i <- grep(
      pattern = paste0(
        package.id,
        '_neon_dataset_summary'
        ),
      x = list.files(
        system.file(
          package = 'ecocomDP'
          )
        )
      )
    
    if ((length(use_i) != 0)){
      file_delimiter <- detect_delimeter(
        path = file.path(system.file(package = 'ecocomDP')),
        data.files = list.files(system.file(package = 'ecocomDP'))[use_i],
        os = detect_os())
      if (file_delimiter == ","){
        file_data <-read.table(paste0(file.path(system.file(package = 'ecocomDP')),
                                      '/',
                                      list.files(system.file(package = 'ecocomDP'))[use_i]),
                               header = T, 
                               sep=",",
                               as.is = T,
                               fill = T)
      } else if (file_delimiter == "\t"){
        file_data <-read.table(paste0(file.path(system.file(package = 'ecocomDP')),
                                      '/',
                                      list.files(system.file(package = 'ecocomDP'))[use_i]),
                               header = T, 
                               sep="\t",
                               as.is = T,
                               fill = T)
      }
    }
    file_data
  }
  neon_data <- lapply(neon_pkg_ids, get_dataset_observation_neon)
  message('summarizing NEON data')
  
  message('... done.')
  
  # Compile EDI and NEON data then view ---------------------------------------
  
  message('Compiling EDI and NEON data ...')
  
  rows <- length(edi_data) + length(neon_data)
  data_out <- data.frame(
    package_id = character(rows),
    title = character(rows),
    original_package_id = character(rows),
    length_of_survey_years = numeric(rows),
    number_of_years_sampled = numeric(rows),
    std_dev_interval_betw_years = numeric(rows),
    max_num_taxa = numeric(rows),
    geo_extent_bounding_box_m2 = numeric(rows),
    stringsAsFactors = F)
  
  for (i in 1:length(edi_data)){
    data_out[i, 'package_id'] <- names(edi_data[i])
    data_out[i, 'title'] <- pkg_titles[i]
    data_out[i, 'original_package_id'] <- edi_data[[i]][1, 'original_package_id']
    data_out[i, 'length_of_survey_years'] <- edi_data[[i]][1, 'length_of_survey_years']
    data_out[i, 'number_of_years_sampled'] <- edi_data[[i]][1, 'number_of_years_sampled']
    data_out[i, 'std_dev_interval_betw_years'] <- edi_data[[i]][1, 'std_dev_interval_betw_years']
    data_out[i, 'max_num_taxa'] <- edi_data[[i]][1, 'max_num_taxa']
    data_out[i, 'geo_extent_bounding_box_m2'] <- edi_data[[i]][1, 'geo_extent_bounding_box_m2']
  }
  
  j <- i + 1
  for (i in 1:length(neon_data)){
    data_out[j, 'package_id'] <- neon_pkg_ids[i]
    data_out[j, 'title'] <- neon_pkg_titles[i]
    data_out[j, 'original_package_id'] <- neon_data[[i]][1, 'original_package_id']
    data_out[j, 'length_of_survey_years'] <- neon_data[[i]][1, 'length_of_survey_years']
    data_out[j, 'number_of_years_sampled'] <- neon_data[[i]][1, 'number_of_years_sampled']
    data_out[j, 'std_dev_interval_betw_years'] <- neon_data[[i]][1, 'std_dev_interval_betw_years']
    data_out[j, 'max_num_taxa'] <- neon_data[[i]][1, 'max_num_taxa']
    data_out[j, 'geo_extent_bounding_box_m2'] <- neon_data[[i]][1, 'geo_extent_bounding_box_m2']
  }
  
  View(data_out)
  data_out
 
}