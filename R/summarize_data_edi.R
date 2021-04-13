#' Summarize supported EDI data packages
#' 
#' @param path (character) Path the file will be saved to
#' 
#' This is a helper function for project maintainers. It summarizes supported
#' EDI data packages and saves it as an .rda data object to the /data 
#' directory of the ecocomDP R package, which is called upon by 
#' \code{search_data()} to quickly identify datasets of potential interest.
#'     
#' @examples
#' \dontrun{
#' summarize_data_edi(path)
#' }
#'         
summarize_data_edi <- function(path) {
  
  message("Creating data_summary_edi.rda")
  
  # Parameterize --------------------------------------------------------------
  
  # List EDI data packages containing the keyword "ecocomDP" - Only ecocomDP 
  # data packages should have this keyword.
  
  # TODO: Make sure this doesn't return the archived search index data package
  supported_data <- httr::GET(
    "https://pasta.lternet.edu/package/search/eml?q=keyword:ecocomDP&fl=packageid,title&rows=1000")
  if (httr::status_code(supported_data) != 200) {
    stop("EDI cannot be reached at this time. Please try your call again later.", 
         call. = F)
  }
  id <- xml2::xml_text(
    xml2::xml_find_all(
      xml2::read_xml(supported_data), ".//document/packageid"))
  
  # Create summary ------------------------------------------------------------
  # A searchable list object for each data package
  
  summary_data_edi <- lapply(
    seq_along(id),
    function(x) {
      
      # Get package metadata
      
      message(paste0("Fetching ", id[x]))
      r <- api_read_metadata(id[x])
      
      if (all(class(r) %in% c("xml_node", "xml_document"))) {
        
        # Title
        
        message("  title")
        title <- xml2::xml_text(
          xml2::xml_find_first(r, "./dataset/title"))
        
        # Description - EML doesn't have the equivalent of a NEON data product
        # description.
        
        description <- NA
        
        # Abstract
        
        message("  abstract")
        abstract <- xml2::xml_text(
          xml2::xml_find_first(r, "./dataset/abstract"))
        
        # Calculate number of years sampled and standard deviation interval 
        # between years. This information is stored in the dataset_summary
        # table.
        
        dataset_summary <- read_table_dataset_summary(id[x])
        message("  number of years sampled")
        number_of_years_sampled <- vector('list', 1)
        names(number_of_years_sampled) <- "geographic_bounds"
        number_of_years_sampled[[1]] <- dataset_summary$number_of_years_sampled
        message("  standard deviation interval between years")
        std_dev_interval_betw_years <- vector('list', 1)
        names(std_dev_interval_betw_years) <- "geographic_bounds"
        std_dev_interval_betw_years[[1]] <- dataset_summary$std_dev_interval_betw_years
        
        # Site coordinates
        
        message("  coordinates")
        geocov <- xml2::xml_find_all(r, "./dataset/coverage/geographicCoverage")
        if (length(geocov) != 0) {
          coordinates <- vector('list', 1)
          names(coordinates) <- "geographic_bounds"
          # FIXME: Support southern and eastern hemispheres
          coordinates[[1]] <- list(
            N = max(
              as.numeric(
                xml2::xml_text(
                  xml2::xml_find_all(
                    geocov, "./boundingCoordinates/northBoundingCoordinate")))),
            E = max(
              as.numeric(
                xml2::xml_text(
                  xml2::xml_find_all(
                    geocov, "./boundingCoordinates/eastBoundingCoordinate")))),
            S = min(
              as.numeric(
                xml2::xml_text(
                  xml2::xml_find_all(
                    geocov, "./boundingCoordinates/southBoundingCoordinate")))),
            W = min(
              as.numeric(
                xml2::xml_text(
                  xml2::xml_find_all(
                    geocov, "./boundingCoordinates/westBoundingCoordinate")))))
        } else {
          coordinates <- NA
        }
        
        # Get number of unique taxa and their corresponding rank values 
        # collapsed into a comma delimited character string
        
        message("  taxa")
        taxcla <- xml2::xml_find_all(
          r, "./dataset/coverage/taxonomicCoverage/taxonomicClassification")
        if (length(taxcla) != 0) {
          taxa <- vector('list', 1)
          names(taxa) <- "geographic_bounds"
          taxa[[1]] <- list(
            unique_taxa = dataset_summary$max_num_taxa,
            taxa = paste(
              unique(
                xml2::xml_text(
                  xml2::xml_find_all(
                    taxcla, ".//taxonRankValue"))),
              collapse = ","))
        } else {
          taxa <- vector('list', 1)
          names(taxa) <- "geographic_bounds"
          taxa[[1]] <- list(
            unique_taxa = NA,
            taxa = NA_character_)
        }
        message("Done")
        
        # Get URL to data package metadata
        
        url <- paste0(
          "https://portal.edirepository.org/nis/mapbrowse?packageid=", id[x])
        
        # Compile components and return
        
        output <- list(
          source = "EDI",
          title = title,
          description = description,
          abstract = abstract,
          number_of_years_sampled = number_of_years_sampled,
          std_dev_interval_betw_years = std_dev_interval_betw_years,
          coordinates = coordinates,
          taxa = taxa,
          url = url)
        output

      }
      
    })
  
  names(summary_data_edi) <- id
  
  # Write to file -------------------------------------------------------------
  
  save(summary_data_edi, file = paste0(path, "/summary_data_edi.rda"), 
       version = 3)
  
}


#' Read the ecocomDP dataset_summary table (from EDI)
#'
#' @description  
#'     Use this function to read the dataset_summary table of an ecocomDP data 
#'     package from the EDI Data Repository.
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
  
  message("Reading dataset_summary table for ", package.id)
  
  eml <- suppressMessages(api_read_metadata(package.id))
  entity_name <- xml2::xml_text(
    xml2::xml_find_all(eml, './/dataset/dataTable/physical/objectName'))
  if (any(stringr::str_detect(entity_name, 'dataset_summary'))) {
    entity_delimiter <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter'))[
          stringr::str_detect(entity_name, 'dataset_summary')]
    entity_url <- xml2::xml_text(
      xml2::xml_find_all(
        eml,
        './/dataset/dataTable/physical/distribution/online/url'))[
          stringr::str_detect(entity_name, 'dataset_summary')]
    if (entity_delimiter == ',') {
      output <-data.table::fread(file = entity_url)
    } else if (entity_delimiter == '\\t') {
      output <-data.table::fread(file = entity_url)
    }
  } else {
    output <- NULL
  }
  output
  
}