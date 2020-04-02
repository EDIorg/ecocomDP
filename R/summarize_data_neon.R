#' Summarize supported NEON data products
#' 
#' This is a helper function for project maintainers. It summarizes supported
#' NEON data products and saves it as an .rda data object to the /data 
#' directory of the ecocomDP R package, which is called upon by 
#' \code{search_data()} to quickly identify datasets of potential interest.
#'     
#' @examples
#' \dontrun{
#' summarize_data_neon()
#' }
#'         
#' @export
#'
summarize_data_neon <- function() {
  
  message("Creating data_summary_neon.rda")
  
  # Parameterize --------------------------------------------------------------
  # List supported NEON data products
  
  supported_data <- suppressMessages(
    readr::read_tsv(
      system.file("neon_data_products_for_ecocomDP.txt", package = "ecocomDP")))
  
  # Create summary ------------------------------------------------------------
  # A searchable list object for each data product
  
  summary_data_neon <- lapply(
    seq_along(supported_data$id),
    function(x) {
      
      # Get product metadata
      
      message(paste0("Fetching ", supported_data$id[x]))
      r <- neonUtilities::getProductInfo(supported_data$id[x])
      
      # Title
      
      if (!is.null(r$productName)) {
        message("  title")
        title <- r$productName
      } else {
        title <- NA
      }
      
      # Description
      
      if (!is.null(r$productDescription)) {
        message("  description")
        description <- r$productDescription
      } else {
        description <- NA
      }
      
      # Abstract
      
      if (!is.null(r$productAbstract)) {
        message("  abstract")
        abstract <- r$productAbstract
      } else {
        abstract <- NA
      }
      
      # Number of years sampled, standard deviation between sampling events,
      # site locations, and taxa - Download data and create summary statistics
      # of each
      
      if (!is.na(supported_data$master_taxon_list[x])) {
        message(
          paste0(
            "    loading ", 
            supported_data$master_taxon_list[x], 
            " taxon table"))
        taxon_table <- try(
          dplyr::select(
            neonUtilities::getTaxonTable(
              supported_data$master_taxon_list[x]), 
            -taxonTypeCode, -taxonID, -scientificNameAuthorship, -taxonRank, 
            -nameAccordingToID),
          silent = T)
        if (class(taxon_table) != "try-error") {
          # Initialize outputs
          taxa <- vector('list', length(r$siteCodes$siteCode))
          number_of_years_sampled <- vector('list', length(r$siteCodes$siteCode))
          std_dev_interval_betw_years <- vector('list', length(r$siteCodes$siteCode))
          coordinates <- vector('list', length(r$siteCodes$siteCode))
          names(taxa) <- r$siteCodes$siteCode
          names(number_of_years_sampled) <- r$siteCodes$siteCode
          names(std_dev_interval_betw_years) <- r$siteCodes$siteCode
          names(coordinates) <- r$siteCodes$siteCode
          # Summarize data
          o <- lapply(
            names(taxa),
            function(k) {
              message(paste0("    summarizing ", k, " data"))
              # Download data
              d <- neonUtilities::getDatatable(
                dpid = supported_data$id[x],
                sample_location_list = k,
                data_table_name = supported_data$data_table[x])
              if (nrow(d) != 0) {
                # taxa & num.taxa - Get number of unique taxa and their 
                # corresponding rank values collapsed into a comma delimited 
                # character string. Variation in column names requires renaming.
                if (("taxonID" %in% colnames(d)) & 
                    (!("acceptedTaxonID" %in% colnames(d)))) {
                  d <- dplyr::rename(d, acceptedTaxonID = taxonID)
                }
                taxa[[k]] <<- list(
                  unique_taxa = length(unique(d$acceptedTaxonID)),
                  taxa = paste(
                    unname(
                      apply(
                        dplyr::select(taxon_table, -acceptedTaxonID)[
                          match(
                            unique(d$acceptedTaxonID), 
                            taxon_table$acceptedTaxonID), ], 
                        1, 
                        function(x) {
                          paste(x[!is.na(x)], collapse = ", ")
                        })),
                    collapse = ", "))
                # years - Variation in column names requires renaming
                if (!("collectDate" %in% colnames(d)) & 
                    (("endDate" %in% colnames(d)))) {
                  d <- dplyr::rename(d, collectDate = endDate)
                } else if (!("collectDate" %in% colnames(d)) & 
                           (("passStartTime" %in% colnames(d)))) {
                  d <- dplyr::rename(d, collectDate = passStartTime)
                }
                dates <- lubridate::ymd(stringr::str_remove(d$collectDate, "T.*$"))
                dates <- dates[!is.na(dates)]
                number_of_years_sampled[[k]] <<- max(lubridate::year(dates)) - 
                  min(lubridate::year(dates))
                # sd.between.surveys
                std_dev_interval_betw_years[[k]] <<- round(sd(diff(unique(dates))/365), 2)
                # geographic.area
                r <- httr::GET(
                  paste0("https://data.neonscience.org/api/v0/locations/", k))
                if (r$status_code == 200) {
                  rp <- jsonlite::fromJSON(httr::content(r, as = "text"))
                  coordinates[[k]] <<- list(
                    N = rp$data$locationDecimalLatitude,
                    E = rp$data$locationDecimalLongitude,
                    S = rp$data$locationDecimalLatitude,
                    W = rp$data$locationDecimalLongitude)
                }
                # Clean up
                # FIXME: Determine the source of years = 0 and years = NA 
                # upstream of this point, and address this issue there.
                if ((number_of_years_sampled[[k]] == 0) | is.na(number_of_years_sampled[[k]])) {
                  number_of_years_sampled[[k]] <<- 0
                  std_dev_interval_betw_years[[k]] <<- 0
                }
              } else {
                taxa[[k]] <<- NULL
                number_of_years_sampled[[k]] <<- NULL
                std_dev_interval_betw_years[[k]] <<- NULL
                coordinates[[k]] <<- NULL
              }
            })
        } else {
          taxa <- NA
          number_of_years_sampled <- NA
          std_dev_interval_betw_years <- NA
          coordinates <- NA
        }
      } else {
        taxa <- NA
        number_of_years_sampled <- NA
        std_dev_interval_betw_years <- NA
        coordinates <- NA
      }
      message("Done")
      
      # Get URL to data product metadata
      
      url <- paste0(
        "https://data.neonscience.org/data-products/", supported_data$id[x])
      
      # Compile components and return
      
      output <- list(
        source = "NEON",
        title = title,
        description = description,
        abstract = abstract,
        number_of_years_sampled = number_of_years_sampled,
        std_dev_interval_betw_years = std_dev_interval_betw_years,
        coordinates = coordinates,
        taxa = taxa,
        url = url)
      output

    })
  
  names(summary_data_neon) <- supported_data$id
  
  # Write to file -------------------------------------------------------------
  
  usethis::use_data(summary_data_neon, overwrite = T)
  
}