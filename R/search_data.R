#' Search ecocomDP data
#' 
#' Use this function to search across ecocomDP data made available by the 
#' Environmental Data Initiative (EDI) and the National Ecological Observatory
#' Network (NEON).
#' 
#' @param text
#'     (character) Text to search in dataset titles, descriptions, and 
#'     abstracts.
#' @param taxa
#'     (character) Taxanomic rank values to search for. The full taxonomic 
#'     hierarchy of each taxa in the data is searchable. Scientific and 
#'     common names are supported.
#' @param num.taxa
#'     (numeric) Minimum and maximum number of taxa in the data.
#' @param years
#'     (numeric) Minimum and maximum number of years sampled.
#' @param sd.between.surveys
#'     (numeric) Minimum and maximum standard deviation between surveys (in 
#'     years).
#' @param geographic.area
#'     (numeric) Decimal degree North, East, South, and West coordinates within
#'     which the data shoud originate.
#' @param boolean.operator
#'     (character) Boolean operator to use when searching \code{text} and 
#'     \code{taxa}. Supported operators are "AND", "OR".
#'     
#' @note
#'     Searches across input arguments are combined with the "AND" boolean 
#'     operator.
#'     
#' @return 
#'     (data.frame) Search results with these feilds:
#'     \itemize{
#'         \item source - Source from which the data originate. Currently 
#'         supported are "EDI" and "NEON".
#'         \item id - Identifier of dataset
#'         \item title - Title of dataset
#'         \item description - Description of dataset
#'         \item abstract - Abstract of dataset
#'         \item years - Number of years sampled
#'         \item sd_between_samples - Standard deviation between sampling 
#'         events in years.
#'         \item sites - Sites names or abbreviations (returned for NEON data
#'         sources but not EDI)
#'         \item url - URL to dataset
#'     }
#'     
#' @examples
#'         
#' @export
#'
search_data <- function(text, taxa, num.taxa, years, sd.between.surveys, 
                        geographic.area, boolean.operator = "AND") {
  
  message("Searching data ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = "search_data",
    fun.args = as.list(environment()))
  
  # Prepare summary data ------------------------------------------------------
  # Combine summaries of EDI and NEON data. These are created by 
  # summarize_data_edi() and summarize_data_neon() respectively.
  
  d <- c(summary_data_edi, summary_data_neon)
  
  # Initialize an index of available datasets (use_i) for recording successful 
  # search hits, and an index of available sites within each dataset (sites_i) 
  # corresponding with taxa, num.taxa, and geographic.area search arguments. 
  # These are used later to collate the search results. use_i is initialized
  # with logical because it will be used to select the matched datasets.
  # sites_i is initialized with NA_character_ because they will be returned
  # to the user in a data frame.
  
  use_i <- rep(
    list(
      list(
        text = F,
        taxa = F,
        num.taxa = F,
        years = F,
        sd.between.surveys = F,
        geographic.area = F)),
    length(d))
  names(use_i) <- names(d)
  
  sites_i <- rep(
    list(
      list(
        text = NA_character_,
        taxa = NA_character_,
        num.taxa = NA_character_,
        years = NA_character_,
        sd.between.surveys = NA_character_,
        geographic.area = NA_character_)),
    length(d))
  names(sites_i) <- names(d)
  
  # Search --------------------------------------------------------------------
  # Apply user specified search criteria to each dataset while recording 
  # successful hits.
  
  for (i in seq_along(d)) {
    arg_i <- rep(F, length(formals()))
    
    # Search text
    
    if (!missing(text)) {
      if (boolean.operator == "AND") {
        use_i[[i]]$text <- all(
          stringr::str_detect(
            tolower(
              paste(
                d[[i]]$title,
                d[[i]]$description,
                d[[i]]$abstract,
                collapse = ", ")),
            tolower(text)))
      } else if (boolean.operator == "OR") {
        use_i[[i]]$text <- stringr::str_detect(
          tolower(
            paste(
              d[[i]]$title,
              d[[i]]$description,
              d[[i]]$abstract,
              collapse = ", ")),
          tolower(paste(text, collapse = "|")))
      }
      if (isTRUE(use_i[[i]]$text)) {
        sites_i[[i]]$text <- names(d[[i]]$coordinates)
      }
    } else {
      use_i[[i]]$text <- NULL
      sites_i[[i]]$text <- NULL
    }
    
    # Search taxa
    
    if (!missing(taxa)) {
      taxa_i <- rep(F, length(d[[i]]$taxa))
      for (k in 1:length(d[[i]]$taxa)) {
        if (boolean.operator == "AND") {
          taxa_i[k] <- all(
            stringr::str_detect(
              tolower(d[[i]]$taxa[[k]]$taxa),
              tolower(taxa)))
        } else if (boolean.operator == "OR") {
          taxa_i[k] <- stringr::str_detect(
            tolower(d[[i]]$taxa[[k]]$taxa),
            tolower(paste(taxa, collapse = "|")))
        }
      }
      if (any(taxa_i, na.rm = T)) {
        use_i[[i]]$taxa <- T
        sites_i[[i]]$taxa <- names(d[[i]]$taxa)[taxa_i]
      }
    } else {
      use_i[[i]]$taxa <- NULL
      sites_i[[i]]$taxa <- NULL
    }
    
    if (!missing(num.taxa)) {
      num_taxa_i <- rep(F, length(d[[i]]$taxa))
      for (k in 1:length(d[[i]]$taxa)) {
        num_taxa_i[k] <- (d[[i]]$taxa[[k]]$unique_taxa >= num.taxa[1]) & 
          (d[[i]]$taxa[[k]]$unique_taxa <= num.taxa[2])
      }
      if (any(num_taxa_i, na.rm = T)) {
        use_i[[i]]$num.taxa <- T
        sites_i[[i]]$num.taxa <- names(d[[i]]$taxa)[num_taxa_i]
      }
    } else {
      use_i[[i]]$num.taxa <- NULL
      sites_i[[i]]$num.taxa <- NULL
    }
    
    # Search temporal coverage
    
    if (!missing(years)) {
      years_i <- (unname(unlist(d[[i]]$number_of_years_sampled)) >= years[1]) & 
        (unname(unlist(d[[i]]$number_of_years_sampled)) <= years[2])
      if (any(years_i, na.rm = T)) {
        use_i[[i]]$years <- T
        sites_i[[i]]$years <- names(d[[i]]$number_of_years_sampled)[years_i]
      }
    } else {
      use_i[[i]]$years <- NULL
      sites_i[[i]]$years <- NULL
    }

    if (!missing(sd.between.surveys)) {
      sdyears_i <- (unname(unlist(d[[i]]$std_dev_interval_betw_years)) >= sd.between.surveys[1]) & 
        (unname(unlist(d[[i]]$std_dev_interval_betw_years)) <= sd.between.surveys[2])
      if (any(sdyears_i, na.rm = T)) {
        use_i[[i]]$sd.between.surveys <- T
        sites_i[[i]]$sd.between.surveys <- names(d[[i]]$std_dev_interval_betw_years)[sdyears_i]
      }
    } else {
      use_i[[i]]$sd.between.surveys <- NULL
      sites_i[[i]]$sd.between.surveys <- NULL
    }
    
    # Search geographic coverage - Methods support point locations (location 
    # falls within the area defined by geographic.area) and areas (overlap 
    # between location area and the area defined by geographic.area).
    # FIXME: Add support for sourthern and western hemispheres
    
    if (!missing(geographic.area)) {
      geographic_area_i <- rep(F, length(d[[i]]$coordinates))
      for (k in 1:length(d[[i]]$coordinates)) {
        if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 2) {
          geographic_area_i[k] <- 
            (d[[i]]$coordinates[[k]]$N <= geographic.area[1]) & 
            (d[[i]]$coordinates[[k]]$S >= geographic.area[3]) & 
            (d[[i]]$coordinates[[k]]$E <= geographic.area[2]) & 
            (d[[i]]$coordinates[[k]]$W >= geographic.area[4])
        } else if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 4) {
          geographic_area_i[k] <- 
            (d[[i]]$coordinates[[k]]$N <= geographic.area[1]) & (d[[i]]$coordinates[[k]]$N >= geographic.area[3]) |
            (d[[i]]$coordinates[[k]]$S <= geographic.area[1]) & (d[[i]]$coordinates[[k]]$S >= geographic.area[3]) |
            (d[[i]]$coordinates[[k]]$W >= geographic.area[4]) & (d[[i]]$coordinates[[k]]$W <= geographic.area[2]) |
            (d[[i]]$coordinates[[k]]$E >= geographic.area[4]) & (d[[i]]$coordinates[[k]]$E <= geographic.area[2])
            
        }
      }
      if (any(geographic_area_i, na.rm = T)) {
        use_i[[i]]$geographic.area <- T
        sites_i[[i]]$geographic.area <- names(d[[i]]$coordinates)[geographic_area_i]
      }
    } else {
      use_i[[i]]$geographic.area <- NULL
      sites_i[[i]]$geographic.area <- NULL
    }
    
    # If no arguments are specified, then return the full list of sites for 
    # each dataset
    
    if (missing(text) & missing(taxa) & missing(num.taxa) & missing(years) & 
        missing(sd.between.surveys) & missing(geographic.area)) {
      sites_i <- lapply(
        names(d),
        function(x) {
          list(all_sites = names(d[[x]]$coordinates))
        })
      names(sites_i) <- names(d)
    }
    
    # Indicate whether all search parameters were met
    
    use_i[i] <- unlist(
      lapply(
        use_i[i],
        function(x) {
          all(unname(unlist(x)))
        }))
    
  }
  
  # Return results ------------------------------------------------------------
  
  d <- d[unname(unlist(use_i))]
  sites_i <- sites_i[unname(unlist(use_i))]
  output <- data.table::rbindlist(
    lapply(
      names(d),
      function(x) {
        # sites - EDI data are inconsistent in the representation of "sites"
        # (in contrast to NEON) therefore these values are set to NA
        if (d[[x]]$source == "EDI") {
          sites <- NA_character_
        } else {
          sites <- paste(Reduce(intersect, sites_i[[x]]), collapse = ",")
        }
        # years - Report as a single value (EDI) or range (NEON)
        if (length(d[[x]]$number_of_years_sampled) == 1) {
          years <- unlist(d[[x]]$number_of_years_sampled)
        } else {
          years <- paste0(
            "min = ", min(unlist(d[[x]]$number_of_years_sampled)), ", ",
            "max = ", max(unlist(d[[x]]$number_of_years_sampled)))
        }
        # sampling_interval - Report as a single value (EDI) or range (NEON)
        if (length(d[[x]]$std_dev_interval_betw_years) == 1) {
          sampling_interval <- round(unlist(d[[x]]$std_dev_interval_betw_years), 2)
        } else {
          sampling_interval <- paste0(
            "min = ", min(unlist(d[[x]]$std_dev_interval_betw_years)), ", ",
            "max = ", max(unlist(d[[x]]$std_dev_interval_betw_years)))
        }
        # Return
         list(
          source = d[[x]]$source,
          id = x,
          title = d[[x]]$title,
          description = d[[x]]$description,
          abstract = d[[x]]$abstract,
          years = years,
          sampling_interval = sampling_interval,
          sites = sites,
          url = d[[x]]$url)
      }))
  
  if (nrow(output) == 0) {
    output <- "No results found."
  } else {
    output
  }

}