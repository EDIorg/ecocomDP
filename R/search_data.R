#' Search published data
#' 
#' @param text (character) Text to search for in dataset titles, descriptions, and abstracts. Datasets matching any exact words or phrase will be returned. Can be a regular expression as used by \code{stringr::str_detect()}. Is not case sensitive. Works with \code{boolean}.
#' @param taxa (character) Taxonomic names to search on. To effectively search the taxonomic tree, it is advisable to start with specific taxonomic names and then gradually broaden the search to higher rank levels when needed. For instance, if searching for "Astragalus gracilis" (species) doesn't produce any results, try expanding the search to "Astragalus" (Genus), "Fabaceae" (Family), and so on. This approach accounts for variations in organism identification, ensuring a more comprehensive exploration of the taxonomic hierarchy.
#' @param num_taxa (numeric) Minimum and maximum number of taxa the dataset should contain. Any datasets within this range will be returned.
#' @param num_years (numeric) Minimum and maximum number of years sampled the dataset should contain. Any datasets within this range will be returned.
#' @param sd_years (numeric) Minimum and maximum standard deviation between survey dates (in years). Any datasets within this range will be returned.
#' @param area (numeric) Bounding coordinates within which the data should originate. Accepted values are in decimal degrees and in the order: North, East, South, West. Any datasets with overlapping areas or contained points will be returned.
#' @param boolean (character) Boolean operator to use when searching \code{text} and \code{taxa}. Supported operators are: "AND", "OR". Default is "AND". Note, other parameters used in a search are combined with an implicit "AND".
#'     
#' @note This function may not work between 01:00 - 03:00 UTC on Wednesdays due to regular maintenance of the EDI Data Repository.
#'     
#' @return (tbl_df, tbl, data.frame) Search results with these feilds:
#'     \itemize{
#'         \item source - Source from which the dataset originates. Currently supported are "EDI" and "NEON".
#'         \item id - Identifier of the dataset.
#'         \item title - Title of the dataset.
#'         \item description - Description of dataset. Only returned for NEON datasets.
#'         \item abstract - Abstract of dataset.
#'         \item years - Number of years sampled.
#'         \item sampling_interval - Standard deviation between sampling events in years.
#'         \item sites - Sites names or abbreviations. Only returned for NEON datasets.
#'         \item url - URL to dataset.
#'         \item source_id - Identifier of source L0 dataset.
#'         \item source_id_url - URL to source L0 dataset.
#'     }
#'     
#' @details Currently, to accommodate multiple L1 versions of NEON data products, search results for a NEON L0 will also list all the L1 versions available for the match. This method is based on the assumption that the summary data among L1 versions is the same, which may need to be addressed in the future. A list of L0 and corresponding L1 identifiers are listed in /inst/L1_versions.txt. Each L1 version is accompanied by qualifying text that's appended to the title, abstract, and descriptions for comprehension of the differences among L1 versions.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Empty search returns all available datasets
#' search_data()
#' 
#' # "text" searches titles, descriptions, and abstracts
#' search_data(text = "Lake")
#' 
#' # "taxa" searches taxonomic ranks for a match
#' search_data(taxa = "Plantae")
#' 
#' # "num_years" searches the number of years sampled
#' search_data(num_years = c(10, 20))
#' 
#' # Use any combination of search fields to find the data you're looking for
#' search_data(
#'   text = c("Lake", "River"),
#'   taxa = c("Plantae", "Animalia"),
#'   num_taxa = c(0, 10),
#'   num_years = c(10, 100),
#'   sd_years = c(.01, 100),
#'   area = c(47.1, -86.7, 42.5, -92),
#'   boolean = "OR")
#' }
#'
search_data <- function(text, taxa, num_taxa, num_years, sd_years, 
                        area, boolean = "AND") {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = "search_data",
    fun.args = as.list(environment()))
  
  ping_edi() # Warn if EDI is down

  # Prepare summary data ------------------------------------------------------
  # Combine summaries of EDI and NEON data. These are created by 
  # summarize_data_edi() and summarize_data_neon() respectively.
  
  # Download this object once per session and save to tempdir() for future calls
  if ("ecocomDP_search_index.rda" %in% dir(tempdir())) {
    load(paste0(tempdir(), "/ecocomDP_search_index.rda"))
    d <- ecocomDP_search_index
  } else {
    newrev <- suppressMessages(api_list_data_package_revisions("edi", "759", filter = "newest"))
    objurls <- suppressMessages(api_read_data_package(paste0("edi.759.", newrev)))
    objurls <- stringr::str_subset(objurls, "/data/")
    objids <- stringr::str_extract(objurls, "(?<=/)[:alnum:]+$")
    objnames <- suppressMessages(
      lapply(objids, api_read_data_entity_name, package.id = paste0("edi.759.", newrev)))
    objnames <- unlist(objnames)
    isdata <- !stringr::str_detect(objnames, "Function")
    objurls <- objurls[isdata]
    for (objurl in objurls) {
      load(url(objurl))
    }
    ecocomDP_search_index <- c(summary_data_edi, summary_data_neon)
    save(ecocomDP_search_index, 
         file = paste0(tempdir(), "/ecocomDP_search_index.rda"), 
         version = 3)
    d <- ecocomDP_search_index
  }
  
  d <- update_neon_summary_info(d) # Adds some metadata and facilitates multiple L1 versions from a single L0
  
  # Initialize an index of available datasets (use_i) for recording successful 
  # search hits, and an index of available sites within each dataset (sites_i) 
  # corresponding with taxa, num_taxa, and area search arguments. 
  # These are used later to collate the search results. use_i is initialized
  # with logical because it will be used to select the matched datasets.
  # sites_i is initialized with NA_character_ because they will be returned
  # to the user in a data frame.
  
  use_i <- rep(
    list(
      list(
        text = F,
        taxa = F,
        num_taxa = F,
        num_years = F,
        sd_years = F,
        area = F)),
    length(d))
  names(use_i) <- names(d)
  
  sites_i <- rep(
    list(
      list(
        text = NA_character_,
        taxa = NA_character_,
        num_taxa = NA_character_,
        num_years = NA_character_,
        sd_years = NA_character_,
        area = NA_character_)),
    length(d))
  names(sites_i) <- names(d)
  
  # Search --------------------------------------------------------------------
  # Apply user specified search criteria to each dataset while recording 
  # successful hits.
  
  for (i in seq_along(d)) {
    arg_i <- rep(F, length(formals()))
    
    # Search text
    
    if (!missing(text)) {
      if (boolean == "AND") {
        use_i[[i]]$text <- all(
          stringr::str_detect(
            tolower(
              paste(
                d[[i]]$title,
                d[[i]]$description,
                d[[i]]$abstract,
                collapse = ", ")),
            tolower(text)))
      } else if (boolean == "OR") {
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
        if (boolean == "AND") {
          taxa_i[k] <- try(
            all(
              stringr::str_detect(
                tolower(d[[i]]$taxa[[k]]$taxa),
                tolower(taxa))), 
            silent = TRUE)
          if (methods::is(taxa_i[k], "try-error")) {
          # if (class(taxa_i[k]) == "try-error") {
            taxa_i[k] <- FALSE
          }
        } else if (boolean == "OR") {
          taxa_i[k] <- try(
            stringr::str_detect(
              tolower(d[[i]]$taxa[[k]]$taxa),
              tolower(paste(taxa, collapse = "|"))),
            silent = TRUE)
          if (methods::is(taxa_i[k], "try-error")) {
            taxa_i[k] <- FALSE
          }
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

    if (!missing(num_taxa)) {
      num_taxa_i <- rep(F, length(d[[i]]$taxa))
      for (k in 1:length(d[[i]]$taxa)) {
        num_taxa_i[k] <- (d[[i]]$taxa[[k]]$unique_taxa >= num_taxa[1]) & 
          (d[[i]]$taxa[[k]]$unique_taxa <= num_taxa[2])
      }
      if (any(num_taxa_i, na.rm = T)) {
        use_i[[i]]$num_taxa <- T
        sites_i[[i]]$num_taxa <- names(d[[i]]$taxa)[num_taxa_i]
      }
    } else {
      use_i[[i]]$num_taxa <- NULL
      sites_i[[i]]$num_taxa <- NULL
    }
    
    # Search temporal coverage
    
    if (!missing(num_years)) {
      years_i <- (unname(unlist(d[[i]]$number_of_years_sampled)) >= num_years[1]) & 
        (unname(unlist(d[[i]]$number_of_years_sampled)) <= num_years[2])
      if (any(years_i, na.rm = T)) {
        use_i[[i]]$num_years <- T
        sites_i[[i]]$num_years <- names(d[[i]]$number_of_years_sampled)[years_i]
      }
    } else {
      use_i[[i]]$num_years <- NULL
      sites_i[[i]]$num_years <- NULL
    }

    if (!missing(sd_years)) {
      sdyears_i <- (unname(unlist(d[[i]]$std_dev_interval_betw_years)) >= sd_years[1]) & 
        (unname(unlist(d[[i]]$std_dev_interval_betw_years)) <= sd_years[2])
      if (any(sdyears_i, na.rm = T)) {
        use_i[[i]]$sd_years <- T
        sites_i[[i]]$sd_years <- names(d[[i]]$std_dev_interval_betw_years)[sdyears_i]
      }
    } else {
      use_i[[i]]$sd_years <- NULL
      sites_i[[i]]$sd_years <- NULL
    }
    
    # Search geographic coverage - Methods support point locations (location 
    # falls within the area defined by area) and areas (overlap 
    # between location area and the area defined by area).
    
    if (!missing(area)) {
      geographic_area_i <- rep(F, length(d[[i]]$coordinates))
      for (k in 1:length(d[[i]]$coordinates)) {
        if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 2) {
          geographic_area_i[k] <- 
            (d[[i]]$coordinates[[k]]$N <= area[1]) & 
            (d[[i]]$coordinates[[k]]$S >= area[3]) & 
            (d[[i]]$coordinates[[k]]$E <= area[2]) & 
            (d[[i]]$coordinates[[k]]$W >= area[4])
        } else if (length(unique(unlist(d[[i]]$coordinates[[k]]))) == 4) {
          geographic_area_i[k] <- 
            (d[[i]]$coordinates[[k]]$N <= area[1]) & (d[[i]]$coordinates[[k]]$N >= area[3]) |
            (d[[i]]$coordinates[[k]]$S <= area[1]) & (d[[i]]$coordinates[[k]]$S >= area[3]) |
            (d[[i]]$coordinates[[k]]$W >= area[4]) & (d[[i]]$coordinates[[k]]$W <= area[2]) |
            (d[[i]]$coordinates[[k]]$E >= area[4]) & (d[[i]]$coordinates[[k]]$E <= area[2])
            
        }
      }
      if (any(geographic_area_i, na.rm = T)) {
        use_i[[i]]$area <- T
        sites_i[[i]]$area <- names(d[[i]]$coordinates)[geographic_area_i]
      }
    } else {
      use_i[[i]]$area <- NULL
      sites_i[[i]]$area <- NULL
    }
    
    # If no arguments are specified, then return the full list of sites for 
    # each dataset
    
    if (missing(text) & missing(taxa) & missing(num_taxa) & missing(num_years) & 
        missing(sd_years) & missing(area)) {
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
        # num_years - Report as a single value (EDI) or range (NEON)
        if (length(d[[x]]$number_of_years_sampled) == 1) {
          num_years <- unlist(d[[x]]$number_of_years_sampled)
        } else {
          num_years <- paste0(
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
        # source_id
        if (!is.null(d[[x]]$source_id)) {
          source_id <- d[[x]]$source_id
        } else {
          source_id <- NA_character_
        }
        # source_id_url
        if (!is.null(d[[x]]$source_id_url)) {
          source_id_url <- d[[x]]$source_id_url
        } else {
          source_id_url <- NA_character_
        }
        # Return
         list(
          source = d[[x]]$source,
          id = x,
          title = d[[x]]$title,
          description = d[[x]]$description,
          abstract = d[[x]]$abstract,
          years = num_years,
          sampling_interval = sampling_interval,
          sites = sites,
          url = d[[x]]$url,
          source_id = source_id,
          source_id_url = source_id_url)
      }))
  
  if (nrow(output) == 0) {
    return("No results found.")
  } else {
    # output <- dplyr::distinct(format_search_results(output))
    output <- dplyr::distinct(output)
    output <- tidyr::as_tibble(output)
    return(output)
  }

}








# Update NEON summary info
# 
# This temporarily accommodates NEON's plan for multiple L1 versions and will have to be refactored to 
# support other data sources adopting this use case. A permanent solution is to refactor the \code{summarize_data_neon()} 
# and \code{summarize_data_edi()} functions to return the exact objects of interest.
#
#
# @param d (list) EDI and NEON dataset summary object
#
# @return (list) \code{d} with added info
# 
# @details Joins /inst/L1_versions.txt (a file holding version info) with \code{d} and does a little reformatting.
#
update_neon_summary_info <- function(d) {
  x <- data.table::fread(
    system.file("extdata", "L1_versions.txt", package = "ecocomDP"))
  for (ind in seq_along(names(d))) {
    id <- names(d)[ind]
    if (stringr::str_detect(id, "^DP1")) {
      i <- x$id %in% id
      d[[id]]$source <- "NEON" # Shift IDs
      d[[id]]$source_id <- id
      d[[id]]$source_id_url <- d[[id]]$url # Shift URLs
      d[[id]]$url <- NA_character_
      d[[id]]$title <- paste(x$title_qualifier[i], d[[id]]$title) # Add qualifiers to title, description, abstract
      d[[id]]$description <- paste(x$description_qualifier[i], d[[id]]$description) 
      d[[id]]$abstract <- paste(x$abstract_qualifier[i], d[[id]]$abstract)
    }
  }
  names(d)[match(x$id, names(d))] <- x$derived_id
  d$neon.ecocomdp.10022.001.002$source_id <- stringr::str_remove(d$neon.ecocomdp.10022.001.002$source_id, "_herps")
  return(d)
}
