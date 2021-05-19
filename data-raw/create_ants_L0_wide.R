# This function creates the example dataset "ants_L0_wide" from:
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118&revision=32

library(ecocomDP)
library(EDIutils)       # remotes::install_github("EDIorg/EDIutils")
library(taxonomyCleanr) # remotes::install_github("EDIorg/taxonomyCleanr")
library(xml2)
library(magrittr)
library(data.table)
library(lubridate)
library(tidyr)
library(usethis)

create_ants_L0_wide <- function(path = NULL, 
                                package.id.L0 = "knb-lter-hfr.118.32", 
                                package.id.L1 = "edi.193.4", 
                                environment = "production",
                                url = NULL) {
  
  # Read source metadata and data ---------------------------------------------
  # The source dataset is about the changing community composition and functional traits of ant species in response to the invasive hemlock woolly adelgid. Observations are made across habitat types within the Harvard Experimental Forest. The dataset consists of 2 tables, a primary table (hf118-01-ants.csv) containing abundances at sites through time, and an ancillary table listing physical and functional traits of observed species (hf118-02-functional-traits.csv).
  
  # Read EML metadata and data tables directly from the EDI Data Repository
  
  eml <- EDIutils::api_read_metadata(package.id.L0, environment)
  data <- EDIutils::read_tables(
    eml = eml, 
    strip.white = TRUE,
    na.strings = "",
    convert.missing.value = TRUE, 
    add.units = TRUE)
  
  ants <- data$`hf118-01-ants.csv`
  traits <- data$`hf118-02-functional-traits.csv`
  
  ants$date <- lubridate::ymd(ants$date)
  
  # Create wide version of source dataset -------------------------------------
  # Joining all source data and relevant metadata into a wide master table both simplifies parsing into ecocomDP tables and maintains referential integrity among data elements.
  
  # Remove duplicate data from the ancillary table and join on species codes once the column names match
  
  traits <- traits %>% dplyr::select(-genus, -species)
  traits <- traits %>% dplyr::rename(code = species.code)
  wide <- dplyr::left_join(ants, traits, by = "code")
  
  # Sorting and arranging rows of this wide table according to sampling date and location at this point will result in a nice human readable dataset. This is merely cosmetic and completely optional.
  
  wide <- wide %>% dplyr::arrange(date, plot, block)
  
  # Each row of the wide table represents an observation of taxa abundance and should therefore be assigned a unique ID.
  
  wide$observation_id <- seq(nrow(wide))
  
  # Observations are made in plots, which are nested in blocks. Each unique combination of these locations creates a location ID.
  
  wide$location_id <- wide %>% group_by(plot, block) %>% group_indices()
  
  # Each unique combination of location and date form a sampling event
  
  wide$event_id <- wide %>% group_by(date, plot, block) %>% group_indices()
  
  # Because the context (i.e. plot and block) will be lost when values are converted into the long (attribute-value) location table of ecocomDP, we should combine the context and values to preserve meaning.
  
  wide$plot <- paste0("plot_", wide$plot)
  wide$block <- paste0("block_", wide$block)
  
  # Ideally, the source dataset would include latitude, longitude, and elevation for each location_id, but all we have are coordinates for the area encompassing all sampling locations. The best option here is to use the middle of the bounding box and elevations.
  
  geocov <- xml2::xml_find_all(eml, ".//geographicCoverage")
  north <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/northBoundingCoordinate'))
  east <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/eastBoundingCoordinate'))
  south <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/southBoundingCoordinate'))
  west <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/westBoundingCoordinate'))
  elev_max <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/altitudeMaximum'))
  elev_min <- xml2::xml_double(
    xml2::xml_find_all(geocov, './/altitudeMinimum'))
  
  wide$latitude <- mean(c(north, south))
  wide$longitude <- mean(c(east, west))
  wide$elevation <- mean(c(elev_max, elev_min))
  
  # Taxonomic entities of this dataset are comprised of unique genus and species pairs.
  
  wide <- wide %>% 
    dplyr::mutate(taxon_name = trimws(paste(genus, species))) %>% 
    dplyr::select(-genus, -species)
  
  wide$taxon_id <- wide %>% group_by(taxon_name) %>% group_indices()
  
  # While not required, resolving taxonomic entities to an authority system greatly improves the discoverability and interoperability of the derived ecocomDP dataset. We can resolve taxa by send taxon_name values to taxonomyCleanr for direct matches against the Integrated Taxonomic Information System (ITIS; https://www.itis.gov/).
  
  taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
    x = unique(wide$taxon_name),
    data.sources = 3)
  
  taxa_resolved <- taxa_resolved %>%
    dplyr::select(taxa, rank, authority, authority_id) %>%
    dplyr::rename(
      taxon_rank = rank,
      taxon_name = taxa,
      authority_system = authority,
      authority_taxon_id = authority_id)
  
  wide <- dplyr::left_join(wide, taxa_resolved, by = "taxon_name")
  
  # Add summary information about this dataset
  
  dates <- wide$date %>% na.omit() %>% sort()
  
  length_of_survey <-  ecocomDP::calc_length_of_survey_years(dates)
  years_sampled <- ecocomDP::calc_number_of_years_sampled(dates)
  std_dev_betw_years <- ecocomDP::calc_std_dev_interval_betw_years(dates)
  number_of_taxa <- length(unique(wide$taxon_name))
  bounding_box_m2 <- ecocomDP::calc_geo_extent_bounding_box_m2(west, east, north, south)
  
  wide$package_id <- package.id.L1
  wide$original_package_id <- package.id.L0
  wide$length_of_survey_years <- length_of_survey
  wide$number_of_years_sampled <- years_sampled
  wide$std_dev_interval_betw_years <- std_dev_betw_years
  wide$max_num_taxa <- number_of_taxa
  wide$geo_extent_bounding_box_m2 <- bounding_box_m2
  
  # Rename columns of the source dataset to match the ecocomDP model, and remove any remaining columns of redundant information that should not be included in the derived ecocomDP dataset.
  
  wide <- wide %>% dplyr::rename(datetime = date) %>% dplyr::select(-year, -code)
  
  # Some columns are not required, but we'll include them for the sake of completenesss
  
  wide$author <- NA_character_
  
  # Save to /data
  ants_L0_wide <- wide
  usethis::use_data(ants_L0_wide)
  
}
