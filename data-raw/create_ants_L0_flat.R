# This function creates the example dataset "ants_L0_flat" from:
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118&revision=33

# Libraries used by this function

library(ecocomDP)
library(xml2)
library(magrittr)
library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(EDIutils)       # remotes::install_github("EDIorg/EDIutils")
library(taxonomyCleanr) # remotes::install_github("EDIorg/taxonomyCleanr")
library(usethis)

create_ants_L0_flat <- function(path = NULL, 
                                source_id = "knb-lter-hfr.118.33", 
                                derived_id = "edi.193.5", 
                                url = NULL) {
  
  # Read source dataset -------------------------------------------------------
  
  # The source dataset is about ant communities and their functional traits 
  # changing in response to an invasive species. Observations are made across 
  # habitat types within the Harvard Experimental Forest. The dataset consists 
  # of a primary table listing abundances at sites through time, and an 
  # ancillary table listing physical and functional traits of observed species.
  
  # Read the source dataset from EDI
  
  eml <- EDIutils::api_read_metadata(source_id)
  data <- EDIutils::read_tables(
    eml = eml, 
    strip.white = TRUE,
    na.strings = "",
    convert.missing.value = TRUE, 
    add.units = TRUE)
  
  ants <- data$`hf118-01-ants.csv`
  traits <- data$`hf118-02-functional-traits.csv`
  ants$date <- ymd(ants$date)
  
  # Join and flatten the source dataset ---------------------------------------
  
  # Joining all source data and relevant metadata into one big flat table 
  # simplifies parsing into ecocomDP tables and facilitates referential 
  # integrity in the process.
  
  # Remove duplicate data from the ancillary table and join on species "code"
  
  traits <- traits %>% select(-genus, -species)
  traits <- traits %>% rename(code = species.code)
  wide <- left_join(ants, traits, by = "code")
  
  # Convert wide format to "flat" format. This is the wide form but gathered on 
  # core observation variables, which are often > 1 in source datasets. This 
  # "flat" table is the "widest" ecocomDP datasets can be consistently returned 
  # to by the ecocomDP::flatten_data() function, and is the input format 
  # required by the "create table" helpers we'll meet shortly. This dataset 
  # only has one core observation variable, "abundance", so gathering really 
  # only entails a change of column names.
  
  wide <- wide %>% rename(value_abundance = abundance)
  flat <- pivot_longer(
    wide,
    cols = matches("abundance"), 
    names_to = c(".value", "variable_name"), 
    names_sep = '\\_')
  
  # We're now in a good place to begin adding columns of the ecocomDP tables 
  # we can create from this source dataset. We'll begin with the observation 
  # table.
  
  # Add columns for the observation table -------------------------------------
  
  # The frequency and timing of surveys (events) varied throughout the history 
  # of this dataset and are uniquely identifiable by grouping sample dates by 
  # year and month.
  
  flat$event_id <- flat %>% group_by(month = floor_date(flat$date, "month"),
                                     year) %>% group_indices()
  flat <- flat %>% arrange(event_id)
  
  # Observations are made in plots, which are nested in blocks. Unique 
  # combinations of these form a location
  
  flat$location_id <- flat %>% group_by(plot, block) %>% group_indices()
  
  # Each row of the flattened source dataset represents an observation of taxa 
  # abundance and should have a unique ID for reference
  
  flat$observation_id <- seq(nrow(flat))
  
  # Add columns for the location table ----------------------------------------
  
  # Ideally, the source dataset would include latitude, longitude, and 
  # elevation for each location_id, but all we have are coordinates for the 
  # area encompassing all sampling locations. The best we can do here is use 
  # the middle of the bounding box and mean of the bounding elevations.
  
  geocov <- xml_find_all(eml, ".//geographicCoverage")
  north <- xml_double(xml_find_all(geocov, './/northBoundingCoordinate'))
  east <- xml_double(xml_find_all(geocov, './/eastBoundingCoordinate'))
  south <- xml_double(xml_find_all(geocov, './/southBoundingCoordinate'))
  west <- xml_double(xml_find_all(geocov, './/westBoundingCoordinate'))
  elev_max <- xml_double(xml_find_all(geocov, './/altitudeMaximum'))
  elev_min <- xml_double(xml_find_all(geocov, './/altitudeMinimum'))
  
  flat$latitude <- mean(c(north, south))
  flat$longitude <- mean(c(east, west))
  flat$elevation <- mean(c(elev_max, elev_min))
  
  # Add columns for the taxon table -------------------------------------------
  
  # Taxonomic entities of this dataset are comprised of unique genus and 
  # species pairs
  
  flat <- flat %>% 
    mutate(taxon_name = trimws(paste(genus, species))) %>% 
    select(-genus, -species)
  
  flat$taxon_id <- flat %>% group_by(taxon_name) %>% group_indices()
  
  # While not required, resolving taxonomic entities to an authority system 
  # improves the discoverability and interoperability of the ecocomDP dataset. 
  # We can resolve taxa by sending names through taxonomyCleanr for direct 
  # matches against the Integrated Taxonomic Information System 
  # (ITIS; https://www.itis.gov/).
  
  taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
    x = unique(flat$taxon_name),
    data.sources = 3)
  
  taxa_resolved <- taxa_resolved %>%
    select(taxa, rank, authority, authority_id) %>%
    rename(taxon_rank = rank,
           taxon_name = taxa,
           authority_system = authority,
           authority_taxon_id = authority_id)
  
  flat <- left_join(flat, taxa_resolved, by = "taxon_name")
  
  # Add columns for the dataset_summary table ---------------------------------
  
  dates <- flat$date %>% stats::na.omit() %>% sort()
  
  # Use the calc_*() helper functions for consistency
  
  flat$package_id <- derived_id
  flat$original_package_id <- source_id
  flat$length_of_survey_years <- ecocomDP::calc_length_of_survey_years(dates)
  flat$number_of_years_sampled <- ecocomDP::calc_number_of_years_sampled(dates)
  flat$std_dev_interval_betw_years <- 
    ecocomDP::calc_std_dev_interval_betw_years(dates)
  flat$max_num_taxa <- length(unique(flat$taxon_name))
  flat$geo_extent_bounding_box_m2 <- 
    ecocomDP::calc_geo_extent_bounding_box_m2(west, east, north, south)
  
  # Odds and ends -------------------------------------------------------------
  
  # Rename source columns with an ecocomDP equivalent (date to datetime) and 
  # remove columns of redundant information  (year can be recalculated from 
  # datetime and code was a key that no longer has use).
  
  flat <- flat %>% rename(datetime = date) %>% select(-year, -code)
  
  # The hard work is done! The flat table contains all the source data and 
  # more! We can now use the "create" functions to parse this table into the 
  # ecocomDP tables.
  
  # Save to /data
  ants_L0_flat <- tidyr::as_tibble(flat)
  usethis::use_data(ants_L0_flat, overwrite = TRUE)
  
}
