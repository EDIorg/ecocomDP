# -----------------------------------------------------------------------------
# This function converts source dataset "knb-lter-hfr.118" (archived in the EDI
# Data Repository) to ecocomDP dataset "edi.193" (also archived in EDI)
# 
# Arguments:
#
# path        Where the ecocomDP tables will be written
# source_id   Identifier of the source dataset
# derived_id  Identifier of the derived dataset
# url         The URL by which the derived tables and metadata can be accessed 
#             by a data repository. This argument is used when automating the 
#             repository publication step, but not used when manually 
#             publishing.
#
# Value:
#
# tables      (.csv) ecocomDP tables
# metadata    (.xml) EML metadata for tables
# 
# Details:
#             This function facilitates automated updates to the derived 
#             "edi.193" whenever new data are added to the source 
#             "knb-lter-hrf.118". The framework executing this maintenance 
#             routine is hosted on a remote server and jumps into action 
#             whenever an update notification is received for 
#             "knb-lter-hrf.118". The maintenance routine parses the 
#             notification to get the arguments to create_ecocomDP().
#
# Landing page to source dataset "knb-lter-hfr.118":
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118
# Landing page to derived dataset "edi.193":
# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=193
# -----------------------------------------------------------------------------

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

create_ecocomDP <- function(path,
                            source_id, 
                            derived_id, 
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
  
  # Parse flat into ecocomDP tables -------------------------------------------
  
  # Each ecocomDP table has an associated "create" function. Begin with the 
  # core required tables.
  
  observation <- ecocomDP::create_observation(
    L0_flat = flat, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "variable_name",
    value = "value",
    unit = "unit")
  
  location <- ecocomDP::create_location(
    L0_flat = flat, 
    location_id = "location_id", 
    location_name = c("block", "plot"), 
    latitude = "latitude", 
    longitude = "longitude", 
    elevation = "elevation")
  
  taxon <- ecocomDP::create_taxon(
    L0_flat = flat, 
    taxon_id = "taxon_id", 
    taxon_rank = "taxon_rank", 
    taxon_name = "taxon_name", 
    authority_system = "authority_system", 
    authority_taxon_id = "authority_taxon_id")
  
  dataset_summary <- ecocomDP::create_dataset_summary(
    L0_flat = flat, 
    package_id = "package_id", 
    original_package_id = "original_package_id", 
    length_of_survey_years = "length_of_survey_years",
    number_of_years_sampled = "number_of_years_sampled", 
    std_dev_interval_betw_years = "std_dev_interval_betw_years", 
    max_num_taxa = "max_num_taxa", 
    geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2")
  
  # Create the ancillary ecocomDP tables. These are optional, but should be 
  # included if possible.
  
  observation_ancillary <- ecocomDP::create_observation_ancillary(
    L0_flat = flat,
    observation_id = "observation_id", 
    variable_name = c("trap.type", "trap.num", "moose.cage"))
  
  location_ancillary <- ecocomDP::create_location_ancillary(
    L0_flat = flat,
    location_id = "location_id",
    variable_name = "treatment")
  
  taxon_ancillary <- ecocomDP::create_taxon_ancillary(
    L0_flat = flat,
    taxon_id = "taxon_id",
    variable_name = c(
      "subfamily", "hl", "rel", "rll", "colony.size", 
      "feeding.preference", "nest.substrate", "primary.habitat", 
      "secondary.habitat", "seed.disperser", "slavemaker.sp", 
      "behavior", "biogeographic.affinity", "source"),
    unit = c("unit_hl", "unit_rel", "unit_rll"))
  
  # Create the variable_mapping table. This is optional but highly recommended 
  # as it provides unambiguous definitions to variables and facilitates 
  # integration with other ecocomDP datasets.
  
  variable_mapping <- ecocomDP::create_variable_mapping(
    observation = observation,
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary, 
    taxon_ancillary = taxon_ancillary)
  
  i <- variable_mapping$variable_name == 'abundance'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/individualCount'
  variable_mapping$mapped_label[i] <- 'individualCount'
  
  i <- variable_mapping$variable_name == 'treatment'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00000506'
  variable_mapping$mapped_label[i] <- 'Manipulative experiment'
  
  i <- variable_mapping$variable_name == 'trap.type'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00001591'
  variable_mapping$mapped_label[i] <- 'type of trap'
  
  i <- variable_mapping$variable_name == 'hl'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  variable_mapping$mapped_label[i] <- 'measurementType'
  
  i <- variable_mapping$variable_name == 'rel'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  variable_mapping$mapped_label[i] <- 'measurementType'
  
  i <- variable_mapping$variable_name == 'rll'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  variable_mapping$mapped_label[i] <- 'measurementType'
  
  i <- variable_mapping$variable_name == 'colony.size'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00000311'
  variable_mapping$mapped_label[i] <- 'Population'
  
  i <- variable_mapping$variable_name == 'feeding.preference'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  variable_mapping$mapped_label[i] <- 'behavior'
  
  i <- variable_mapping$variable_name == 'primary.habitat'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  variable_mapping$mapped_label[i] <- 'type of habitat'
  
  i <- variable_mapping$variable_name == 'secondary.habitat'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  variable_mapping$mapped_label[i] <- 'type of habitat'
  
  i <- variable_mapping$variable_name == 'seed.disperser'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  variable_mapping$mapped_label[i] <- 'behavior'
  
  i <- variable_mapping$variable_name == 'slavemaker.sp'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  variable_mapping$mapped_label[i] <- 'behavior'
  
  i <- variable_mapping$variable_name == 'behavior'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  variable_mapping$mapped_label[i] <- 'behavior'
  
  i <- variable_mapping$variable_name == 'biogeographic.affinity'
  variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  variable_mapping$mapped_label[i] <- 'type of habitat'
  
  i <- variable_mapping$variable_name == 'source'
  variable_mapping$mapped_system[i] <- 'Darwin Core'
  variable_mapping$mapped_id[i] <- 'http://purl.org/dc/terms/references'
  variable_mapping$mapped_label[i] <- 'references'
  
  # Write tables to file
  
  ecocomDP::write_tables(
    path = path, 
    observation = observation, 
    location = location,
    taxon = taxon,
    dataset_summary = dataset_summary, 
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary, 
    taxon_ancillary = taxon_ancillary, 
    variable_mapping = variable_mapping)
  
  # Validate tables -----------------------------------------------------------
  
  # Validation checks ensure the derived set of tables comply with the ecocomDP
  # model. Any issues at this point 
  # should be addressed in the lines of code above, the tables rewritten, and 
  # another round of validation, to be certain the fix worked.
  
  issues <- ecocomDP::validate_data(path = path)
  
  # Create metadata -----------------------------------------------------------
  
  # Before publishing the derived ecocomDP dataset, we need to describe it. The 
  # create_eml() function does this all for us. It knows the structure of the 
  # ecocomDP model and applies standardized table descriptions and mixes in 
  # important elements of the source dataset metadata for purposes of 
  # communication and provenance tracking.
  
  # Convert "dataset level keywords" listed in the source to "dataset level 
  # annotations" in the derived. The predicate "is about" is used, which 
  # results in an annotation that reads "This dataset is about 'species 
  # abundance'", "This dataset is about 'Population'", etc. All source datasets
  # involving a human induced manipulative experiment, not a natural 
  # disturbance/experiment, should include the "Manipulative experiment" 
  # annotation below to enable searching on this term.
  
  dataset_annotations <- c(
    `species abundance` = 
      "http://purl.dataone.org/odo/ECSO_00001688",
    Population = 
      "http://purl.dataone.org/odo/ECSO_00000311",
    `Manipulative experiment` = 
      "http://purl.dataone.org/odo/ECSO_00000506",
    `level of ecological disturbance` = 
      "http://purl.dataone.org/odo/ECSO_00002588",
    `type of ecological disturbance` = 
      "http://purl.dataone.org/odo/ECSO_00002589")
  
  # Add contact information for the author of this script and dataset
  
  additional_contact <- data.frame(
    givenName = 'Colin',
    surName = 'Smith',
    organizationName = 'Environmental Data Initiative',
    electronicMailAddress = 'ecocomdp@gmail.com',
    stringsAsFactors = FALSE)
  
  # Create EML metadata
  
  eml <- ecocomDP::create_eml(
    path = path,
    source_id = source_id,
    derived_id = derived_id,
    is_about = dataset_annotations,
    script = "create_ecocomDP.R",
    script_description = 
      "A function for converting knb-lter-hrf.118 to ecocomDP",
    contact = additional_contact,
    user_id = 'ecocomdp',
    user_domain = 'EDI',
    basis_of_record = "HumanObservation")
  
}