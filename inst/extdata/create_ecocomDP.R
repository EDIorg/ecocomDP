# This function converts EDI data package "knb-lter-hfr.118" into the ecocomDP,
# which in turn is archived in EDI as data package "edi.193". Specifically this
# function reads in knb-lter-hfr.118 tables and metadata directly from the EDI
# repository, then synthesizes and reformats this information into ecocomDP 
# tables and metadata, and finally performs a set of validation checks to 
# ensure the result is compliant with the ecocomDP model.
#
# Source and derived data packages:
# knb-lter-hfr.118 - https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118
# edi.193 - https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=193
#
# This function is designed in a way that facilitates automated updates to the 
# derived edi.193 whenever new data are added to the source knb-lter-hrf.118.
# The framework executing this maintenance routine is hosted on a remote server
# and jumps into action whenever an event notification is received for 
# knb-lter-hrf.118 is received. The maintenance routine parses the notification
# to get the input values to the 5 arguments of this function:
#
# 1.) "path" - The directory to which the ecocomDP tables will be written.
# 2.) "package.id.L0" - The identifier of the source data package.
# 3.) "package.id.L1" - The identifier of the derived data package.
# 4.) "environment" - The environment of the data repository in which 
#     package.id.L0 and package.id.L1 are found. Some repositories have
#     "development", "staging", and "production" environments, which may affect
#     repository API calls.
# 5.) "url" - The URL through which the derived data tables and metadata can be
#     downloaded by the repository in which the derived data package will be
#     be archived.
#
# If you would like your conversion script hooked into this maintenance 
# routine, then use the same conventions of this function.

library(ecocomDP)
library(EDIutils)       # remotes::install_github("EDIorg/EDIutils")
library(taxonomyCleanr) # remotes::install_github("EDIorg/taxonomyCleanr")
library(xml2)
library(magrittr)
library(data.table)
library(lubridate)
library(tidyr)


create_ecocomDP <- function(path,
                            package.id.L0, 
                            package.id.L1, 
                            environment,
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
  
  # Parse ecocomDP tables -----------------------------------------------------
  # The hard work is done! The wide master source table is complete and we can now use the "create" functions to parse it into the ecocomDP tables. Each ecocomDP table has an associated create function.
  
  # Create the core ecocomDP tables. These are required.
  # FIXME: Add general comments on what arguments mean and why their values (i.e. what data cols mean)
  
  observation <- ecocomDP::create_observation(
    L0_wide = wide, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "abundance",
    unit = "unit_abundance")
  
  location <- ecocomDP::create_location(
    L0_wide = wide, 
    location_id = "location_id", 
    location_name = c("block", "plot"), 
    latitude = "latitude", 
    longitude = "longitude", 
    elevation = "elevation")
  
  taxon <- ecocomDP::create_taxon(
    L0_wide = wide, 
    taxon_id = "taxon_id", 
    taxon_rank = "taxon_rank", 
    taxon_name = "taxon_name", 
    authority_system = "authority_system", 
    authority_taxon_id = "authority_taxon_id")
  
  dataset_summary <- ecocomDP::create_dataset_summary(
    L0_wide = wide, 
    package_id = "package_id", 
    original_package_id = "original_package_id", 
    length_of_survey_years = "length_of_survey_years",
    number_of_years_sampled = "number_of_years_sampled", 
    std_dev_interval_betw_years = "std_dev_interval_betw_years", 
    max_num_taxa = "max_num_taxa", 
    geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2")
  
  # Create the ancillary ecocomDP tables. These are optional, but should be included if possible.
  
  observation_ancillary <- ecocomDP::create_observation_ancillary(
    L0_wide = wide,
    observation_id = "observation_id", 
    variable_name = c("trap.type", "trap.num", "moose.cage"))
  
  location_ancillary <- ecocomDP::create_location_ancillary(
    L0_wide = wide,
    location_id = "location_id",
    variable_name = "treatment")
  
  taxon_ancillary <- ecocomDP::create_taxon_ancillary(
    L0_wide = wide,
    taxon_id = "taxon_id",
    variable_name = c(
      "subfamily", "hl", "rel", "rll", "colony.size", 
      "feeding.preference", "nest.substrate", "primary.habitat", 
      "secondary.habitat", "seed.disperser", "slavemaker.sp", 
      "behavior", "biogeographic.affinity", "source"),
    unit = c("unit_hl", "unit_rel", "unit_rll"))
  
  # Create the variable_mapping table. This is optional but highly recommended as it provides unambiguous definitions to variables and facilitates integration with other ecocomDP datasets.
  
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
  
  # Write ecocomDP tables to file
  
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
  
  # Validate ecocomDP tables --------------------------------------------------
  # Validation checks ensure the derived set of tables comply with the ecocomDP
  # model and enable use of the data accordingly. Any issues at this point 
  # should be addressed in the lines of code above, the tables rewritten, and 
  # another run of validate(), just to be certain the fix did what it was
  # supposed to.
  
  issues <- ecocomDP::validate(data.path = path)
  
  # Create ecocomDP metadata --------------------------------------------------
  # Before publishing the derived ecocomDP dataset in the EDI Data Repository (or any other repository supporting EML metadata) we need to describe it. The known structure of the derived ecocomDP dataset allows us to apply boilerplate metadata to describe the tables, and utilize the create_eml() function to mix in important elements of the source and derived datasets.
  
  # Convert L0 keywords to annotations for the L1
  # "is about"
  
  dataset_annotations <- c( # FIXME: dataset level annotations that are not boilerplate to the model
    `species abundance` = "http://purl.dataone.org/odo/ECSO_00001688",
    Population = "http://purl.dataone.org/odo/ECSO_00000311",
    `level of ecological disturbance` = "http://purl.dataone.org/odo/ECSO_00002588",
    `type of ecological disturbance` = "http://purl.dataone.org/odo/ECSO_00002589")
  
  # Add contact information for the author of these ecocomDP scripts
  
  additional_contact <- data.frame(
    givenName = 'Colin',
    surName = 'Smith',
    organizationName = 'Environmental Data Initiative', # FIXME: Don't require this
    electronicMailAddress = 'ecocomdp@gmail.com',
    stringsAsFactors = FALSE)
  
  eml <- ecocomDP::create_eml(
    path = path,
    parent.package.id = package.id.L0,
    child.package.id = package.id.L1,
    is.about = dataset_annotations,
    script = "create_ecocomDP.R",
    script.description = "A function for converting knb-lter-hrf.118 to ecocomDP",
    contact = additional_contact,
    user.id = 'ecocomdp',
    user.domain = 'EDI',
    basis_of_record = "HumanObservation") # FIXME: darwin core terms facilitate annotation of downstream L2 conversion
}
