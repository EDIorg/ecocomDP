# Create ecocomDP data tables
#
# This script converts knb-lter-bes.543.x to the ecocomDP.

convert_bes543_to_ecocomDP <- function(path, parent_pkg_id, child_pkg_id){
  
  message(paste0('Converting ', parent_pkg_id, ' to ecocomDP (', child_pkg_id, ')' ))
  
  project_name <- 'bird_survey'
  
  # Load libraries
  
  library(XML)
  library(tidyr)
  library(dplyr)
  library(ecocomDP)
  library(lubridate)
  library(taxonomyCleanr)
  library(dataCleanr)
  library(stringr)
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to the directory that will be filled with ecocomDP tables.')
  }
  if (missing(child_pkg_id)){
    stop('Input argument "child_pkg_id" is missing! Specify new package ID (e.g. edi.249.1, or knb-lter-mcr.8.1.')
  }
  if (missing(parent_pkg_id)){
    stop('Input argument "parent_pkg_id" is missing! Specify the project name of the L0 dataset.')
  }
  
  # Load EML and extract information ------------------------------------------
  
  message('Loading data')
  
  metadata <- EDIutils::read_metadata(parent_pkg_id)
  
  data_name_id <- EDIutils::read_data_entity_names(parent_pkg_id)
  data_name <- data_name_id$name
  data_id <- data_name_id$identifier
  
  data_delimiters <- unlist(
    xmlApply(
      metadata[
        "//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"
        ],
      xmlValue
    )
  )
  
  data_urls <- c(
    unlist(
      xmlApply(
        metadata["//dataset/dataTable/physical/distribution/online/url"], 
        xmlValue
      )
    ),
    unlist(
      xmlApply(
        metadata["//dataset/otherEntity/physical/distribution/online/url"],
        xmlValue
      )
    )
  )
  
  data_name_id$url <- NA_character_
  for (i in 1:length(data_name_id$identifier)){
    data_name_id$url[i] <- data_urls[
      stringr::str_detect(data_urls, data_name_id$identifier[i])
    ]
  }
  
  use_i <- data_name_id$name == 'BES_0543-1'
  birds <- read.csv(file = data_name_id$url[use_i], header = T, as.is = T)
  
  use_i <- data_name_id$name == 'BES_0543-2'
  surveys <- read.csv(file = data_name_id$url[use_i], header = T, as.is = T)
  
  use_i <- data_name_id$name == 'BES_0543-3'
  sites <- read.csv(file = data_name_id$url[use_i], header = T, as.is = T)
  
  use_i <- data_name_id$name == 'BES_0543-4'
  taxa <- read.csv(file = data_name_id$url[use_i], header = T, as.is = T)
  
  # Create observation table --------------------------------------------------
  
  message('Creating table "observation"')	
  
  # observation_datetime
  
  birds$observation_datetime <- surveys$survey_date[
    match(surveys$survey_id, birds$survey_id)
  ]
  
  birds$observation_datetime <- dataCleanr::iso8601_char(
    x = birds$observation_datetime,
    orders = 'mdy'
  )
  
  # event_id
  
  birds$event_id <- birds$survey_id
  
  # package_id
  
  birds$package_id <- child_pkg_id
  
  # location_id
  
  birds$location_id <- trimws(birds$site_id, 'both')
  
  # taxon_id
  
  birds$taxon_id <- trimws(birds$species_id, 'both')
  
  # Gather
  
  observation <- tidyr::gather(
    birds,
    'variable_name',
    'value',
    bird_count
  )
  
  # unit
  
  observation$unit <- 'number'
  
  # observation_id
  
  observation$observation_id <- paste0(
    'ob_',
    seq(nrow(observation))
  )
  
  # Select
  
  observation <- select(
    observation,
    observation_id,
    event_id,
    package_id,
    location_id,
    observation_datetime,
    taxon_id,
    variable_name,
    value,
    unit
  )
  
  # Create location table -----------------------------------------------------
  
  message('Creating table "location"')
  
  # Fix referential integrity violation
  # observation$location_id contains site_id absent in the sites list
  
  use_i <- !(observation$location_id %in% sites$site_id)
  
  if (sum(use_i) > 0){
    
    df <- as.data.frame(
      matrix(
        NA_character_,
        sum(use_i),
        ncol(sites)
      )
    )
    
    colnames(df) <- colnames(sites)
    
    df$site_id <- observation$location_id[use_i]
    
    df$park_code <- 'unknown'
    
    sites <- rbind(
      sites,
      df
    )
    
  }
  
  # Make location table
  
  location <- ecocomDP::make_location(
    x = sites,
    cols = c('park_code', 'site_id')
  )

  # Update observation table --------------------------------------------------
  
  # location_id
  
  observation$location_id <- location$location_id[
    match(observation$location_id, location$location_name)
  ]
  
  # Create location_ancillary table -------------------------------------------
  
  message('Creating table "location_ancillary"')
  
  sites$location_id <- location$location_id[
    match(sites$site_id, location$location_name)
  ]
  
  location_ancillary <- select(
    sites,
    location_id,
    park_district,
    park_name,
    point_code,
    point_location,
    park_acreage
  )
  
  # Gather
  
  location_ancillary <- tidyr::gather(
    location_ancillary,
    'variable_name',
    'value',
    park_district,
    park_name,
    point_code,
    point_location,
    park_acreage
  )
  
  # unit
  
  location_ancillary$unit <- NA_character_
  
  location_ancillary$unit[
    location_ancillary$variable_name == 'park_acreage'
  ] <- 'acre'
  
  # location_ancillary_id
  
  location_ancillary$location_ancillary_id <- paste0(
    'loan_',
    seq(nrow(location_ancillary))
  )
  
  # Select
  
  location_ancillary <- select(
    location_ancillary,
    location_ancillary_id,
    location_id,
    variable_name,
    value,
    unit
  )
  
  # Create taxon table --------------------------------------------------------
  
  taxon <- ecocomDP::make_taxon(
    taxa = taxa$common_name,
    taxon.id = taxa$species_id,
    name.type = 'common',
    data.sources = 3
  )
  
  # Create taxon_ancillary table ----------------------------------------------
  
  message('Creating table "taxon_ancillary"')
  
  # Select
  
  taxon_ancillary <- dplyr::select(
    taxa,
    species_id,
    asu_itis
  )
  
  # taxon_id
  
  names(taxon_ancillary)[
    names(taxon_ancillary) == 'species_id'
  ] <- 'taxon_id'
  
  # Gather
  
  taxon_ancillary <- tidyr::gather(
    taxon_ancillary,
    'variable_name',
    'value',
    asu_itis
  )
  
  # taxon_ancillary_id
  
  taxon_ancillary$taxon_ancillary_id <- paste0(
    'txan_',
    seq(nrow(taxon_ancillary))
  )
  
  # Select
  
  taxon_ancillary <- dplyr::select(
    taxon_ancillary,
    taxon_ancillary_id,
    taxon_id,
    variable_name,
    value
  )
  
  # Create observation_ancillary table ----------------------------------------
  
  message('Creating table "observation_ancillary"')
  
  observation_ancillary <- unique.data.frame(surveys)
  
  # event_id
  
  names(observation_ancillary)[
    names(observation_ancillary) == 'survey_id'
  ] <- 'event_id'
  
  # Select
  
  observation_ancillary <- dplyr::select(
    observation_ancillary,
    event_id,
    survey_date,
    time_start,
    observer,
    wind,
    air_temp_F,
    cloud
  )
  
  # Reformat datetime
  
  observation_ancillary$survey_date <- dataCleanr::iso8601_char(
    x = observation_ancillary$survey_date,
    orders = 'mdy'
  )
  
  # Gather
  
  observation_ancillary <- tidyr::gather(
    observation_ancillary,
    'variable_name',
    'value',
    survey_date,
    time_start,
    observer,
    wind,
    air_temp_F,
    cloud
  )
  
  # unit
  
  observation_ancillary$unit <- NA_character_
  
  observation_ancillary$unit[
    observation_ancillary$variable_name == 'air_temp_F'
  ] <- 'degree'
  
  observation_ancillary$unit[
    observation_ancillary$variable_name == 'cloud'
    ] <- 'dimensionless'
  
  # observation_ancillary_id
  
  observation_ancillary$observation_ancillary_id <- paste0(
    'oban_',
    seq(nrow(observation_ancillary))
  )
  
  # Select
  
  observation_ancillary <- dplyr::select(
    observation_ancillary,
    observation_ancillary_id,
    event_id,
    variable_name,
    value,
    unit
  )
  
  # Create dataset_summary table ----------------------------------------------
  
  message('Creating table "dataset_summary"')
  
  dataset_summary <- make_dataset_summary(
    parent.package.id = parent_pkg_id,
    child.package.id = child_pkg_id,
    sample.dates = observation$observation_datetime,
    taxon.table = taxon
  )
  
  # Create variable_mapping table ---------------------------------------------
  
  message('Creating table "variable_mapping"')
  
  variable_mapping <- make_variable_mapping(
    observation = observation,
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary,
    taxon_ancillary = taxon_ancillary
  )
  
  # define bird_count
  
  use_i <- variable_mapping$variable_name == 'bird_count'
  variable_mapping$mapped_system[use_i] <- 'Darwin Core'
  variable_mapping$mapped_id[use_i] <- 'http://rs.tdwg.org/dwc/terms/individualCount'
  variable_mapping$mapped_label[use_i] <- 'individualCount'
  
  # define survey_date
  
  use_i <- variable_mapping$variable_name == 'survey_date'
  variable_mapping$mapped_system[use_i] <- 'Darwin Core'
  variable_mapping$mapped_id[use_i] <- 'http://rs.tdwg.org/dwc/terms/eventDate'
  variable_mapping$mapped_label[use_i] <- 'eventDate'

  # define time_start
  
  use_i <- variable_mapping$variable_name == 'time_start'
  variable_mapping$mapped_system[use_i] <- 'Darwin Core'
  variable_mapping$mapped_id[use_i] <- 'http://rs.tdwg.org/dwc/terms/eventTime'
  variable_mapping$mapped_label[use_i] <- 'eventTime'

  # define observer
  
  use_i <- variable_mapping$variable_name == 'observer'
  variable_mapping$mapped_system[use_i] <- 'Darwin Core'
  variable_mapping$mapped_id[use_i] <- 'http://rs.tdwg.org/dwc/terms/recordedBy'
  variable_mapping$mapped_label[use_i] <- 'recordedBy'

  # define wind
  
  use_i <- variable_mapping$variable_name == 'wind'
  variable_mapping$mapped_system[use_i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[use_i] <- 'http://purl.dataone.org/odo/ECSO_00001233'
  variable_mapping$mapped_label[use_i] <- 'Wind Speed'
  
  # define air_temp_F
  
  use_i <- variable_mapping$variable_name == 'air_temp_F'
  variable_mapping$mapped_system[use_i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[use_i] <- 'http://purl.dataone.org/odo/ECSO_00001225'
  variable_mapping$mapped_label[use_i] <- 'Air Temperature'
  
  # define park_district
  
  use_i <- variable_mapping$variable_name == 'park_district'
  variable_mapping$mapped_system[use_i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[use_i] <- 'http://purl.obolibrary.org/obo/ENVO_00000004'
  variable_mapping$mapped_label[use_i] <- 'administrative region'
  
  # define park_acreage
  
  use_i <- variable_mapping$variable_name == 'park_acreage'
  variable_mapping$mapped_system[use_i] <- 'The Ecosystem Ontology'
  variable_mapping$mapped_id[use_i] <- 'http://purl.obolibrary.org/obo/UO_0000047'
  variable_mapping$mapped_label[use_i] <- 'area unit'
  
  # Write tables to file ------------------------------------------------------
  
  ecocomDP::write_ecocomDP_tables(
    path = path,
    sep = ',',
    study.name = project_name,
    observation = observation,
    location = location,
    taxon = taxon,
    dataset_summary = dataset_summary,
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary,
    taxon_ancillary = taxon_ancillary,
    variable_mapping = variable_mapping
  )

}
