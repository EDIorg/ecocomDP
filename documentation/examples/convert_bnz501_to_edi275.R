# Create ecocomDP data package
#
# This script converts knb-lter-bnz.501.x to the edi.275.x

# Supply arguments for function

path <- '/Users/csmith/datasets/edi_275'
parent_pkg_id <- 'knb-lter-bnz.501.17'
child_pkg_id <- 'edi.275.1'

# Function

convert_tables <- function(path, parent_pkg_id, child_pkg_id){
  
  project_name <- 'CiPEHR_biomass'
  
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
  
  metadata <- EDIutils::pkg_eml(parent_pkg_id)
  
  data_name_id <- EDIutils::pkg_data_entity_names(parent_pkg_id)
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
  
  
  data <- read.csv(file = data_urls, header = T, as.is = T)
  
  # Create observation table --------------------------------------------------
  
  message('Creating table "observation"')	
  
  # Rename fields
  
  names(data)[names(data) == 'year'] <- 'observation_datetime'
  
  # Add fields
  
  data$package_id <- child_pkg_id
  data$location_id <- paste0(data$plot, '_', data$block, '_', data$fence)
  data$taxon_id <- data$species
  
  # Convert to long format
  
  data <- tidyr::gather(data, 'variable_name', 'value', avghits, biomass)
  
  # Add more fields
  
  data$observation_id <- paste0('obs_', seq(nrow(data)))
  data$unit[data$variable_name == 'avghits'] <- 'dimensionless'
  data$unit[data$variable_name == 'biomass'] <- 'gramsPerSquareMeter'
  df <- data.frame(yr = unique(data$observation_datetime),
                   ev = paste0('ev_', seq(length(unique(data$observation_datetime)))),
                   stringsAsFactors = F)
  data$event_id <- df$ev[match(data$observation_datetime, df$yr)]
  
  # Select fields
  
  observation <- dplyr::select(data, observation_id, event_id, package_id, location_id,
                               observation_datetime, taxon_id, variable_name, value, 
                               unit)
  
  # Create location table -----------------------------------------------------
  
  message('Creating table "location"')	
  
  location <- make_location(data, cols=c("plot","block","fence"))
  
  # Update observation table with location_id
  
  observation$location_id <- location$location_id[
    match(observation$location_id, location$location_name)
    ]
  
  # Create taxon table --------------------------------------------------------
  
  message('Creating table "taxon"')	
  
  # Initialize table
  
  taxon <- data.frame(
    taxon_id = rep(NA_character_, length(unique(observation$taxon_id))),
    taxon_rank = rep(NA_character_, length(unique(observation$taxon_id))),
    taxon_name = rep(NA_character_, length(unique(observation$taxon_id))),
    authority_system = rep(NA_character_, length(unique(observation$taxon_id))),
    authority_taxon_id = rep(NA_character_, length(unique(observation$taxon_id))),
    stringsAsFactors = F
  )
  
  # Get taxa names
  
  taxa_name_map <- data.frame(taxa_codes = unique(observation$taxon_id),
                              names = c('AMS',
                                        'Andromeda prolifolia',
                                        'Betula nana',
                                        'Carex bigelowii',
                                        'Empetrum nigrum',
                                        'Eriophorum vaginatum',
                                        'Lichen',
                                        'Oxycoccus microcarpus',
                                        'Rubus chamaemorus',
                                        'Rhododendron subarcticum',
                                        'Vaccinium uliginosum',
                                        'Vaccinium vitisidaea',
                                        'Aulucomnium',
                                        'Dicranum',
                                        'FEMO',
                                        'moss',
                                        'Polytrichum',
                                        'Sphagnum'),
                              stringsAsFactors = F)
  
  # Add fields
  
  taxon$taxon_id <- paste0('tx_', seq(nrow(taxon)))
  
  message('Resolving taxa to authorities')
  
  resolved_taxa <- taxonomyCleanr::resolve_sci_taxa(data.sources = c(3,11), 
                                                    x = taxa_name_map$names)
  taxon$taxon_rank <- resolved_taxa$rank
  taxon$taxon_name <- resolved_taxa$taxa
  taxon$authority_system <- resolved_taxa$authority
  taxon$authority_taxon_id <- resolved_taxa$authority_id
  
  # Update observation table with taxon_id
  
  observation$taxon_id <- taxon$taxon_id[
    match(observation$taxon_id, taxa_name_map$taxa_codes)]
  
  # Create dataset summary table ----------------------------------------------
  
  message('Creating table "dataset_summary"')	
  
  dataset_summary <- make_dataset_summary(
    parent.package.id = parent_pkg_id,
    child.package.id = child_pkg_id,
    sample.dates = as.character(observation$observation_datetime),
    taxon.table = taxon
  )
  
  # Create location ancillary table -------------------------------------------
  
  message('Creating table "location_ancillary"')	
  
  # Update data with location_id
  
  data$location_id <- location$location_id[match(data$location_id, location$location_name)]
  
  df <- unique.data.frame(
    data.frame(
      datetime = data$observation_datetime,
      location_id = data$location_id,
      WW = data$WW,
      SW = data$SW,
      stringsAsFactors = F
    )
  )
  
  df <- gather(
    df, 
    'variable_name', 
    'value', 
    WW, 
    SW
  )
  
  # Add fields
  
  df$location_ancillary_id <- paste0('loan_', seq(nrow(df)))
  
  # Select fields
  
  location_ancillary <- select(
    df, 
    location_ancillary_id, 
    location_id, 
    datetime,
    variable_name,
    value
  )
  
  # Create variable mapping table ---------------------------------------------
  
  message('Creating table "variable_mapping"')	
  
  variable_mapping <- make_variable_mapping(
    observation = observation,
    location_ancillary = location_ancillary
  )
  
  # Define 'biomass'
  use_i <- variable_mapping$variable_name == 'biomass'
  variable_mapping$mapped_system[use_i] <- 'BioPortal'
  variable_mapping$mapped_id[use_i] <- 'http://purl.dataone.org/odo/ECSO_00000513'
  variable_mapping$mapped_label[use_i] <- 'Biomass Measurement Type'
  # Define 'WW'
  use_i <- variable_mapping$variable_name == 'WW'
  variable_mapping$mapped_system[use_i] <- 'BioPortal'
  variable_mapping$mapped_id[use_i] <- 'http://purl.dataone.org/odo/ECSO_00000506'
  variable_mapping$mapped_label[use_i] <- 'Manipulative experiment'
  # Define 'SW'
  use_i <- variable_mapping$variable_name == 'SW'
  variable_mapping$mapped_system[use_i] <- 'BioPortal'
  variable_mapping$mapped_id[use_i] <- 'http://purl.dataone.org/odo/ECSO_00000506'
  variable_mapping$mapped_label[use_i] <- 'Manipulative experiment'
  
  # Write tables to file --------------------------------------------------
  
  message('Writing tables to file')	
  
  write.csv(taxon,file=paste0(path,"/",project_name,"_taxon.csv"),row.names=FALSE,quote=FALSE)
  write.csv(observation,file=paste0(path,"/",project_name,"_observation.csv"),row.names=FALSE,quote=FALSE)
  write.csv(location,file=paste0(path,"/",project_name,"_location.csv"),row.names=FALSE,quote=FALSE)
  write.csv(dataset_summary,file=paste0(path,"/",project_name,"_dataset_summary.csv"),row.names=FALSE,quote=FALSE)
  write.csv(location_ancillary,file=paste0(path,"/",project_name,"_location_ancillary.csv"),row.names=FALSE,quote=FALSE)
  write.csv(variable_mapping,file=paste0(path,"/",project_name,"_variable_mapping.csv"),row.names=FALSE,quote=FALSE)
  
}
