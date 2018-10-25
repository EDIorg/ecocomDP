#' Make variable_mapping table
#'
#' @description  
#'     Make a data frame of the variable mapping table.
#'
#' @usage 
#'     make_variable_mapping(observation, location, taxon, dataset_summary,
#'     observation_ancillary = NULL, location_ancillary = NULL, taxon_ancillary = NULL)
#'
#' @param observation
#'     (data frame) The completed observation table.
#' @param location
#'     (data frame) The completed location table.
#' @param taxon
#'     (data frame) The completed taxon table.
#' @param dataset_summary
#'     (data frame) The completed dataset_summary table.
#' @param observation_ancillary
#'     (data frame) The completed observation_ancillary table.
#' @param location_ancillary
#'     (data frame) The completed location_ancillary table.
#' @param taxon_ancillary
#'     (data frame) The completed taxon_ancillary table.
#'
#' @return 
#'     A data frame of the variable_mapping with completed columns:
#'     \itemize{
#'         \item{variable_mapping_id}
#'         \item{table_name}
#'         \item{variable_name}
#'     }
#'     Other columns are filled with NA.
#'     
#' @export
#'

make_variable_mapping <- function(observation, location, taxon, dataset_summary, 
                                  observation_ancillary = NULL, location_ancillary = NULL, taxon_ancillary = NULL){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(observation)){
    stop('Input argument "observation" is missing!')
  }
  if (missing(location)){
    stop('Input argument "location" is missing!')
  }
  if (missing(taxon)){
    stop('Input argument "taxon" is missing!')
  }
  if (missing(dataset_summary)){
    stop('Input argument "dataset_summary" is missing!')
  }
  
  # Get unique variables from tables ------------------------------------------
  
  # Initialize storage
  
  vars <- c()
  tbls <- c()
  
  # Add observation variables
  
  vars <- c(vars, unique(observation$variable_name))
  tbls <- c(tbls, rep('observation', length(unique(observation$variable_name))))
  
  # Add observation ancillary variables
  
  if (!missing(observation_ancillary)){
    vars <- c(vars, unique(observation_ancillary$variable_name))
    tbls <- c(tbls, rep('observation_ancillary', length(unique(observation_ancillary$variable_name))))
  }
  
  # Add location ancillary variables
  
  if (!missing(location_ancillary)){
    vars <- c(vars, unique(location_ancillary$variable_name))
    tbls <- c(tbls, rep('location_ancillary', length(unique(location_ancillary$variable_name))))
  }
  
  # Add taxon ancillary variables
  
  if (!missing(taxon_ancillary)){
    vars <- c(vars, unique(taxon_ancillary$variable_name))
    tbls <- c(tbls, rep('taxon_ancillary', length(unique(taxon_ancillary$variable_name))))
  }
  
  # Create variable_mapping data frame ----------------------------------------
  
  # Initialize data frame
  
  variable_mapping <- data.frame(
    variable_mapping_id = rep(NA_character_, length(vars)),
    table_name = rep(NA_character_, length(vars)),
    variable_name = rep(NA_character_, length(vars)),
    mapped_system = rep(NA_character_, length(vars)),
    mapped_id = rep(NA_character_, length(vars)),
    mapped_label = rep(NA_character_, length(vars)),
    stringsAsFactors = FALSE
  )
  
  # Add content
  
  variable_mapping$variable_mapping_id <- paste0(
    'vama_',
    seq(length(vars))
    )
  
  variable_mapping$table_name <- tbls
  
  variable_mapping$variable_name <- vars
  
  # Return --------------------------------------------------------------------
  
  variable_mapping
  
  
}
