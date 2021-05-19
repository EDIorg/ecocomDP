#' Create the variable_mapping table
#'
#' @param observation (data.frame) The observation table.
#' @param observation_ancillary (data.frame) The optional observation_ancillary table.
#' @param location_ancillary (data.frame) The optional location_ancillary table.
#' @param taxon_ancillary (data.frame) The optional taxon_ancillary table.
#' 
#' @details This function collects specified data tables, extracts unique variable_name values from each, converts into long (attribute-value) form with the table name and variable_name values to the resulting table's "table_name" and "variable_name" columns, respectively. The resulting table's "mapped_system", "mapped_id", and "mapped_label" are filled with \code{NA} and are to be manually filled.
#'
#' @return (data.frame) The variable_mapping table.
#' 
#' @export
#' 
#' @examples 
#'
create_variable_mapping <- function(observation, 
                                    observation_ancillary = NULL, 
                                    location_ancillary = NULL, 
                                    taxon_ancillary = NULL) {
  
  message("Creating variable_mapping")
  
  # Get unique variables from tables ------------------------------------------
  
  # Initialize storage
  
  vars <- c()
  tbls <- c()
  
  # Add observation variables
  
  vars <- c(vars, unique(as.character(observation$variable_name)))
  tbls <- c(tbls, rep('observation', length(unique(observation$variable_name))))
  
  # Add observation ancillary variables
  
  if (!is.null(observation_ancillary)){
    vars <- c(vars, unique(as.character(observation_ancillary$variable_name)))
    tbls <- c(tbls, rep('observation_ancillary', length(unique(observation_ancillary$variable_name))))
  }
  
  # Add location ancillary variables
  
  if (!is.null(location_ancillary)){
    vars <- c(vars, unique(as.character(location_ancillary$variable_name)))
    tbls <- c(tbls, rep('location_ancillary', length(unique(location_ancillary$variable_name))))
  }
  
  # Add taxon ancillary variables
  
  if (!is.null(taxon_ancillary)){
    vars <- c(vars, unique(as.character(taxon_ancillary$variable_name)))
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
  
  variable_mapping$variable_mapping_id <- seq(length(vars))
  
  variable_mapping$table_name <- tbls
  
  variable_mapping$variable_name <- vars
  
  # Return --------------------------------------------------------------------
  
  return(variable_mapping)
  
  
}
