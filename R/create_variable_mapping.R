#' Create the variable_mapping table
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param observation_ancillary (tbl_df, tbl, data.frame) The optional observation_ancillary table.
#' @param location_ancillary (tbl_df, tbl, data.frame) The optional location_ancillary table.
#' @param taxon_ancillary (tbl_df, tbl, data.frame) The optional taxon_ancillary table.
#' 
#' @details This function collects specified data tables, extracts unique variable_name values from each, converts into long (attribute-value) form with the table name and variable_name values to the resulting table's "table_name" and "variable_name" columns, respectively. The resulting table's "mapped_system", "mapped_id", and "mapped_label" are filled with \code{NA} and are to be manually filled.
#'
#' @return (tbl_df, tbl, data.frame) The variable_mapping table.
#' 
#' @export
#' 
#' @examples 
#' flat <- ants_L0_flat
#' 
#' # Create inputs to variable_mapping()
#' 
#' observation <- create_observation(
#'   L0_flat = flat, 
#'   observation_id = "observation_id", 
#'   event_id = "event_id", 
#'   package_id = "package_id",
#'   location_id = "location_id", 
#'   datetime = "datetime", 
#'   taxon_id = "taxon_id", 
#'   variable_name = "variable_name",
#'   value = "value",
#'   unit = "unit")
#' 
#' observation_ancillary <- create_observation_ancillary(
#'   L0_flat = flat,
#'   observation_id = "observation_id", 
#'   variable_name = c("trap.type", "trap.num", "moose.cage"))
#' 
#' location_ancillary <- create_location_ancillary(
#'   L0_flat = flat,
#'   location_id = "location_id",
#'   variable_name = "treatment")
#' 
#' taxon_ancillary <- create_taxon_ancillary(
#'   L0_flat = flat,
#'   taxon_id = "taxon_id",
#'   variable_name = c(
#'     "subfamily", "hl", "rel", "rll", "colony.size", 
#'     "feeding.preference", "nest.substrate", "primary.habitat", 
#'     "secondary.habitat", "seed.disperser", "slavemaker.sp", 
#'     "behavior", "biogeographic.affinity", "source"),
#'   unit = c("unit_hl", "unit_rel", "unit_rll"))
#' 
#' # Create variable_mapping table
#' 
#' variable_mapping <- create_variable_mapping(
#'   observation = observation,
#'   observation_ancillary = observation_ancillary,
#'   location_ancillary = location_ancillary, 
#'   taxon_ancillary = taxon_ancillary)
#' 
#' variable_mapping
#' 
create_variable_mapping <- function(observation, 
                                    observation_ancillary = NULL, 
                                    location_ancillary = NULL, 
                                    taxon_ancillary = NULL) {
  
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
  
  # coerce classes
  variable_mapping <- coerce_table_classes(variable_mapping, "variable_mapping", class(observation))
  
  # Return --------------------------------------------------------------------
  
  return(variable_mapping)
  
  
}
