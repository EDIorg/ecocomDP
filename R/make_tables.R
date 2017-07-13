#' Make ecocomDP tables
#'
#' @description  
#'     Translate user supplied data tables to the Ecological Community Data
#'     Pattern (ecocomDP) then write to file.
#'
#' @usage 
#'     make_ecocomDP_tables(path)
#'
#' @param path 
#'     A path to the dataset working directory containing the level-0 (raw)
#'     data tables and the level-0 to level-1 mapping (map.txt).
#'
#' @return 
#'     \strong{taxon.csv} A comma delimited UTF-8 file containing taxonomic data.
#'     A taxon is a unique taxonomic entity recorded at some taxon rank. Each
#'     taxon should be associated with an authority, which can be a citation or
#'     or URL. An authority taxon id (such as an ITIS TSN) should be included
#'     if available. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/taxon.html}{Click here to view the schema for taxon.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{taxon_id} Identifier assigned to each unique entry,
#'         most likely code-generated
#'         \item \strong{taxon_level} Taxon rank (e.g., species, genus, family,
#'         order, etc.)
#'         \item \strong{taxon_name} Taxon name
#'         \item \strong{authority_system} Citation or URL of the taxonomic 
#'         authority or registry, e.g., TSN or ITIS
#'         \item \strong{authority_taxon_id} Identifier in the authority 
#'         reference, e.g., an ITIS number
#'     }
#'     
#' @return 
#'     \strong{taxon_ancillary.csv} A comma delimited UTF-8 file containing 
#'     ancillary taxonomic data. A taxon may have ancillary information, such
#'     as habitat, plant lifeform, reproductive status, or other descriptive
#'     information. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/taxon_ancillary.html}{Click here to view the schema for taxon_ancillary.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{taxon_ancillary_id} Identifier assigned to each unique
#'         entry, most likely code-generated
#'         \item \strong{taxon_id} Taxon_id from taxon table that links the tables
#'         \item \strong{datetime} Datetime value for this entry (may be automatic)
#'         \item \strong{variable_name} Variable name, e.g., "life_history_stage"
#'         \item \strong{value} Variable value, e.g., "juvenile"
#'         reference, e.g., an ITIS number.
#'         \item \strong{author} Observer identification, e.g., "Jane Scientist"
#'     }
#'     
#' @return 
#'     \strong{sampling_location.csv} Sampling location is a plot, transect, 
#'     quadrat, or other sampling location. One sampling location can be nested
#'     within another, and nested sampling locations are defined by a recursive 
#'     relationship (self-referencing table). \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/sampling_location.html}{Click here to view the schema for sampling_location.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{sampling_location_id} Identifier assigned to each
#'         unique entry, may be code-generated
#'         \item \strong{sampling_location_name} Sampling location full name
#'         \item \strong{latitude} Latitude of the sampling location in decimal 
#'         degrees, datum = WGS84
#'         \item \strong{longitude} Longitude of the sampling location in 
#'         decimal degrees, datum = WGS84
#'         \item \strong{elevation} Sampling location elevation in meters above 
#'         sea level
#'         \item \strong{parent_sampling_location_id} Sampling_location_id from 
#'         this table for the parent of this sampling location; indicates 
#'         nested locations
#'     }
#'     
#' @return 
#'     \strong{sampling_location_ancillary.csv} Each sampling location may have
#'     ancillary information associated with it. Ancillary information includes
#'     treatments, disturbance, history, soils, vegetation, geology, or other 
#'     descriptive information. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/sampling_location_ancillary.html}{Click here to view the schema for sampling_location_ancillary.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{sampling_location_ancillary_id} Identifier assigned to
#'         each unique entry, most likely code-generated
#'         \item \strong{sampling_location_id} Sampling location id from 
#'         sampling_location table that links the tables
#'         \item \strong{datetime} Year or date associated with the ancillary
#'         information, e.g., date of a treatment or disturbance, etc.
#'         \item \strong{variable_name} Variable name, e.g., treatment, 
#'         vegetation_description, history
#'         \item \strong{value} Variable value, e.g., burned, treeline, 
#'         precommercial thin
#'         \item \strong{unit} Unit of measurement, optional. Reference the 
#'         standard unit dictionary for options. Enter the value found in the
#'         \emph{id} field. If unit is not available, add your custom unit
#'         following the syntax in the \emph{id} field of the standard unit
#'         dictionary.
#'     }
#'     
#' @return 
#'     \strong{event.csv} Holds ancillary information associated with an 
#'     observation, such as the name of the observer, environmental 
#'     characteristics, or other descriptive information. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/event.html}{Click here to view the schema for event.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{event_id} Identifier assigned to each unique entry,
#'         most likely code-generated.
#'         \item \strong{variable_name} Name of variables, e.g., observer, 
#'         weather_conditions
#'         \item \strong{value} Value for the event variable, e.g., calm
#'     }
#'     
#' @return 
#'     \strong{dataset_summary.csv} The dataset summary table contains 
#'     summary statistics that are calculated from the observation table. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/dataset_summary.html}{Click here to view the schema for dataset_summary.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{dataset_summary_id} Identifier assigned to each unique
#'         dataset_summary, most likely code-generated
#'         \item \strong{original_dataset_id} Identifier of the original 
#'         dataset
#'         \item \strong{length_of_survey_years} Number of years the study has 
#'         been ongoing; calculated from data on import
#'         \item \strong{number_of_years_sampled} Number of years within period 
#'         of the study that samples were taken; calculated from the data on 
#'         import
#'         \item \strong{std_dev_interval_betw_years} Standard deviation of the
#'         interval between sampling events; calculated from the data on 
#'         import. A larger SD indicates a more variable sampling interval.
#'         \item \strong{max_num_taxa} Number of unique values in the taxon 
#'         table. Approximate, and probably a maximum, as it may include terms 
#'         like "rock", "bare ground" etc.
#'         \emph{geo_extent_bounding_box_m2} Area in meters squared of the 
#'         study area; calculated from the data on import.
#'     }
#'     
#' @return 
#'     \strong{observation.csv} Each observation records an observation or 
#'     measurement of one taxon at one sampling location at one time. This is
#'     the central table, and links to taxa, event, and summary. \href{https://htmlpreview.github.io/?https://raw.githubusercontent.com/EDIorg/ecocomDP/master/postgreSQL/html/tables/observation.html}{Click here to view the schema for observation.csv}.
#'     Column names and definitions:
#'     \itemize{
#'         \item \strong{observation_id} Identifier assigned to each unique 
#'         entry, most likely code-generated.
#'         \item \strong{event_id} Id from event table that links the tables
#'         \item \strong{dataset_summary_id} Dataset summary id from the 
#'         dataset_summary table that links the tables
#'         \item \strong{sampling_location_id} Sampling location id from the 
#'         sampling_location table that links the tables
#'         \item \strong{observation_datetime} Date and time that the 
#'         observation
#'         \item \strong{taxon_id} Taxon id from the taxon table that links 
#'         the tables
#'         \item \strong{variable_name} Name of the variable being measured
#'         \item \strong{value} Observation value
#'         \item \strong{unit} Observation unit of measurement
#'         
#'     }
#'     
#'     
#' @export     
#'     


make_tables <- function(path){
  
  
  
  
}
