# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

The ecocomDP is composed of 7 tables, which can be linked via their indexes. This document describes the seven tables, and the contents (columns) of each. Examples can be found in the /examples/ directory. The graphic showing all seven data objects and their relationships was created from a relational database implementation (PostgreSQL, see the directory for more information). Of the seven, three are required (“observation”, “location”, “taxon”). The “dataset_summary” is populated from the “observation” table by code.   The location and taxon tables are each linked to an optional table for ancillary information.

table list and suggested population order, ie, parents first.

|  order | table name 	|   required?	|   references tables    | description            | sample cols (examples)   | unique constraints |
|--------|--------------|-------------|------------------------|------------------------|-------------------------|--------------------|
|1.| location | yes | NA | basic info to identify a place | lon, lat, elev |  location_id | 
|2.| taxon | yes | NA | basic info to identify an organism | name, id from an external system |  taxon_id |
|3.| observation |yes| location, taxon | observations about taxa, that are being analyzed. Eg, organism abundance or density, or the data to compute density (count) | variable, value, unit | observation_id, package_id, location_id, observation_datetime, taxon_id, variable_name   | 
|4.| location_ancillary |no| location | additional info about a place that does not change, in long format. | variable, value, unit (sampling area, lake area, depth of ocean) | location_id, datetime, variable_name |
|5.| taxon_ancillary|no|  taxon | additonal info about an organism that does not change, in long format | variable, value, unit (phenotypic traits) | taxon_id, date_time, variable_name   |
|6.| observation_ancillary  | no | NA | additional info about the sampling event (not related to taxa or locations) in long-format  |  variable_name, value, unit | observation_id, variable_name  |
|7.| dataset_summary|yes|  observation | summary info calculated from incoming data. one line table |See examples directory |   |


_____
Tables
---
Table: location
---
Description: identifying info about a place (lon, lat, elev). table is self-referencing so that sites can be nested. example (for a transect) shows reference to another parent-row.	

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|-------------|---------|
| location_id | character | yes  	      |   NA|   | sbclter_abur_I |
| location_name   	|  character  	| no 	|   NA | 	|  Arroyo Burro Reef, transect I 	|
| latitude 	  |  float 	|   	no                  |   NA |Latitude in decimal degrees. Latitudes south of the equator are negative.|  34.400275	|
| longitude 	|  float 	|   	no                  |   NA |Longitude in decimal degrees. Longitudes west of the prime meridian are negative.| -119.7445915 |
| elevation	  |  float 	|   	np                  |   NA |Elevation in meters.| -15	|
| parent_location_id	|  character | no  	|   NA| 	|  sbclter_abur	|


Table: taxon
---
Description: identifying information about a taxon, eg, name, id and system. When table is constructed, use a lookup service (eg, taxize) to find the ID assigned by a taxon system, like ITIS. 

Columns

|  column name 	|   type	| required in table? |  references cols 	| description | example |
|---------------|---------|--------------------|-------------------|--------------|---------|
| taxon_id           | character |  yes        | NA | ID used in the dataset | sbclter_MAPY |  
|	taxon_rank         | character |  no         | NA | rank of the organism | species |   
|	taxon_name         | character |  yes        | NA | species name | M. pyrifera | 
|	authority_system   | character |  no         | NA | name of the system assigning the ID | ITIS|  
|	authority_taxon_id | character |  no         | NA | ID in that system |  11274|


Table: observation
---
Description: This is the core table - which holds the observations being analyzed, eg, organism abundance or density. observations must be linked to a taxon

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|  
| record_id            | character |yes| NA	| a unique id for this record  	|  4161	 	|
| observation_id       | character |yes|   	|   	|   	|
|	package_id           | character |yes|   	| the ID of this data package  	| edi.100001.1   	|
|	location_id | character |yes| (table = location) location_id |  a reference to a location	|  sbc_ABUR_1 	|
|	observation_datetime | datetime  |yes|   	|Date and time of the observation, following the ISO 8601 standard format YYYY-MM-DDThh:mm+-hh to the precision of datetime data| 2017-08-01 or 2017-08-01T14:01-07  	|
|	taxon_id             | character |yes| (table = taxon) taxon_id  	| reference to a taxon ID  	| sbclter_MAPY   	|
|	variable_name        | character |yes|   	| name of the variable measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  kelp_density  	|
|	value                | float     |yes|   	| value for the variable  	| 7  	|
|	unit                 | character |yes|   	| unit for this variable  	|  numberPerMeterSquared 	|

Table: location_ancillary 
---
Description: additional info about a place that does not change frequently, eg, lake area or depth, experimental treatment. (features that change frequently should be kept in the "observation_ancillary" table 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|-------------|---------| 
|location_ancillary_id | character |yes|   	|   	|   	|
|location_id           | character |yes|(table = location) location_id   	| Id of the location for reference	| sbclter_ABUR_1  	|
|datetime                       | datetime	|no|  | date and time of the ancillary info, ISO datetime	|  experimental treatment date 	| 
|variable_name                  | character |yes|   	|  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList) 	| treatment  	|
|value                          | character |yes|   	|  value for the variable 	| kelp removal  	|
|unit                           | character |no|   	|  unit for this variable 	|   	|



Table: taxon_ancillary
---
Description: additional info about an organism that does not change frequently, eg, trophic level  (features that change frequently are probably observations) 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|     
| record_id          | character |yes| NA | a unique id for this record | auto-generated |  
| taxon_ancillary_id | character |yes|   	              |   	|   	|
| taxon_id           | character |yes| (table = taxon) taxon_id   	|   	|   	|
|	datetime           | datetime  |no|   	              | date and time of the ancillary info, ISO datetime  	|   	|
| variable_name      | character |yes|   	              |  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  trophic_level 	|
| value              | character |yes|   	              |  value for the variable 	|   primary producer	|
| unit               | character |no|   	              | unit for this variable  	|   	|



Table: observation_ancillary 
---
Description: ancillary information about an observational event for context, but that are not about the organism or sampling location. Examples: water depth, height of a tower, temperature of medium. Since this table can hold a variety of measurements, there are 2 additional columns to name the entity and characteristic being measured, both of which can be controlled in external vocabularies or ontologies.  

Columns

|  column name 	|   type	 |   required in table?	|  references cols 	| description | example |
|---------------|----------|-----------------------|-------------------|--------------|---------| 
|	observation_ancillary_id | character |yes| NA	| the id of the observation_ancillary, a row or record identifier 	| 	TBE01JUN05  	|
|	observation_id           | character |yes| NA	| (table = obsewrvation) observation_id                         	| 	  	|
|	variable_name            | character |yes| NA | variable measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  sample_z  	|
|	value                    | character | no| NA	| value for variable  	|  5  	|
|	unit                     | character | no| NA	|  unit for variable 	|  m 	|


Table: dataset_summary
---
Description: summary info about the dataset. some could be elevated to metadata to enhance discovery.

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| package_id                  | character |yes| (table = observation) package_id 	|  id of the L1 pkg (this package)	|  edi.100001.1 	|
|	original_package_id         | character |no|   	|  id of the L0 pkg (original, source) | knb-lter-sbc.21.17 |
|	length_of_survey_years      | integer   |yes|   	|   	| 17  	| 
|	number_of_years_sampled     | integer   |yes|   	|   	| 17  	|
|	std_dev_interval_betw_years | float     |yes|   	|   	| 1.1  	|
|	max_num_taxa                |integer    |yes|   	|   	| 1  	|
|	geo_extent_bounding_box_m2  |float      |no|   	|   	|  40 	|

