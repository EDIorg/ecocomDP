# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

The ecocomDP is composed of 7 tables, which can be linked via their indexes, and there are many mechanisms to accomplish this. This document describes the seven tables, and the contents (columns) of each. Examples can be found in the /examples/ directory. The graphic showing all seven data objects and their relationships was created from a relational database implementation (PostgreSQL, see the directory for more information). Of the seven, three are required (“observation”, “sampling_location”, “taxon”). The “dataset_summary” is populated from the “observation” table by code.   The sampling_location and taxon tables are each linked to an optional table for ancillary information.

table list and suggested population order, ie, parents first.

|  order | table name 	|   required?	|   references tables    | description            | sample cols, examples   |
|--------|--------------|-------------|------------------------|------------------------|---------------|
|1.| sampling_location | yes | NA | basic info to identify a place | lon, lat, elev | 
|2.| taxon | yes | NA | basic info to identify an organism | name, id from an external system | 
|3.| event  | no | NA | info about the sampling event, in long-format |  variable, value, unit | 
|4.| observation |yes| sampling_location, taxon, event | the obs that are being analyzed, eg, organism abundance or density | variable, value, unit|   
|5.| sampling_location_ancillary |no| sampling_location | additional info about a place that does not change, eg, lake area, depth of ocean | See examples directory  |
|6.| taxon_ancillary|no|  taxon | additonal info about an organism that does not change, eg, phenotypic traits | | 
|7.| dataset_summary|no|  observation | summary info calculated from incoming data. one line/dataset |See examples directory | 


_____
Tables
---
Table: sampling_location
---
Description: identifying info about a place (lon, lat, elev). table is self-referencing so that sites can be nested. example (for a transect) shows reference to another parent row.	

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| sampling_location_id | character | yes  	      |   NA|   | sbclter_abur_I |
| sampling_location_name   	|  character  	| no 	|   NA | 	|  Arroyo Burro Reef, transect I 	|
| latitude 	  |  float 	|   	no                  |   NA |	|  34.400275	|
| longitude 	|  float 	|   	no                  |   NA | 	| -119.7445915 |
| elevation	  |  float 	|   	np                  |   NA | 	| -15	|
| parent_sampling_location_id	|  character | no  	|   NA| 	|  sbclter_abur	|


Table: taxon
---
Description: identifying information about a taxon, eg, name, id and system. When table is constructed, use a lookup service (eg, taxize) to find the ID assigned by a taxon system, like ITIS. 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| taxon_id           | character |  yes           | NA | ID used in the dataset | sbclter_MAPY |  
|	taxon_rank         | character |  no            | NA | rank of the organism | species |   
|	taxon_name         | character |  yes           | NA | species name | M. pyrifera | 
|	authority_system   | character |  no            | NA | name of the system assigning the ID | ITIS|  
|	authority_taxon_id | character |  no            | NA | ID in that system |  11274|


Table: event 
---
Description: addition info about an observation event for context, eg, water depth, height of a tower, temperature of medium 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|event_record_id | character |yes| NA	| a unique id for this record that can be reference in other tables   	|  4161	 	|
|	event_id       | character |yes| NA	| the id of the event, may be repeated  	| 	TBE01JUN05  	|
|	variable_name  | character | no| NA| variable measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  depth  	|
|	value          | character | no| NA	| value for variable  	|  5  	|
|	unit           | character | no| NA	|  unit for variable 	|  m 	|


Table: observation
---
Description: This is the core table - which holds the obs being analyzed, eg, organism abundance or density	

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|  
| observation_id       | character |yes|   	|   	|   	|
|	event_record_id      | character |no| (table=event) event_record_id 	| a reference to ane event  	|   	|
|	package_id           | character |yes|   	| the ID of this data package  	| edi.100001.1   	|
|	sampling_location_id | character |yes| (table=sampling_location) sampling_location_id |  a reference to a location	|  sbc_ABUR_1 	|
|	observation_datetime | datetime  |yes|   	|Date and time of the observation, following the ISO 8601 standard format YYYY-MM-DDThh:mm+-hh to the precision of datetime data| 2017-08-01 or 2017-08-01T14:01-07  	|
|	taxon_id             | character |yes| (table=taxon) taxon_id  	| reference to a taxon ID  	| sbclter_MAPY   	|
|	variable_name        | character |yes|   	| variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  NPP_carbon  	|
|	value                | float |yes|   	| value for the variable  	| 7  	|
|	unit                 | character |yes|   	| unit for this variable  	|  kilogramPerMeterSquaredPerDay 	|


Table: sampling_location_ancillary 
---
Description: additional info about a place that does not change frequently, eg, lake area or depth, experimental treatment. (features that change frequently should be kept in the "event" table 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|sampling_location_ancillary_id | character |yes|   	|   	|   	|
|sampling_location_id           | character |yes|(table=sampling_location) sampling_location_id   	| Id of the location for reference	| sbclter_ABUR_1  	|
|datetime                       | datetime	|no|  | date and time of the ancillary info, ISO datetime	|  experimental treatment date 	| 
|variable_name                  | character |yes|   	|  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList) 	| treatment  	|
|value                          | character |yes|   	|  value for the variable 	| kelp removal  	|
|unit                           | character | 	|   	|  unit for this variable 	|   	|



Table: taxon_ancillary
---
Description: additional info about an organism that does not change frequently, eg, trophic level  (features that change frequently are probably observations) 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|     
| taxon_ancillary_id | character |yes|   	              |   	|   	|
| taxon_id           | character |yes| (table = taxon) taxon_id   	|   	|   	|
|	datetime           | datetime | 	 |   	              | date and time of the ancillary info, ISO datetime  	|   	|
| variable_name      | character |yes|   	              |  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  trophic_level 	|
| value              | character |yes|   	              |  value for the variable 	|   primary producer	|
|author              | character | 	 |   	              | unit for this variable  	|   	|


Table: dataset_summary
---
Description: summary info about the dataset. some could be elevated to metadata to enhance discovery.

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| package_id                  | character |yes| observation.package_id  	|  id of the L1 pkg (this package)	|  edi.100001.1 	|
|	original_package_id         | character |yes|   	|   	 id of the L0 pkg (original, source)	| knb-lter-sbc.21.17 |
|	length_of_survey_years      | integer   |yes|   	|   	| 17  	| 
|	number_of_years_sampled     | integer   |yes|   	|   	| 17  	|
|	std_dev_interval_betw_years | float     |yes|   	|   	| 1.1  	|
|	max_num_taxa                |integer    |yes|   	|   	| 1  	|
|	geo_extent_bounding_box_m2  |float      | 	|   	|   	|  40 	|

