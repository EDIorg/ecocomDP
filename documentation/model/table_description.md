# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

The ecocomDP is composed of 7 tables, which can be linked via their indexes, and there are many mechanisms to accomplish this. This document describes the seven tables, and the contents (columns) of each. Examples can be found in the /examples/ directory. The graphic showing all seven data objects and their relationships was created from a relational database implementation (PostgreSQL, see the directory for more information). Of the seven, three are required (“observation”, “sampling_location”, “taxon”). The “dataset_summary” is populated from the “observation” table by code.   The sampling_location and taxon tables are each linked to an optional table for ancillary information.

table list and suggested population order, ie, parents first.

|  order | table name 	|   required?	|   references tables    | description            | example content |
|--------|--------------|-------------|------------------------|------------------------|---------------|
|1.| sampling_location | yes | NA | lon, lat, elev |-119.7445915,34.400275, -15 | 
|2.| taxon | yes | NA | name, taxon-identifier from a system | M. pyrifera, ITIS, 11274| 
|3.| event  | yes | NA | info about the sampling event | water depth, height of a tower, temperature of medium | 
|4.| observation |no| sampling_location, taxon, event | the obs that are being analyzed |organism abundance, density |  
|5.| sampling_location_ancillary |no| sampling_location | general info about a place that does not change | See examples directory, lake area, depth of ocean |
|6.| taxon_ancillary|no|  taxon | general info about an organism that does not change | phenotypic traits |
|7.| dataset_summary|no|  observation | summary info calculated from incoming data. one line/dataset |See examples directory | 


_____
Tables
---
Table: sampling_location
---
Description: basic info about a place (lon, lat, elev). table is self-referencing so that sites can be nested. example (for a transect) shows reference to another parent row.	

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
Description: 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| taxon_id           | character |  yes           |  |
|	taxon_rank         | character |                | 
|	taxon_name         | character |  yes           | |
|	authority_system   | character |                | | 
|	authority_taxon_id | character |                | |


Table: event
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|event_record_id | character |yes|   	|   	|   	|
|	event_id       | character |yes|   	|   	|   	|
|	variable_name  |character  | 	 |   	|   	|   	|
|	value          | character | 	 |   	|   	|   	|
|	unit           | character | 	 |   	|   	|   	|


Table: observation
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|  
|observation_id        | character |yes|   	|   	|   	|
|	event_record_id      | character |yes| event_record.event_record_id 	|   	|   	|
|	package_id           | character |yes|   	|   	|   	|
|	sampling_location_id | character |yes| sampling_location.sampling_location_id  	|   	|   	|
|	observation_datetime | datetime  | 	 |   	|date and time of the observation, ISO datetime |   	|
|	taxon_id             | character |yes| taxon.taxon_id  	|   	|   	|
|	variable_name        | character |yes|   	|   	|   	|
|	value                | character |yes|   	|   	|   	|
|	unit                 | character | 	 |   	|   	|   	|


Table: sampling_location_ancillary 
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|sampling_location_ancillary_id | character |yes|   	|   	|   	|
|sampling_location_id           | character |yes|sampling_location.sampling_location_id   	|   	|   	|
|datetime                       | timestamp	|   |  | date and time of the ancillary info, ISO datetime	|  experimental treatment date 	| 
|variable_name                  | character |yes|   	|   	|   	|
|value                          | character |yes|   	|   	|   	|
|unit                           | character | 	|   	|   	|   	|



Table: taxon_ancillary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|     
| taxon_ancillary_id | character |yes|   	              |   	|   	|
| taxon_id           | character |yes| taxon.taxon_id   	|   	|   	|
|	datetime           | timestamp | 	 |   	              |   	|   	|
| variable_name      | character |yes|   	              |   	|   	|
| value              | character |yes|   	              |   	|   	|
|author              | character | 	 |   	              |   	|   	|


Table: dataset_summary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| package_id                  | integer   |yes| observation.package_id  	|  id of the L1 pkg (this package)	|   	|
|	original_package_id         | character |yes|   	|   	 id of the L0 pkg (original, source)	| |
|	length_of_survey_years      | integer   |yes|   	|   	|   	|
|	number_of_years_sampled     | integer   |yes|   	|   	|   	|
|	std_dev_interval_betw_years | float     |yes|   	|   	|   	|
|	max_num_taxa                |integer    |yes|   	|   	|   	|
|	geo_extent_bounding_box_m2  |float      | 	|   	|   	|   	|

