# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

The ecocomDP is composed of 7 tables, which can be linked via their indexes, and there are many mechanisms to accomplish this. This document describes the seven tables, and the contents (columns) of each. Examples can be found in the /examples/ directory. The graphic showing all seven data objects and their relationships was created from a relational database implementation (PostgreSQL, see the directory for more information). Of the seven, three are required (“observation”, “sampling_location”, “taxon”). The “dataset_summary” is populated from the “observation” table by code.   The sampling_location and taxon tables are each linked to an optional table for ancillary information.

table list and suggested population order, ie, parents first.

|  order | table name 	|   required?	|   references tables    | description            | example |
|--------|--------------|-------------|------------------------|------------------------|---------------|
|1.| sampling_location | yes
|2.| taxon | yes
|3.| event  | yes
|4.| observation |no| sampling_location, taxon, event
|5.| sampling_location_ancillary |no| sampling_location
|6.| taxon_ancillary|no|  taxon
|7.| dataset_summary|no|  observation


_____
Tables
---
Table: sampling_location
---
Description:

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| sampling_location_id | character | yes  	| | |
| sampling_location_name   	|  character  	|   	|   	|   	|
|   latitude 	|  float 	|   	|   	|   	|
|   longitude 	|  float 	|   	|   	|   	|
|   elevation	|  float 	|   	|   	|   	|
|   parent_sampling_location_id	|  character  	|   	|   	|   	|


Table: taxon
---
Description: 

Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| taxon_id | character |  yes|  |
|	taxon_rank | character | | 
|	taxon_name | character |  yes| |
|	authority_system | character | | | 
|	authority_taxon_id | character|  | |


Table: event
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|event_record_id | character |yes 	|   	|   	|   	|
|	event_id | character |yes  	|   	|   	|   	|
|	variable_name |character | 	|   	|   	|   	|
|	value | character | 	|   	|   	|   	|
|	unit | character | 	|   	|   	|   	|


Table: observation
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|  
|observation_id | character |yes 	|   	|   	|   	|
|	event_id record_id | character |yes 	|   	|   	|   	|
|	package_id | character |yes 	|   	|   	|   	|,
|	sampling_location_id character |yes 	|   	|   	|   	|
|	observation_datetime | datetime | 	|   	|   	|   	|
|	taxon_id | character |yes 	|   	|   	|   	|
|	variable_name | character |yes 	|   	|   	|   	|
|	value | character |yes 	|   	|   	|   	|
|	unit | character | 	|   	|   	|   	|


Table: sampling_location_ancillary 
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------| 
|sampling_location_ancillary_id character |yes 	|   	|   	|   	|
|    sampling_location_id| character |yes 	|   	|   	|   	|
|    datetime| timestamp  | 	|   	|   	|  experimental treatment date 	| 
|    variable_name |  character |yes 	|   	|   	|   	|
|    value | character |yes 	|   	|   	|   	|
|    unit | character  | 	|   	|   	|   	|



Table: taxon_ancillary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|     
| taxon_ancillary_id character |yes 	|   	|   	|   	|
|	  taxon_id | character |yes 	|   	|   	|   	|
|	  datetime| timestamp | 	|   	|   	|   	|
|    variable_name | character |yes 	|   	|   	|   	|
|    value | character |yes 	|   	|   	|   	|
|	  author| character | 	|   	|   	|   	|


Table: dataset_summary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
|package_id | integer |yes 	|   	|   	|   	|
|	original_package_id | character |yes 	|   	|   	|   	|
|	length_of_survey_years | integer |yes 	|   	|   	|   	|
|	number_of_years_sampled | integer |yes 	|   	|   	|   	|
|	std_dev_interval_betw_years| float |yes 	|   	|   	|   	|
|	max_num_taxa |integer |yes 	|   	|   	|   	|
|	geo_extent_bounding_box_m2 |float | 	|   	|   	|   	|

