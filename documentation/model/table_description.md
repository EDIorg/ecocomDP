# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

The ecocomDP is composed of 7 tables, which can be linked via their indexes, and there are many mechanisms to accomplish this. This document describes the seven tables, and the contents (columns) of each. Examples can be found in the /examples/ directory. The graphic showing all seven data objects and their relationships was created from a relational database implementation (PostgreSQL, see the directory for more information). Of the seven, three are required (“observation”, “sampling_location”, “taxon”). The “dataset_summary” is populated from the “observation” table by code.   The sampling_location and taxon tables are each linked to an optional table for ancillary information.
_____
Table: sampling_location
---
Description:

Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---------------|---------|------------------------|--------------|-----------|
| sampling_location_id | character varying(100) NOT NULL  	| | | |
| sampling_location_name   	|  character varying(500), 	|   	|   	|   	|
|   latitude 	|  float 	|   	|   	|   	|
|   longitude 	|  float 	|   	|   	|   	|
|   elevation	|  float 	|   	|   	|   	|
|   parent_sampling_location_id	|  character varying(100) 	|   	|   	|   	|


Table: taxon
---
Description: 

Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|
| taxon_id | character |  yes|  |
|	taxon_rank | character | | 
|	taxon_name | character |  yes| |
|	authority_system | character | | | 
|	authority_taxon_id | character|  | |


Table: event
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|  
|event_record_id character varying(100) NOT NULL,
|	event_id character varying(100) NOT NULL,
|	variable_name character varying(200),
|	value character varying(200),
|	unit character varying(200)


Table: observation
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|  
|observation_id character varying(100) NOT NULL,
|	event_id record_id character varying(100) NOT NULL,
|	package_id character varying(100) NOT NULL,
|	sampling_location_id character varying(100) NOT NULL,
|	observation_datetime timestamp without time zone,
|	taxon_id character varying(100) NOT NULL,
|	variable_name character varying(200) NOT NULL,
|	value character varying(200) NOT NULL,
|	unit character varying(200) NOT NULL


Table: observation
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|  
|observation_id character varying(100) NOT NULL,
|	event_id record_id character varying(100) NOT NULL,
|	package_id character varying(100) NOT NULL,
|	sampling_location_id character varying(100) NOT NULL,
|	observation_datetime timestamp without time zone,
|	taxon_id character varying(100) NOT NULL,
|	variable_name character varying(200) NOT NULL,
|	value character varying(200) NOT NULL,
|	unit character varying(200) NOT NULL


Table: taxon_ancillary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|      
| taxon_ancillary_id character varying(100) NOT NULL,
|	  taxon_id character varying(100) NOT NULL,
|	  datetime timestamp without time zone NOT NULL,  -- mob: lookup, is timestamp right?
|    variable_name  character varying(200) NOT NULL,
|    value character varying(200) NOT NULL,
|	  author character varying(200)


Table: dataset_summary
---
Description:
Columns

|  column name 	|   type	|   required in table?	|  description 	| example  	|
|---	|---	|---	|---	|---	|  
|package_id integer NOT NULL,
|	original_package_id character varying(200),
|	length_of_survey_years integer NOT NULL,
|	number_of_years_sampled integer NOT NULL,
|	std_dev_interval_betw_years float NOT NULL,
|	max_num_taxa integer NOT NULL, 
|	geo_extent_bounding_box_m2 float

