# Table descriptions, relationships, and requirements

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. 
For information on the process see https://edirepository.org/resources/thematic-standardization

The ecocomDP is composed of 8 tables, which can be linked via identifiers. This document describes the tables and their contents (columns). Examples of tables can be found in data package [edi.193](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=192). The graphic showing all data objects and their relationships was created from a relational database implementation (PostgreSQL, see the top-level directory of that name for DDL). Four tables are required ("observation", "location", "taxon", and "dataset_summary"). The "dataset_summary" is populated from the "observation" table by code.  Two tables hold links to external resources: taxon, and variable_mapping. The taxon table is specifically designed to hold lookup-info from a service such as ITIS. The variable_mapping table hold URIs and lables for external measurement dictionaries.  

Each main table has an optional ancillary table for additional information. These are included because primary research typically includes related measurements which may be of interest during anlaysis. 

Below is the table list and suggested population order, ie, primary tables first.

|  order | table name 	|   required?	|   references tables    | description            | sample cols (examples)   | unique constraints |
|--------|--------------|-------------|------------------------|------------------------|-------------------------|--------------------|
|1.| location | yes | NA | basic info to identify a place | lon, lat, elev |  location_id | 
|2.| taxon | yes | NA | basic info to identify an organism | name, id from an external system |  taxon_id |
|3.| observation |yes| location, taxon, observation_ancillary, dataset_summary | observations about taxa, that are being analyzed. Eg, organism abundance or density, or the data to compute density (count) | variable, value, unit | observation_id, package_id, location_id, datetime, taxon_id, variable_name   | 
|4.| location_ancillary |no| location | additional info about a place that does not change, in long format. | variable, value, unit (sampling area, lake area, depth of ocean) | location_id, datetime, variable_name |
|5.| taxon_ancillary|no|  taxon | additonal info about an organism that does not change, in long format | variable, value, unit (phenotypic traits) | taxon_id, date_time, variable_name   |
|6.| observation_ancillary  | no | observation | additional info about the sampling event (not related to taxa or locations) in long-format  |  variable_name, value, unit | event_id, variable_name  |
|7.| dataset_summary|yes|  NA | summary info calculated from incoming data. one line table |See examples directory |   |
|8.| variable_mapping| no |  ONE OF: observation, observation_ancillary, taxon_ancillary, OR location_ancillary | mappings from variable names in tables to external dictionaries | table_name, variable_name, mapped_system, mapped_id, mapped_label | NA   |


_____
Tables
---
Table: location
---
Description: identifying information about a place (lonitude, latitude, elevation). The table is self-referencing so that sites can be nested. The example is for a transect, with a reference to another parent-row.	

Columns

|  column name 	|   type	|   not NULL required?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|-------------|---------|
| location_id | character | yes  	      |   NA| Identifier assigned to each unique entry. | sbclter_abur_I |
| location_name   	|  character  	| no 	|   NA | Sampling location full name.	|  Arroyo Burro Reef, transect I 	|
| latitude 	  |  float 	|   	no                  |   NA |Latitude in decimal degrees. Latitudes south of the equator are negative.|  34.400275	|
| longitude 	|  float 	|   	no                  |   NA |Longitude in decimal degrees. Longitudes west of the prime meridian are negative.| -119.7445915 |
| elevation	  |  float 	|   	np                  |   NA |Sampling location elevation in meters relative to sea level. Above sea level is positive. Below sea level is negative.| -15	|
| parent_location_id	|  character | no  	|   NA| Sampling location identifier from this table for the parent of this sampling location. Presence indicates nested locations.	|  sbclter_abur_I	|


Table: taxon
---
Description: identifying information about a taxon, eg, name, id and system. When table is constructed, use a lookup service (eg, taxize) to find the ID assigned by a taxon system, like ITIS. 

Columns

|  column name 	|   type	| not NULL required? |  references cols 	| description | example |
|---------------|---------|--------------------|-------------------|--------------|---------|
| taxon_id           | character |  yes        | NA | ID used in the dataset | sbclter_MAPY |  
|	taxon_rank         | character |  no         | NA | Taxonomic rank of the organism name | species |   
|	taxon_name         | character |  yes        | NA | Taxonomic name of the organism | Macrocystis pyrifera | 
|	authority_system   | character |  no         | NA | Name of the system assigning the taxon ID | ITIS|  
|	authority_taxon_id | character |  no         | NA | ID in the authority system |  11274|


Table: observation
---
Description: This is the core table - which holds the observations being analyzed, eg, organism abundance or density. Observations must be linked to a taxon and to a location. Linking to ancillary observations (via event_id) is optional, and event_id is required to be populated only if the observation_ancillary table is created.

Columns

|  column name 	|   type	|   not NULL required?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|  
| observation_id         | character |yes|   	|  A unique id for this record      |  4161   	|
| event_id             | character |yes|(table = observation_ancillary) event_id    | The ID of the sampling event, required if observation_ancillary table is included   | 2009mar03_dive1      |
| package_id           | character |yes|(table = summary) package_id   	| The ID of this data package  	| edi.100001.1   	|
| location_id | character |yes| (table = location) location_id |  A reference to a location	|  sbc_ABUR_1 	|
| datetime | datetime  |yes|   	|Date and time of the observation following the ISO 8601 standard format [see here for details](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/datetime.md)| 2017-08-01, 2017-08-01T14:01-07, etc.  	|
| taxon_id             | character |yes| (table = taxon) taxon_id  	| reference to a taxon ID  	| sbclter_MAPY   	|
| variable_name        | character |yes|   	| name of the variable measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  kelp_density  	|
| value                | float     |yes|   	| value for the variable  	| 7  	|
| unit                 | character |yes|   	| unit for this variable  	|  numberPerMeterSquared 	|

Table: location_ancillary 
---
Description: additional information about a place that does not change frequently, e.g., lake area or depth, experimental treatment. Features that change frequently are probably more closely related to the observational event, and so should be kept in the "observation_ancillary" table. Ancillary observations are linked through the location_id, and one location_id may have many ancillary observations about it.

Columns

|  column name 	|   type	|   not NULL required?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|-------------|---------| 
|location_ancillary_id | character |yes|          |   Identifier of the sampling location ancillary.	|   	|
|location_id           | character |yes|(table = location) location_id   	| Id of the location for reference	| sbclter_ABUR_1  	|
|datetime              | datetime  |no |          | Date and time of ancillary info following the ISO 8601 standard format [see here for details](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/datetime.md)	|  experimental treatment date 	| 
|variable_name         | character |yes|   	      |  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList) 	| treatment  	|
|value                 | character |yes|   	      |  value for the variable 	| kelp removal  	|
|unit                  | character |no |   	      |  unit for this variable 	|   	|



Table: taxon_ancillary
---
Description: additional info about an organism that does not change frequently, e.g., trophic level. Features that change frequently are probably observations. Ancillary observations are linked through the taxon_id, and one taxon_id may have many ancillary observations about it.

Columns

|  column name 	|   type	|   not NULL required?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|     
| taxon_ancillary_id | character |yes|            | a unique id for this record  	|   	|
| taxon_id           | character |yes| (table = taxon) taxon_id   	|  The ID of the taxon table. 	|   	|
| datetime           | datetime |no|            | Date and time of the ancillary info following the ISO 8601 standard format [see here for details](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/datetime.md) 	|   	|
| variable_name      | character |yes|            |  variable that was measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  trophic_level 	|
| value              | character |yes|            |  value for the variable 	|   primary producer	|
| unit               | character |no |            | unit for this variable  	|   	|
| author             | character | no |           | Author associated with taxon identification for this dataset |    |



Table: observation_ancillary 
---
Description: ancillary information about an observational event for context, but that are not about the organism or sampling location. Examples: water depth, height of a tower, temperature of medium. These are very often "environmental driver" data in analyses.

This table can hold a variety of measurements, and the relationship to the observation table is "many-to-many", that is, there may be many core observation and many ancillary observations collected during the same event (unlike the relationships in the other two ancillary-table pairs, which are one-to-many). For this reason, the relationship linking the event_id in the two tables is depicted graphically with a dotted line [see ecocomDP.png](ecocomDP.png)). Recommendations for handling or joining observation and observation_ancillary can be found in the [instructions](../instructions).

Columns

|  column name 	|   type	 |   not NULL required?	|  references cols 	| description | example |
|---------------|----------|-----------------------|-------------------|--------------|---------| 
| observation_ancillary_id | character |yes| NA	| the id of the observation_ancillary, a row or record identifier 	| 	TBE01JUN05  	|
| observation_id             | character |yes|(table = observation) observation_id    | The ID of the taxon table.   | 4161      |
| variable_name            | character |yes| NA | variable measured. in EML metadata, these should be code-def pairs (enumeratedList)  	|  sample_z  	|
| value                    | character | no| NA	| value for variable  	|  5  	|
| unit                     | character | no| NA	|  unit for variable 	|  m 	|


Table: dataset_summary
---
Description: summary info about the dataset. Information could be elevated to metadata to enhance discovery.

Columns

|  column name 	|   type	|   not NULL required?	|  references cols 	| description | example |
|---------------|---------|-----------------------|-------------------|--------------|---------|
| package_id                  | character |yes| (table = observation) package_id 	|  id of the L1 pkg (this package)	|  edi.100001.1 	|
| original_package_id         | character |no|   	|  id of the L0 pkg (original, source) | knb-lter-sbc.21.17 |
| length_of_survey_years      | integer   |yes|   	|   Number of years the study has been ongoing.	| 17  	| 
| number_of_years_sampled     | integer   |yes|   	|   Number of years within the period of the study that samples were taken.	| 17  	|
| std_dev_interval_betw_years | float     |yes|   	|  Standard deviation of the interval between sampling events. 	| 1.1  	|
| max_num_taxa                |integer    |yes|   	|  Number of unique values in the taxon table. 	| 1  	|
| geo_extent_bounding_box_m2  |float      |no|   	|   Area of the study location.	|  40 	|




Table: variable_mapping 
---
Description: Information linking a variable_name used in a data table to an external definition

This optional table holds mappings (or relations) between variable names in the data tables and measurement definitions external to the data package. This table has multiple uses: 

- provides definitions for variables in the datasets (more extensive than might be found in metadata) 
- code can use this table to  to create EML code-definition pairs or annotations in metadata
- a single column in a data table may have mappings to multiple dictionaries, by including multiple rows for it

Columns

|  column name 	|   type	 |   not NULL required?	|  references cols 	| description | example |
|---------------|----------|-----------------------|-------------------|--------------|---------| 
| variable_mapping_id  | character |yes| NA	| the id of the variable mapping, a row or record identifier  	| 	1  	|
| table_name           | character |yes| NA	| the name of the table holding this variable 	| 	my_observation  	|
| variable_name        | character |yes| ONE OF: observation.variable_name, observation_ancillary.variable_name, taxon_ancillary.variable_name, OR location_ancillary.variable_name | the variable name in another data table  	|  sample_z  	|
| mapped_system        | character | no| NA	| system defining this variable_name  	|  NERC  	|
| mapped_id     | character | no| NA	| id of the definition in that system 	| SDN:P07::CFSN0721 	|
| mapped_label  | character | no| NA	| label for this variable in that mapped system  	|  depth  	|



