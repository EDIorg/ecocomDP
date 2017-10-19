# Design Pattern - ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is planned to be a flexible intermediate for ecological community survey data. For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

For detailed descriptions of tables and relationships in the model, see ../model

table list and suggested population order, ie, parents first.

|  order | table name 	|   required?	|   references tables    | description            | 
|--------|--------------|---------------|------------------------|------------------------|
|1.|location              |yes|NA              |basic info to identify a place    |  
|2.|taxon                 |yes|NA              |basic info to identify an organism | 
|3.|observation           |yes|location, taxon |observations about taxa, that will be analyzed. Eg, organism abundance or density, or the data to compute density (count) |  
|4.|location_ancillary    |no |location        |additional info about a place that does not change, in long format. |
|5.|taxon_ancillary       |no |taxon           |additonal info about an organism that does not change, in long format |
|6.|observation_ancillary |no |observation     |additional info about the sampling event (not related to taxa or locations) in long-format  |  
|7.|dataset_summary       |yes|observation     |summary info calculated from incoming data. one line table |


_____
Tables
---
Table: location
---
Description: identifying info about a place (lon, lat, elev). 

Columns

|  column name 	|   type   | required | references cols | note |
|---------------|----------|----------|-----------------|-------|
| location_id        |char |yes|NA|         | 
| location_name      |char |no |NA | 		|
| latitude 	         |float|no |NA |
| longitude 	     |float|no |NA |
| elevation	         |float|no |NA	|
| parent_location_id |char | no|NA| Use this column for self-referencing so that sites can be nested	|


Table: taxon
---
Description: identifying information about a taxon, eg, name, id and system. 

Columns

|  column name 	|   type   | required |  references cols 	| note |
|---------------|----------|----------|-------------------|-----|
|taxon_id           | char |yes| NA | |  
|taxon_rank         | char |no| NA |  |   
|taxon_name         | char |yes| NA |  | 
|authority_system   | char |no| NA | use a recognized service, eg, ITIS, WoRMS, USDA |  
|authority_taxon_id | char |no| NA | use a lookup service (eg, taxize) to find the ID assigned by a taxon system, like ITIS.|


Table: observation
---
Description: This is the core table - which holds the observations being analyzed, eg, organism abundance or density. 
observations must be linked to a taxon and a location

Columns

|  column name 	|   type	| required |  references cols 	| note |
|---------------|-----------|----------|-------------------|-------|  
|record_id            |char |yes       |NA	|  	|
|observation_id       |char |yes       |NA    	|   	|
|package_id           |char |yes       | NA  	|   	|
|location_id          |char |yes       |location.location_id |   	|
|observation_datetime |datetime  |yes  |NA   	|  	|
|taxon_id             |char |yes       |taxon.taxon_id  	| 	|
|variable_name        |char |yes|      |NA 	|    	|
|value                |float     |yes  |NA   	|   	|
|unit                 |char |yes       |NA   	| 	|

Table: location_ancillary 
---
Description: additional info about a place that does not change frequently, 
eg, lake area or depth, experimental treatment. 
features that change frequently (e.g, air temperature at a place) should be placed in the "observation_ancillary" table 

Columns

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|----------|-------------------|-------|  
|location_ancillary_id |char |yes|NA   |  	|
|location_id           |char |yes|location.location_id |  	|
|datetime              |datetime|no|NA |  	| 
|variable_name         |char |yes|NA   |    	|
|value                 |char |yes|NA   |   	|
|unit                  |char |no|NA    |    	|



Table: taxon_ancillary
---
Description: additional info about an organism that does not change frequently, 
eg, trophic level  
features that change frequently are probably primary observations

Columns

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|-----------------------|-------------------|-------|  
|record_id          |char |yes|NA | most likley auto-generated |  
|taxon_ancillary_id |char |yes|NA   	              |   	|   	|
|taxon_id           |char |yes| (table = taxon) taxon_id   	|   	|
|datetime       |datetime |no|   	              |  ISO datetime    	|
|variable_name      |char |yes|   	              |   in EML metadata, these should be code-def pairs (enumeratedList) 	|
|value              |char |yes|   	              |  	|
|unit               |char |no|   	              | 	|



Table: observation_ancillary 
---
Description: ancillary information about an observational event for context, but that are not about the organism or sampling location. Examples: water depth, height of a tower, temperature of medium. Since this table can hold a variety of measurements, there are 2 additional columns to name the entity and characteristic being measured, both of which can be controlled in external vocabularies or ontologies.  

Columns

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|-----------------------|-------------------|-------|  
|observation_ancillary_id | char |yes| NA	|  	|
|observation_id           | char |yes| NA	| observation.observation_id                         	| 	  	|
|variable_name            | char |yes| NA | in EML metadata, these should be code-def pairs (enumeratedList)  		|
|value                    | char | no| NA	|   	|
|unit                     | char | no| NA	|   	|


Table: dataset_summary
---
Description: summary info about the dataset. some could be elevated to metadata to enhance discovery.

Columns

|  column name  |   type           | required |  references cols   | note |
|---------------|------------------|-----------------------|-------------------|-------|  
|package_id                  |char |yes| (table = observation) package_id    |  id of the L1 pkg (this package)      |
|original_package_id         |char |no|    |  id of the L0 pkg (original, source) |
|length_of_survey_years      |integer   |yes|       |    | 
|number_of_years_sampled     |integer   |yes|       |  |
|std_dev_interval_betw_years |float     |yes|       |   |
|max_num_taxa                |integer    |yes|       |      |
|geo_extent_bounding_box_m2  |float      |no|    |     |
