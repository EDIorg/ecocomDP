# How to create tables in the design pattern ecocomDP 

Introduction
---
The ecocom design pattern (ecocomDP) is a flexible intermediate for ecological community survey data. 
For information on the process see https://environmentaldatainitiative.org/resources/tools/dataset-design/

This document contains guidelines for creating tables in this design pattern. Other resources are available
* Detailed descriptions of the tables and relationships in the model: ../model
* Detailed info about creating EML metadata for a group of tables in this design pattern: [link TBD ]
* Templates to assist in planning a converstion from your Level 0 data to this design pattern: [ link TBD ]

General guidelines
---

1. Our goal is to provide data in regular formats to simplify querying and synthesis. DO NOT calculate values or aggregate primary observations (in the observation table). Users (e.g., data synthesis working groups) will perform calcluations for their own needs. 
2. Conversion requires a basic understanding of the data. You will probably need to read available metadata.
3. The three ancillary tables (taxon_ancillary, location_ancillary, observation_ancillary) may have aggregated data. If you aggreagate in these tables, include a copy of the raw data table (L0) with the final L1 data package so that additional details are easy to find. A sitation where this might happen is when the ancillary data have more structure than the primary observation data, e.g., a plankton tow (through a water column) is accompanied by a profile with bins for each depth. The profile values could be averaged for the observation_ancillary table, but the profile structure may be of interest as well, so include it (e.g., as an 8th table). Keep in mind: in a design pattern for biogeochemistry data, the profile would be the 'primary observation', and values would not be averaged! 
4. Use the temlate to help plan the conversion, eg to "map" between L0 and L1 fields, and to keep notes for processing required as you convert.
5. there are taxonomic lookup services to help align species names, and confirm ids. 
6. To be flexible, this design pattern uses a long format, with key-value pairs (plus a "unit"), where key = the variable name. We have observed this format used in a variety of applications where data from mulitple sources are combined. There is a drawback to this format however, mainly that the definition of the key (variable_name) is generally uncontrolled and with little typing. Including a unit field helps aleviate this to some extent, and we are exploring ways to add semantic meaning to measurmenet fields, if vocabularies exist. For the present, we recommend that the EML metadata for variable fields be constructed as code-definition pairs (an EML "enumeratedList"), so that every variable_name is accompanied by a definition. If L0 data already have EML metadata, these definitions can be found at this XPath: //dataset/dataTable/attributeList/attribute/defininition


Table list 
---
Tables are listed here in suggested population order, ie, parents first. 
The location and taxon tables are most likely to come first, but could be in either order, as they are equal parents of observation.

|  order | table name 	|   required?	|   references tables    | description            | 
|--------|--------------|---------------|------------------------|------------------------|
|1.|location              |yes|NA              |basic info to identify a place    |  
|2.|taxon                 |yes|NA              |basic info to identify an organism | 
|3.|observation           |yes|location, taxon |observations about taxa that will be analyzed (organism abundance or density, or the data to compute these, count) |  
|4.|location_ancillary    |no |location        |additional info about a place that does not change, in long format. |
|5.|taxon_ancillary       |no |taxon           |additonal info about an organism that does not change, in long format |
|6.|observation_ancillary |no |observation     |additional info to contextualize the observation (not specific to the taxon or locations) in long-format  |  
|7.|dataset_summary       |yes|observation     |summary info calculated from incoming data. one line table |


_____
Tables
---
Table: location
---
Description: identifying info about a place (lon, lat, elev). 

Column notes

|  column name 	|   type   | required | references cols | note |
|---------------|----------|----------|-----------------|-------|
| location_id        |char |yes|NA|         | 
| location_name      |char |no |NA | 		|
| latitude 	         |float|no |NA |
| longitude 	     |float|no |NA |
| elevation	         |float|no |NA	|
| parent_location_id |char | no|NA| Use this column to reference another site in the same table, and enable site nesting.	|


Table: taxon
---
Description: identifying information about a taxon, eg, name, id and system. 
This is likely to be a troublesome table, if data contributors have used local names, or common names or codes.

Column notes

|  column name 	|   type   | required |  references cols 	| note |
|---------------|-----------|----------|--------------------|-------|
|taxon_id           | char |yes|NA |very likely to be the local id used in the L0 dataset |  
|taxon_rank         | char |no |NA |  |   
|taxon_name         | char |yes|NA |ideally, the L0 dataset will contain the species binomial.   | 
|authority_system   | char |no |NA | use a recognized service, eg, ITIS, WoRMS, USDA |  
|authority_taxon_id | char |no |NA | use a lookup service (eg, taxize) to find the ID assigned by a taxon system, like ITIS.|


Table: observation
---
Description: This is the core table - which holds the observations being analyzed, eg, organism abundance or density.  
or the data to compute density (count).
Observations must be linked to a taxon and a location


Column notes

|  column name 	|   type	| required |  references cols 	| note |
|---------------|-----------|----------|-------------------|-------|  
|observation_id       |char |yes       |NA    	|   	|
|package_id           |char |yes       | NA  	|   	|
|location_id          |char |yes       |location.location_id |   	|
|observation_datetime |datetime  |yes  |NA   	|ISO preferred 	    |
|taxon_id             |char |yes       |taxon.taxon_id | 	|
|variable_name        |char |yes       |NA 	|  in EML metadata, these should be code-def pairs (enumeratedList)  	|
|value                |float|yes       |NA   	|   	|
|unit                 |char |yes       |NA   	|    	|

Table: location_ancillary 
---
Description: additional info about a place that does not change frequently, 
eg, lake area or depth, experimental treatment. 
features that change frequently (e.g, air temperature at a place) should be placed in the "observation_ancillary" table 

Column notes

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|----------|-------------------|-------|  
|location_ancillary_id |char |yes|NA   |  	|
|location_id           |char |yes|location.location_id |  	|
|datetime              |datetime|no|NA |ISO preferred. column included because some info about a location (e.g., experimental treatment or natural disturbance) is not recorded at the same datetime as the observaiton  	| 
|variable_name         |char |yes|NA   |in EML metadata, these should be code-def pairs (enumeratedList)  	|
|value                 |char |yes|NA   |    	|
|unit                  |char |no|NA    |    	|



Table: taxon_ancillary
---
Description: additional info about an organism that does not change frequently, 
eg, trophic level  
features that change frequently are probably primary observations

Column notes

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|----------|--------------------|-------|  
|record_id          |char |yes|NA | most likley auto-generated |  
|taxon_ancillary_id |char |yes|NA   	              |   	|   	|
|taxon_id           |char |yes| (table = taxon) taxon_id   	|   	|
|datetime       |datetime |no|   	              |ISO preferred. column included because some info about a taxon (e.g., grwoth stage, trophic level) is not recorded at the same datetime as the observaiton    	|
|variable_name      |char |yes|   	              |in EML metadata, these should be code-def pairs (enumeratedList) 	|
|value              |char |yes|   	              |  	|
|unit               |char |no|   	              | 	|



Table: observation_ancillary 
---
Description: ancillary information about an observational event for context, but that are not about the organism or sampling location. Examples: water depth, height of a tower, temperature of medium. 

There is no datatime field in the observation_ancillary table. Since it is context for the observation, it will inherit observation datetimes.

Column notes

|  column name  |   type    | required |  references cols   | note |
|---------------|-----------|----------|--------------------|-------|  
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
|---------------|-----------|----------|--------------------|-------|  
|package_id                  |char |yes| (table = observation) package_id    |  id of the L1 pkg (this package)      |
|original_package_id         |char |no|    |  id of the L0 pkg (original, source) |
|length_of_survey_years      |integer   |yes|       |    | 
|number_of_years_sampled     |integer   |yes|       |  |
|std_dev_interval_betw_years |float     |yes|       |   |
|max_num_taxa                |integer    |yes|       |      |
|geo_extent_bounding_box_m2  |float      |no|    |     |
