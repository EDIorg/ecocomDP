# Instructions for creating and reusing ecocomDP datasets

### Contents:

* [Overview](#overview)
* [Create](#create)
* [Validate](#validate)
* [Document](#document)
* [Archive](#archive)
* [Discover](#discover)
* [Reuse](#reuse)

## Overview
[back to top](#contents)

Creation of an ecocomDP should be fully scripted to enable automated maintenance and upkeep as the underlying level-0 data and metadata change. Validation checks ensure the ecocomDP tables are congruent with with the ecocomDP schema and that down stream processes can occur without issue. Documenting the ecocomDP with EML metadata is a simple process combining level-0 and ecocomDP metadata elements. Archive in the EDI Data Repository is supported by multiple methods. Discovery and reuse of ecocomDP data is facilitated by the ecocomDP R package. See below for details.

## Create
[back to top](#contents)

#### Familiarize yourself with the ecocomDP
There are several details to be aware of including required and non-required tables and columns, as well as column class restrictions and date-time format expectations. 

  * [Table descriptions, relationships, and requirements](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model) 

#### Understand level-0 dataset content and how it will map to the ecocomDP
Since level-0 content and format varies widely among level-0 datasets, we can't offer a "one size fits all" solution but do have some recommendations on the order in which tables should be created and notes on handling specific types of content.

  * [Table creation recommendations](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md)

#### Write a script to convert level-0 data to the ecocomDP tables
The script should cover these processes:

  * Read in level-0 data and metadata from the EDI Data Repository
  * Reformat level-0 data into ecocomDP tables and write to files

  and only require three arguments:
    
  * path - Where the ecocomDP tables will be written
  * parent.package.id - ID of the level-0 data package
  * child.package.id - ID of the ecocomDP data package
    
#### Functions available to help create ecocomDP tables

  * `ecocomDP` R library functions:
    * `make_location` - Make the location table
    * `make_variable_mapping` - Make the variable_mapping table. [Resources for linking variables to external ontologies are available here](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/measurement_vocabularies.md)
    * `make_dataset_summary` - Make the dataset summary table
    * `make_taxon` - Make the taxon table
  * `dataCleanr` R library:
    * `iso8601_char` - Convert datetimes to the ISO 8601 format
    * `iso8601_format` - Get the format string of ISO 8601 datetimes (e.g. 'YYYY-MM-DD')
    * `iso8601_read` - Read ISO 8601 formatted datetimes into POSIXct and POSIXlt
    
[Example script for creating ecocomDP tables](https://github.com/EDIorg/ecocomDP/blob/master/documentation/examples/convert_bes543_to_ecocomDP.R)

## Validate
[back to top](#contents)

Validate ecocomDP tables with the `validate_ecocomDP` function of the ecocomDP R library. This function ensures tables pass normalization tests, and checks for required tables, fields, field classes, datetime formats, etc.

## Document
[back to top](#contents)

Create EML metadata for a set of ecocomDP tables using the `make_eml` function of the ecocomDP R library. This function collects relevant information from the level-0 EML and combines it with boiler plate metadata to create an ecocomDP EML record.

[Example script for creating ecocomDP EML](https://github.com/EDIorg/ecocomDP/blob/master/documentation/examples/package_bes543.R)

## Archive
[back to top](#contents)

Archive the ecocomDP tables, conversion script, and EML metadata in the EDI Data Repository. [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.

* [Example ecocomDP data package, including conversion script and tables](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=191)

## Discover
[back to top](#contents)

Get a list of all available ecocomDP data with the `view_all_ecocomDP` function, and view ecocomDP metadata with `view_landing_page` function. Alternatively, search on the term "ecocomDP" in the EDI Data Repository.

## Reuse
[back to top](#contents)

Aggregate L1 of interest with the `aggregate_ecocomDP` function.
