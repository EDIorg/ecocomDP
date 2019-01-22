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

Familiarize yourself with the ecocomDP. There are several details to be aware of including required and non-required tables and columns, as well as column class restrictions and date-time format expectations. 

* [Table descriptions, relationships, and requirements](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model) 

Understand level-0 dataset content and how it will map to the ecocomDP. Since level-0 content and format varies widely among level-0 datasets, we can't offer a "one size fits all" solution but do have some recommendations on the order in which tables should be created and notes on handling specific types of content.

* [Table creation recommendations](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md)

Write a script to convert level-0 data to the ecocomDP tables. The script should cover these processes:

* Read in level-0 data and metadata from the EDI Data Repository
* Reformat level-0 data into ecocomDP tables and write to files

  and only require three arguments:
    
* path - Where the ecocomDP tables will be written
* parent.package.id - ID of the level-0 data package
* child.package.id - ID of the ecocomDP data package
    
Functions are available in the ecocomDP R library to help create the tables:

* `make_location` - Make the location table
* `make_variable_mapping` - Make the variable_mapping table. [Resources for linking variables to external ontologies are available here](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/measurement_vocabularies.md)
* `make_dataset_summary` - Make the dataset summary table
    
[An example script for creating ecocomDP tables is available here.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/examples/convert_bnz501_to_edi275.R)

## Validate
[back to top](#contents)

Once you've created a L0 for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, etc. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Run the `validate_ecocomDP` function. Consult function documentation for use.

Resolve each error encountered in the validation process. If no errors exist, and your L1 is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

* `validate_ecocomDP` - Validate a set of ecocomDP tables
* `define_variables` - Get definitions and units for variables listed in ecocomDP tables
    * `make_eml` - Make EML for a set of ecocomDP tables

## Document
[back to top](#contents)

After the L1 tables have been validated and categorical variables defined, you can make an EML metadata record for them to complete the data package for upload to the EDI data repository. Before running the `make_eml` function run `import_templates` (enter `?import_templates` in the RStudio console to see required arguments.  Because L1 tables are standardized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

## Archive
[back to top](#contents)

Your L1 ecocomDP and associated metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.

[An example ecocomDP data package including the conversion script and tables is available here.](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=275)

## Discover
[back to top](#contents)

Get a list of all available L1 data with the function `view_all_ecocomDP`. View L1 metadata with `view_landing_page`.

## Reuse
[back to top](#contents)

Aggregate L1 of interest with the function `aggregate_ecocomDP`.
