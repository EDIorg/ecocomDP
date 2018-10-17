# Instructions for creating, archiving, and using the ecocomDP

### Overview

The raw version of your dataset is what we refer to as Level-0, or L0. The ecocomDP version of your data is Level-1, or L1. Because the format and content of L0 datasets vary widely, reformatting from L0 to L1 requires a creation of a custom conversion script. Once this script exists, future revisions of the L1 become simple. After the L1 tables have been created a set of validation checks test that the data are indeed compliant with the ecocomDP schema. This step ensures the L1 can be seemlessly aggregated with other L1. The process of creating EML metadata for the L1 is simplified by the fact that the data are in a standardized format, and boiler plate metadata can be combined with L0 metadata to create a richly documented data package. Once documented, the data should be archived in the EDI Data Repository to be discovered and aggregated with other L1 in the archive. The following set of topics will help you create, validate, document, archive, and aggregate the L1 ecocomDP.

### Contents:

* [Archive the L0 dataset](#archive-the-l0-dataset)
* [Update taxonomy data](#update-taxonomy-data)
* [Create L1 tables](#create-l1-tables)
* [Validate L1 tables](#validate-l1-tables)
* [Define categorical variables of L1 tables](#define-categorical-variables-of-l1-tables)
* [Document](#document)
* [Archive the L1 dataset](#archive-the-l1-dataset)
* [Find L1 data](#find-l1-data)
* [Aggregate L1 data](#aggregate-l1-data)

## Archive the L0 dataset

It is a good practice to archive the L0 version of your dataset “as is” before converting it to L1. The L0 may contain information that will be lost from the L1. Create EML metadata for your L0 and upload your data and metadata to the Environmental Data Initiative repository using [the `EMLassemblyline`](https://github.com/EDIorg/EMLassemblyline).

## Update taxonomy data

While you are going through the effort of reformatting a dataset to L1, you may as well update the taxonomic data with authority IDs and taxonomic serial numbers (TSNs) to make the taxonomic information of these datasets relatable. [We've developed the `taxonomyCleanr` R package to help you do this](https://github.com/EDIorg/taxonomyCleanr).

## Create L1 tables

The process of formatting a L0 dataset to L1 is variable and depends on several factors making a scripted solution difficult. However, we have identified some general steps that are helpful to follow in this process and have created some resources to help with this process:
* [General instructions with notes.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md).
* [Detailed descriptions of the L1 ecocomDP tables and their relationships.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)
* [Measurement vocabularies to assist in creation of the variable_mapping table](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/measurement_vocabularies.md)
* [Templates to assist in planning a converstion from your L0 data to the L1 ecocomDP.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/template-mapping.md)

## Validate L1 tables

Once you've created a L0 for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, etc. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Run the `validate_ecocomDP` function. Consult function documentation for use.

Resolve each error encountered in the validation process. If no errors exist, and your L1 is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

## Define categorical variables of L1 tables

After the L1 tables have been validated, you will need to define their categorical variables (e.g. the 'variable_name' field of the 'observation' table is composed of categorical variables that others won't know the meaning of without a supplied definition). Run the `define_variables` function (enter `?define_variables` in the RStudio Console window for instructions) and the code will automatically identify the unique categorical variables in your tables and prompt you to provide 'definitions' and 'units'. If you have no units to report then leave the units field blank.

## Document

After the L1 tables have been validated and categorical variables defined, you can make an EML metadata record for them to complete the data package for upload to the EDI data repository. Before running the `make_eml` function run `import_templates` (enter `?import_templates` in the RStudio console to see required arguments.  Because L1 tables are standardized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

## Archive the L1 dataset

Your L1 ecocomDP and associated metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.

## Find L1 data

Get a list of all available L1 data with the function `view_all_ecocomDP`. View L1 metadata with `view_landing_page`.

## Aggregate L1 data

Aggregate L1 of interest with the function `aggregate_ecocomDP`.
