# Instructions for creating, archiving, and using the ecocomDP

### Overview

The raw version of your dataset is what we refer to as Level-0, or L0. The ecocomDP version of your data is Level-1, or L1. Because the format and content of L0 datasets vary widely, reformatting from L0 to L1 requires a creation of a custom conversion script. Once this script exists, future revisions of the L1 become simple. After the L1 tables have been created a set of validation checks test that the data are indeed compliant with the ecocomDP schema. This step ensures the L1 can be seemlessly aggregated with other L1. The process of creating EML metadata for the L1 is simplified by the fact that the data are in a standardized format, and boiler plate metadata can be combined with L0 metadata to create a richly documented data package. Once documented, the data should be archived in the EDI Data Repository to be discovered and aggregated with other L1 in the archive. The following set of topics will help you create, validate, document, archive, and aggregate the L1 ecocomDP.

### Contents:

* [Create](#create)
* [Validate](#validate)
* [Document](#document)
* [Archive](#archive)
* [Discover](#discover)
* [Reuse](#reuse)

## Create
[back to top](#contents)

The process of converting L0 to L1 should be fully scripted to enable future updates to the L1 when the L0 is updated. The preferred scripting language is R, but other languages are acceptable. This conversion script will be archived with the L1 tables and called upon to revise L1 data when L0 updates occur. The automated maintenance of L1 datasets is managed by EDI.

__Resources to help create L1 tables:__
* [Detailed descriptions of the L1 ecocomDP tables and their relationships.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model) - Familiarize yourself with the L1 data pattern before attempting conversion. There are several details that you should be aware of, including required and non-required tables and columns, as well as column class restrictions and date-time format expectations.
* [Example L1 data package](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=192&revision=3) - This is a good example of an L1 data package. The metadata is composed of L0 content, and new L1 content. This example contains a set of L1 tables and the conversion script that created these tables.
* [Example conversion script](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=192&revision=3) - Within the above listed data package is the conversion script that created the tables (convert_cap627_to_ecocomDP.R). This script reads in the L0 data and metadata from the EDI Data Repository, applies a series of transformations, outputs the L1 tables, validates the L1 tables, defines L1 variables, and creates EML for the L1 tables. The function only requires 3 arguments: (1) The path to where the L1 tables will be exported, (2) The L0 package ID, (3) The L1 package ID. __Your conversion script should only require these arguments as well.__
* [Guide to creating tables](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md) - Includes general recommendations for the order in which tables should be created, notes on table content, and functions from the `ecocomDP` R package that help create tables.
* [Measurement vocabularies to assist in creation of the variable_mapping table](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/measurement_vocabularies.md)

## Validate
[back to top](#contents)

Once you've created a L0 for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, etc. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Run the `validate_ecocomDP` function. Consult function documentation for use.

Resolve each error encountered in the validation process. If no errors exist, and your L1 is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

## Document
[back to top](#contents)

After the L1 tables have been validated and categorical variables defined, you can make an EML metadata record for them to complete the data package for upload to the EDI data repository. Before running the `make_eml` function run `import_templates` (enter `?import_templates` in the RStudio console to see required arguments.  Because L1 tables are standardized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

## Archive
[back to top](#contents)

Your L1 ecocomDP and associated metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.

## Discover
[back to top](#contents)

Get a list of all available L1 data with the function `view_all_ecocomDP`. View L1 metadata with `view_landing_page`.

## Reuse
[back to top](#contents)

Aggregate L1 of interest with the function `aggregate_ecocomDP`.
