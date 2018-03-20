# Instructions for creating an ecocomDP

### Overview

The raw version of your dataset is what we refer to as Level-0, or L0. The ecocomDP version of your data is Level-1, or L1. Because the format and content of L0 datasets vary widely, reformatting from L0 to L1 cannot be automated and often requires a custom process. Once the L1 tables have been created, metadata generation and L1 aggregation is relatively simple and mostly automated. The following steps will help you create a L1 ecocomDP for your L0 dataset.

### Step 1: Archive the L0 version of your dataset

It is a good practice to archive the L0 version of your dataset “as is” before converting it to L1. The L0 may contain information that will be lost from the L1. Create EML metadata for your L0 and upload your data and metadata to the Environmental Data Initiative repository using [the `EMLassemblyline`](https://github.com/EDIorg/EMLassemblyline).

### Step 2: Create a working directory for your L1

Create a working directory for your L1. This directory will contain all the files made in the L1 creation process and will be available for editing should you need to create a future revision.

Replace spaces with underscores (e.g. `name of your directory` should be `name_of_your_directory`).

### Step 3: Update your taxonomy data

While you are going through the effort of reformatting a dataset to L1, you may as well update the taxonomic data with authority IDs and taxonomic serial numbers (TSNs) to make the taxonomic information of these datasets relatable. [We've developed the `taxonomyCleanr` R package to help you do this](https://github.com/EDIorg/taxonomyCleanr).

### Step 4: Make the L1 tables

The process of formatting a L0 dataset to L1 is variable and depends on several factors making a scripted solution difficult. However, we have identified some general steps that are helpful to follow in this process and have created some resources to help with this process:
* [General instructions with notes.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md).
* [Detailed descriptions of the L1 ecocomDP tables and their relationships.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)
* [Templates to assist in planning a converstion from your L0 data to the L1 ecocomDP.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/template-mapping.md)

### Step 5: Validate your L1 tables

Once you've created a L0 for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, etc. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Run the `validate_ecocomDP` function. Consult function documentation for use.

Resolve each error encountered in the validation process. If no errors exist, and your L1 is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

### Step 6: Define categorical variables of your L1 tables

After the L1 tables have been validated, you will need to define their categorical variables (e.g. the 'variable_name' field of the 'observation' table is composed of categorical variables that others won't know the meaning of without a supplied definition). Run the `define_variables` function (enter `?define_variables` in the RStudio Console window for instructions) and the code will automatically identify the unique categorical variables in your tables and prompt you to provide 'definitions' and 'units'. If you have no units to report then leave the units field blank.

### Step 7: Make EML metadata for your L1 tables

After the L1 tables have been validated and categorical variables defined, you can make an EML metadata record for them to complete the data package for upload to the EDI data repository. Because L1 tables are standardized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

### Step 8: Upload your data package to the EDI repository

Your L1 ecocomDP and associated metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.
