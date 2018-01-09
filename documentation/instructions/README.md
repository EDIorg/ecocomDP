# Instructions for creating an ecocomDP

### Overview

The raw version of your dataset is what we refer to as Level-0, or L0. The ecocomDP version of your data is Level-1, or L1. Because the format and content of L0 datasets vary widely, reformatting from L0 to L1 can not be automated and often requires a custom process. But once the L1 tables have been created, metadata generation and L1 aggregation is relatively simple and largely automated. The following steps will help you create a L1 ecocomDP for your L0 dataset.

### Step 1: Archive the L0 version of your dataset

It is a good practice to archive the L0 version of your dataset before converting it to L1. The L0 may contain information that will be lost in the L1 form, and is therefore a good practice to archive "as is". Create EML metadata for your L0 and upload your dataset and metadata to the Environmental Data Initiative repository using [the `EMLassemblyline`](https://github.com/EDIorg/EMLassemblyline).

### Step 2: Create a working directory for your L1

Create a working directory for your ecocomDP. This directory will contain all the files made in the ecocomDP creation process and will be available for editing should you need to create a revision.

Replace spaces with underscores (e.g. `name of your directory` should be `name_of_your_directory`).

### Step 3: Update your taxonomy data

While you are going through the effort of reformatting a dataset to ecocomDP, you may as well update the taxonomic data with authority IDs and taxonomic serial numbers (TSNs) to make the taxonomic information of these datasets relatable. [We've developed the `taxonomyCleanr` R package to help you do this](https://github.com/EDIorg/taxonomyCleanr).

### Step 4: Make the ecocomDP tables

The process of formatting a L0 dataset to the L1 ecocomDP is variable and depends on several factors making a scripted solution difficult. However, we have identified some general steps that are helpful to follow in this process and have created some resources to help with this process:
* [General instructions with notes.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-creation.md).
* [Detailed descriptions of the ecocomDP tables and their relationships.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)
* [Templates to assist in planning a converstion from your Level 0 data to this design pattern.](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/template-mapping.md)

### Step 5: Validate your ecocomDP tables

Once you've created an ecocomDP for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, and other odds and ends. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Run the `validate_ecocomDP` function. Consult function documentation for use.

Resolve each error encountered in the validation process. If no errors exist, and your ecocomDP is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

### Step 6: Make EML metadata for your ecocomDP

After the ecocomDP tables have been validated, you can make an EML metadata record for them to complete the data package for upload to the EDI data repository. Because ecocomDP tables are standarized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

### Step 7: Upload your data package to the EDI repository

Your ecocomDP and associated metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.
