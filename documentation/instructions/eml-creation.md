# Instructions for making EML for an ecocomDP

### Overview

After the ecocomDP tables are created and validated, you will need to make an EML metadata record for them to complete the data package for upload to the EDI data repository. Because ecocomDP tables are standarized, the `make_eml` function will generate most of the EML automatically, however you will need to supply some additional information.

#### Installation

Please reinstall periodically to ensure you have the latest version. Installation requires the `devtools` package.

```
# Install devtools and load
install.packages("devtools")
library(devtools)

# Install and load ecocomDP
install_github("EDIorg/ecocomDP")
library(ecocomDP)
```

### Step 1: Create a directory

If you haven't already created a designated directory for your ecocomDP please do so now. This is where the ecocomDP tables, associated processing scripts, and metadata parts are stored.

Replace spaces with underscores (e.g. `name of your directory` should be `name_of_your_directory`).

### Step 2: Update your taxonomy data

While you are going through the effort of reformatting a dataset to ecocomDP, you may as well update the taxonomic data with authority IDs and taxonomic serial numbers (TSNs) to make the taxonomic information of these datasets relatable. [We've developed the `taxonomyCleanr` R package to help you do this](https://github.com/EDIorg/taxonomyCleanr).

Output from the `taxonomyCleanr` is a revised version of your L0 data table containing the updated taxonomy information and the taxonomicCoverage EML tree for your data. You will need to reprocess your ecocomDP tables

Once you've updated your taxonomy data you will need to reprocess your ecocomDP tables. This should be relatively straight forward if the table creation step was scripted.

### Step 3: Validate tables

Validate your ecocomDP tables relative to the model ecocomDP [following these instructions](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-validation.md).

### Step 4: Import and complete metadata templates

Import and complete the metadata templates to provide additional information about your ecocomDP. Use the function `import_templates` and consult the function documentation for more information.
```
?import_templates
```

### Step 5: Define variables

Define variables contained in your ecocomDP tables and assign units with `define_variables`. Consult function documentation for more information.
```
?define_variables
```

### Step 6: Make EML

Make EML metadata with `make_eml`. See function documentation for more information.
```
?make_eml
```

### Step 7: Upload your data package to the EDI repository

Your data and metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). [Follow these instructions](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-4-submit-your-data-package/) to upload your data package.


