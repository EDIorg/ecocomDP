# EML creation

Introduction
---
After the ecocomDP tables are created and validated, you will need to create an EML metadata record for them, and create a "data package" for a repository. Because the tables are standarized, a few R functions will generate table-level metadata relatively quickly, although you may need to add additional high-level metadata. As this design pattern has been undergoing review, these R functions may not be up to date. Please check with info@environmentaldatarepository.org for the latest information.

Using the R package
---
More detailed step by step instructions for creating EML for an ecocomDP will be added here soon. In the meantime, a very coarse explanation is ...


1. Install the ecocomDP package from GitHub.
```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load the ecocomDP
install_github("EDIorg/ecocomDP")
library(devtools)
```

2. Import and complete template files to provide additional metadata to the ecocomDP EML. Use the function `import_templates` and consult the function documentation for more information.

3. Define ecocomDP variables and assign units with `define_variables`. Consult function documentation for more information.

4. Validate your ecocomDP tables relative to the model ecocomDP [following these instructions](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-validation.md).

5. Create ecocomDP EML with `make_eml`. See function documentation for more information.
