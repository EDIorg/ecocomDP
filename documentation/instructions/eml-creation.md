# EML creation

Introduction
---
After the ecocomDP tables are created and validated, you will need to make an EML metadata record for them to complete the data package for upload to the EDI data repository. Because ecocomDP tables are standarized, the `make_eml` function will generate most of the EML automatically, however you will need to provide some additional information.

Making EML
---

1. Install the ecocomDP package from GitHub.
```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load the ecocomDP
install_github("EDIorg/ecocomDP")
library(ecocomDP)
```

2. Validate your ecocomDP tables relative to the model ecocomDP [following these instructions](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/table-validation.md).

3. Import and fill out template files to provide additional metadata for your ecocomDP EML. Use the function `import_templates` and consult the function documentation (`?import_templates`) for more information.

4. Define ecocomDP variables and assign units with `define_variables`. Consult function documentation for more information (`?define_variables`).

5. Create ecocomDP EML with `make_eml`. See function documentation for more information (`?make_eml`).
