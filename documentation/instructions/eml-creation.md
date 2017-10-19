# EML creation

Introduction
---
Once a dataset has been converted to the ecocomDP and subsequently validated you will need to create EML for it. Thankfully, the now standardized dataset combined with a few R functions makes creating EML relatively painless and quick. The current set of functions in the ecocomDP package accomodate the previous version of ecocomDP. This will be updated soon. Once they have been updated you will recieve a briefing on use of the package below.

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

4. Create ecocomDP EML with `make_eml`. See function documentation for more information.
