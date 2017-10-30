# Table validation

Introduction
---
Once you've created an ecocomDP for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, and other odds and ends. Validation tests are provided as R functions in the ecocomDP package. Current validation checks are against the previous version of ecocomDP and will be updated to the most current version soon. Furthermore, the full suite of validation checks have yet to be completed. Check back soon!

Using the R package
---
A terse explanation of the validation process is ...

1. Install the ecocomDP package from GitHub.
```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load the ecocomDP
install_github("EDIorg/ecocomDP")
library(ecocomDP)
```

2. Run the `validate_ecocomDP` function. Consult function documentation for use.

3. Resolve each error encountered in the validation process. If no errors exist, and your ecocomDP is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*
