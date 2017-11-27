# Table validation

Introduction
---
Once you've created an ecocomDP for your dataset, and before making EML, you will need to ensure the relational tables pass normalization tests as well as checks for required tables, fields, field classes, datetime formats, and other odds and ends. Validation tests are run by the `validate_ecocomDP` function of the ecocomDP R package.

Running the validation checks
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

2. Run the `validate_ecocomDP` function. Consult function documentation for use.

3. Resolve each error encountered in the validation process. If no errors exist, and your ecocomDP is valid, you will receive the message: *"Congratulations! Your ecocomDP has passed validation!"*

4. Now you can make EML for your ecocomDP ([instructions are here](https://github.com/EDIorg/ecocomDP/blob/master/documentation/instructions/eml-creation.md)).
