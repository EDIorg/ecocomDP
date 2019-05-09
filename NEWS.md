# ecocomDP 0.0.0.9000

#### 2019-02-02
* __Enhancement:__ Release `L0_missing_value_codes_to_na.R`, a function to convert missing value codes to NA. Converting missing value codes to a single type simplifies downstream usage.
* __Enhancement:__ Release `L0_codes_to_definitions.R`, a function to convert categorical codes to definitions. Occasionally L1 content is locked up in L0 code definitions (e.g. species names). This function converts these codes to the associated definitions.
* __Enhancement:__ Release `L0_trimws.R`, a function to remove white space from L0 character class columns. The presence of white space affects alignment with L0 EML and downstream usage.
* __Enhancement:__ Add change log.