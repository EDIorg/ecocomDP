# ecocomDP: A dataset design pattern for ecological community data to facilitate synthesis and reuse

We know from experience that original, primary research data sets cannot be easily combined or synthesised until all data are completely understood and converted to a similar format. There are two approaches to achieving this data regularity: a) to prescribe the format before data collection starts, or b) to convert primary data into a flexible standard format for reuse. Prescribed formats are impossible to impose on research studies, so we take the second approach: define a flexible intermediate, and convert primary data to that model. The Ecological Community Data Pattern (ecocomDP) is the result of our examination of the needs of synthesis scientists using community survey data, and determination of the features of a flexible intermediate for these data.

![](https://github.com/EDIorg/ecocomDP/blob/master/documentation/images/ecocom_dp_workflow_cut.png)

_Figure: Abstract view of dataset levels. A flexible intermediate (L1, middle) lies between datasets of primary observations (L0, left) and the aggregated views used by synthesis projects. If datasets are in a recognized format, EDI can create tools for some basic functions_

## Getting Started

### Contents

#### Documentation

[Table descriptions, relationships, and requriements.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)

[An example set of ecocomDP tables.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/examples)

[Instructions for creating an ecocomDP](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions)

#### PostgreSQL

[A PostgreSQL implementation of ecocomDP can be found here](https://github.com/EDIorg/ecocomDP/tree/master/postgreSQL).

#### R package

We're developing an R package to do the following:
1. Help format a level-0 dataset to the ecocomDP (in development).
2. Validate tables against the ecocomDP schema (in development).
3. Make metadata for the ecocomDP tables in the Ecological Metadata Language (EML; completed).
4. Aggregate all data packages in the ecocomDP format and create/export views (in development). 

The ecocomDP R package is available here on GitHub. Installation requires `devtools`.
```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load ecocomDP
install_github("EDIorg/ecocomDP")
library(ecocomDP)
```

Instructions for using the `ecocomDP` R package are incorporated in the [instructions for creating an ecocomDP](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions)

## Running the tests

Coming soon!

## Contributing

Community contributions are welcome. Please reference our [code conduct](https://github.com/EDIorg/ecocomDP/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/EDIorg/ecocomDP/blob/master/CONTRIBUTING.md) for submitting pull requrests to us.

## Versioning

We have not yet versioned this project.

## Authors

[Several people have contributed to this project](https://github.com/EDIorg/ecocomDP/blob/master/AUTHORS.md). We welcome you to join us.

## License

This project is licensed under the [CC0 1.0 Universal (CC0 1.0)](https://creativecommons.org/publicdomain/zero/1.0/legalcode) License - see the LICENSE file for details.
