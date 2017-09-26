# ecocomDP: A dataset design pattern for ecological community data to facilitate synthesis and reuse

We know from experience that original, primary research data sets cannot be easily combined or synthesised until all data are completely understood and converted to a similar format. There are two approaches to achieving this data regularity: a) to prescribe the format before data collection starts, or b) to convert primary data into a flexible standard format for reuse. Prescribed formats are impossible to impose on research studies, so we take the second approach: define a flexible intermediate, and convert primary data to that model. The Ecological Community Data Pattern (ecocomDP) is the result of our examination of the needs of synthesis scientists using community survey data, and determination of the features of a flexible intermediate for these data.

![](https://github.com/EDIorg/ecocomDP/blob/master/documentation/images/ecocom_dp_workflow_cut.png)

_Figure: Abstract view of dataset levels. A flexible intermediate (L1, middle) lies between datasets of primary observations (L0, left) and the aggregated views used by synthesis projects. If datasets are in a recognized format, EDI can create tools for some basic functions_

## Getting Started

### Contents

#### Model documentation

View table descriptions, relationships, and requriements [here](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model).

An example set of tables can be accessed [here](https://github.com/EDIorg/ecocomDP/tree/master/documentation/examples).

#### Formatting guide

The process of formatting a level-0 dataset to level-1 (ecocomDP) is variable and depends on several factors making a scripted solution difficult. However, we have identified some general steps that are helpful to follow in this process and have written up a set of instructions [here](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions).

#### PostgreSQL

A PostgreSQL implementation of ecocomDP can be found [here](https://github.com/EDIorg/ecocomDP/tree/master/postgreSQL).

#### R package

We are developing an R package of tools to do the following: (1) help format a level-0 dataset to ecocomDP, (2) validate tables against the ecocomDP schema, (3) make metadata in the Ecological Metadata Language (EML) for ecocomDP tables, (4) aggregate several ecocomDP data packages and create/export views. These functions are in development and are not yet stable. Follow development in [projects](https://github.com/EDIorg/ecocomDP/projects).

### Installing

The ecocomDP R package is available here on GitHub. To install you will first need to install and load `devtools`.

```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load the ecocomDP
install_github("EDIorg/ecocomDP)
library(devtools)
```

## Running the tests

We have no tests yet.

## Contributing

This project is in the initial phase of development. Community contributions are welcome, but might be more effective once we solidify the ecocomDP design pattern a little more. Please reference our [code conduct](https://github.com/EDIorg/ecocomDP/blob/master/CODE_OF_CONDUCT.md) and contributing guidelines for submitting pull requrests to us.

## Versioning

We do not yet have any versions available. Stay tuned!

## Authors

See also the list of contributors who participated in this project.

## License

This project is licensed under the [CC0 1.0 Universal (CC0 1.0)](https://creativecommons.org/publicdomain/zero/1.0/legalcode) License - see the LICENSE file for details.
