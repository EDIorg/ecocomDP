# ecocomDP
### A dataset design pattern for ecological community data to facilitate synthesis and reuse

We know from experience that original, primary research data sets cannot be easily combined or synthesised until all data are completely understood and converted to a similar format. There are two approaches to achieving this data regularity: a) to prescribe the format before data collection starts, or b) to convert primary data into a flexible standard format for reuse. Prescribed formats are impossible to impose on research studies, so we take the second approach: define a flexible intermediate, and convert primary data to that model. The Ecological Community Data Pattern (ecocomDP) is the result of our examination of the needs of synthesis scientists using community survey data, and determination of the features of a flexible intermediate for these data.



_Figure: Abstract view of dataset levels. A flexible intermediate (L1, middle) lies between datasets of primary observations (L0, left) and the aggregated views used by synthesis projects. If datasets are in a recognized format, EDI can create tools for some basic functions_

## Getting Started

### Contents

We are developing an R package of tools to help:

1. Create the ecocomDP tables.
2. Validate tables against the ecocomDP schema.
3. Make metadata, in the Ecological Metadata Language (EML), for a set of ecocomDP tables.
4. Aggregate ecocomDP data packages and create/export views.
    
These functions are in development and are not stable. Follow development in [projects](https://github.com/EDIorg/ecocomDP/projects).

### Installing

The ecocomDP R code package is available here on GitHub. To install you will need to install and load `devtools`.

```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load the ecocomDP
install_github("EDIorg/ecocomDP)
library(devtools)
```

## Running the tests

We have not yet drafted tests for this project.

## Contributing

We are in the beginning phase of this project. Once it moves a little further along we will share details on our code of conduct, and the process for submitting pull requrests to us.

## Versioning

We do not yet have any versions available. Stay tuned!

## Authors

See also the list of contributors who participated in this project.

## License

This project is licensed under the [CC0 1.0 Universal (CC0 1.0)](https://creativecommons.org/publicdomain/zero/1.0/legalcode) License - see the LICENSE file for details.
