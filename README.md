# ecocomDP: A dataset design pattern for ecological community data to facilitate synthesis and reuse

With the almost forty year existence of the Long-term Ecological Research (LTER) Network and other networks worldwide, a diverse array of long-term observational and experimental data is becoming increasingly available. A number of data repositories are making the data accessible and the accompanying detailed metadata allow meaningful reuse, repurpose and integration with other data. However, in synthesis research the largest time investment is still in discovering, cleaning and combining primary datasets until all data are completely understood and converted to a similar format. There are two approaches to achieving this data regularity: a) to prescribe the format before data collection starts, or b) to convert primary data into a flexible standard format for reuse. Prescribed formats have rarely been successful due to a wide range of ecosystems represented, original research questions driving collection, and varying sampling and analysis methods. Hence, we took the second approach: define a flexible intermediate data model, and convert primary data. In the context of the Environmental Data Initiative’s data repository, this allows us to maintain the original dataset, which is most convenient for answering the original research questions (and does not interfere with depositors' existing practices), add a conversion script in R to reformat the data into the intermediary format and make this available to synthesis research as Level 1 datasets. This pre-harmonization step may be accomplished by data managers because the dataset still contains all original information without any aggregation or science question specific decisions for data omission or cleaning. Although the data are still distributed into distinct datasets, they can easily be discovered and converted into other formats.

![](https://github.com/EDIorg/ecocomDP/blob/master/documentation/images/ecocom_dp_workflow_cut.png)

_Figure: Abstract view of dataset levels. A flexible intermediate (L1, middle) lies between datasets of primary observations (L0, left) and the aggregated views used by synthesis projects. If datasets are in a recognized format, EDI can create tools for some basic functions_

## Getting Started

### Contents

#### Documentation

[Table descriptions, relationships, and requriements.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)

[An example set of ecocomDP tables.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/examples)

[Instructions for creating an ecocomDP](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions)

[List of datasets to be converted to ecocomDP.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/processing_queue). This list includeds datasets that have been converted, but is not exhaustive. 

[Datasets converted to ecocomDP, available from EDI](https://portal.edirepository.org:443/nis/simpleSearch?defType=edismax&q=ecocomDP&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false) To find all ecocomDP datasets, enter "ecocomDP' in the 'simple search' box in the EDI data repository.


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
