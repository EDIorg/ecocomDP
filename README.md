# ecocomDP: A dataset design pattern for ecological community data to facilitate synthesis and reuse

With the almost forty year existence of the Long-term Ecological Research (LTER) Network and other networks worldwide, a diverse array of long-term observational and experimental data is becoming increasingly available. A number of data repositories are making the data accessible, with the intent that accompanying detailed metadata allow meaningful reuse, repurpose and integration with other data. 

However, in synthesis research the largest time investment is still in discovering, cleaning and combining primary datasets until all data are completely understood and converted to a similar format. There are two approaches to achieving this data regularity: a) to prescribe the format before data collection starts, or b) to convert primary data into a flexible intermediate format for reuse. Prescribed formats have rarely been successful due to a wide range of ecosystems represented, original research questions that drive collection, and varying sampling and analysis methods. Hence, we took the second approach: define a flexible intermediate data model, and convert primary data to it. In the context of the Environmental Data Initiative’s data repository, this allows us to maintain the original dataset, which is most convenient for descibing and answering the original research questions (and does not interfere with depositors' existing practices), add a conversion script to reformat the data into the intermediate format and make the harmonized intermediate available to synthesis research. This pre-harmonization step may be accomplished by data managers because the dataset still contains all original information without aggregation, filtering or cleaning necessary for targeted research questions. Although the data are still distributed into distinct datasets, they can easily be discovered, aggregated, and converted further into other formats, for specific secondary use.

![](https://github.com/EDIorg/ecocomDP/blob/master/documentation/images/ecocom_dp_workflow_cut.png)

_Figure: Abstract view of dataset levels. A flexible intermediate (L1, middle) lies between datasets of primary observations (L0, left) and the aggregated views used by synthesis projects. If datasets are in a recognized format, EDI can create tools for some basic functions_

## Contents

### Documentation

* [Table descriptions, relationships, and requriements.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)
* [Instructions for creating, validating, and archiving ecocomDP tables](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions)
* [List of datasets to be converted to ecocomDP](https://github.com/EDIorg/ecocomDP/tree/master/documentation/processing_queue)

### Data

* Datasets converted to ecocomDP:
    * [Environmental Data Initiative (EDI)](https://portal.edirepository.org:443/nis/simpleSearch?defType=edismax&q=ecocomDP&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false) To find all ecocomDP datasets, enter "ecocomDP' in the 'simple search' box in the EDI data repository.
    * [National Ecological Observatory Network (NEON)](https://github.com/EDIorg/ecocomDP/tree/master/documentation/examples/NEON) Run the function `view_all_ecocomDP` to see which NEON data products are available. The function `view_all_ecocomDP` is available in the ecocomDP R package (more info below).
    * [Popler](https://github.com/AldoCompagnoni/popler) Approximately 50 datasets from the Popler database are available in the ecocomDP format. Use the function `format_popler_to_ecocomDP` to get Popler data into the ecocomDP format. The function `format_popler_to_ecocomDP` is available for [download through the LTER Meta-communities working group](https://github.com/sokole/ltermetacommunities/blob/master/EDI/format_popler_to_ecocomDP.R).

### PostgreSQL

[A PostgreSQL implementation of ecocomDP can be found here](https://github.com/EDIorg/ecocomDP/tree/master/postgreSQL).

### R package

The R package helps create, validate, document, archive, find, and use data in the ecocomDP. The ecocomDP R package is available here on GitHub.
```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load ecocomDP
install_github("EDIorg/ecocomDP")
library(ecocomDP)
```

## Running the tests

Tests are implemented with the `testthat` R-package, and are organized under the /tests/testthat.

## Contributing

Community contributions are welcome. Please reference our [code conduct](https://github.com/EDIorg/ecocomDP/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/EDIorg/ecocomDP/blob/master/CONTRIBUTING.md) for submitting pull requrests to us.

## Versioning

This project follows the [semantic versioning specification](https://semver.org).

## Authors

[Several people have contributed to this project](https://github.com/EDIorg/ecocomDP/blob/master/AUTHORS.md). We welcome you to join us.
