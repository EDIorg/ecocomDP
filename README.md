
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecocomDP

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.com/EDIorg/ecocomDP.svg?branch=master)](https://travis-ci.org/EDIorg/ecocomDP)
[![codecov.io](https://codecov.io/github/EDIorg/ecocomDP/coverage.svg?branch=master)](https://codecov.io/github/EDIorg/ecocomDP?branch=master)
<!-- badges: end -->

A dataset design pattern for ecological community data to facilitate
synthesis and reuse

With the almost forty year existence of the Long-term Ecological
Research (LTER) Network and other networks worldwide, a diverse array of
long-term observational and experimental data is becoming increasingly
available. A number of data repositories are making the data accessible,
with the intent that accompanying detailed metadata allow meaningful
reuse, repurpose and integration with other data.

However, in synthesis research the largest time investment is still in
discovering, cleaning and combining primary datasets until all data are
completely understood and converted to a similar format. There are two
approaches to achieving this data regularity: a) to prescribe the format
before data collection starts, or b) to convert primary data into a
flexible intermediate format for reuse. Prescribed formats have rarely
been successful due to a wide range of ecosystems represented, original
research questions that drive collection, and varying sampling and
analysis methods. Hence, we took the second approach: define a flexible
intermediate data model, and convert primary data to it. In the context
of the Environmental Data Initiative’s data repository, this allows us
to maintain the original dataset, which is most convenient for descibing
and answering the original research questions (and does not interfere
with depositors’ existing practices), add a conversion script to
reformat the data into the intermediate format and make the harmonized
intermediate available to synthesis research. This pre-harmonization
step may be accomplished by data managers because the dataset still
contains all original information without aggregation, filtering or
cleaning necessary for targeted research questions. Although the data
are still distributed into distinct datasets, they can easily be
discovered, aggregated, and converted further into other formats, for
specific secondary use.

![](https://github.com/EDIorg/ecocomDP/blob/master/documentation/images/ecocom_dp_workflow_cut.png)

*Figure: Abstract view of dataset levels. A flexible intermediate (L1,
middle) lies between datasets of primary observations (L0, left) and the
aggregated views used by synthesis projects. If datasets are in a
recognized format, EDI can create tools for some basic functions*

## Installation

You can install the released version of ecocomDP from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ecocomDP")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EDIorg/ecocomDP", ref = "development")
```

## Example

#### Use

  - Find:
      - Use the
        [`view_all_ecocomDP()`](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions#discover)
        function to list all ecocomDP datasets. This function is apart
        of the `ecocomDP` R package.
      - [Environmental Data Initiative
        (EDI)](https://portal.edirepository.org:443/nis/simpleSearch?defType=edismax&q=ecocomDP&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false)
        Enter "ecocomDP’ in the ‘simple search’ box in the EDI data
        repository.
      - [National Ecological Observatory Network
        (NEON)](https://github.com/EDIorg/ecocomDP/tree/master/documentation/examples/NEON)
        Use the
        [`view_all_ecocomDP()`](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions#discover)
        function to list all NEON data available in the ecocomDP format.
        This function is apart of the `ecocomDP` R package.
  - Import to R:
      - Use the
        [`aggregate_ecocomDP()`](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions#reuse)
        function to combine ecocomDP datasets. This function is apart of
        the `ecocomDP` R package.

#### Create

  - [Table descriptions, relationships, and
    requriements.](https://github.com/EDIorg/ecocomDP/tree/master/documentation/model)
  - [Instructions for creating, validating, and archiving ecocomDP
    tables](https://github.com/EDIorg/ecocomDP/tree/master/documentation/instructions)
  - [List of datasets to be converted to
    ecocomDP](https://github.com/EDIorg/ecocomDP/tree/master/documentation/processing_queue)

<!-- end list -->

``` r
library(ecocomDP)
## basic example code
```

## Getting help

Use [GitHub Issues](https://github.com/EDIorg/ecocomDP/issues) for bug
reporting, feature requests, and general questions/discussions. When
filing bug reports, please include a minimal reproducible example.

## Contributing

Community contributions are welcome\! Please reference our [contributing
guidelines](https://github.com/EDIorg/ecocomDP/blob/master/CONTRIBUTING.md)
for details.

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/EDIorg/ecocomDP/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
