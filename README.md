<!-- README.md is generated from README.Rmd. Please edit that file -->



# ecocomDP <a href="https://ediorg.github.io/ecocomDP/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ecocomDP)](https://cran.r-project.org/package=ecocomDP)
<!-- badges: end -->

## Overview

Tools to create, use, and convert 'ecocomDP' datasets. 'ecocomDP' is a dataset design pattern for harmonizing ecological community surveys in a research question agnostic format, from source datasets published across multiple repositories, and with methods that keep the derived datasets up-to-date as the underlying sources change. Described in O'Brien et al. (2021), <https://doi.org/10.1016/j.ecoinf.2021.101374>.

## Installation

Get the latest CRAN release:

``` r
install.packages("ecocomDP")
```

Get the latest development version:

``` r
# install.packages("remotes")
remotes::install_github("EDIorg/ecocomDP", ref = "development")
```

## Authentication

Accessing data from the Environmental Data Initiative (EDI) repository requires an API key.

### 1. Obtain an EDI API Key
1. Visit the [EDI Identity and Access Manager (IAM)](https://auth.edirepository.org/) and log in (or create a free account).
2. Navigate to the [Access Keys](https://auth.edirepository.org/auth/ui/key) page.
3. Generate a new API key (or copy an existing key).

### 2. Configure `EDI_API_KEY` in R

Configure your API key using any of the following methods:

**Option A: Persistent Configuration (Recommended)**  
Add `EDI_API_KEY` to your user-level `.Renviron` file so it is loaded automatically in every R session:

``` r
# Open your .Renviron file:
usethis::edit_r_environ()

# Add this line, save the file, and restart R:
EDI_API_KEY="your_api_key_here"
```

**Option B: In-Session via `Sys.setenv()`**  
Set the key in your current R session before making API calls:

``` r
Sys.setenv(EDI_API_KEY = "your_api_key_here")
```

**Option C: In-Session via `EDIutils`**  

``` r
EDIutils::login(key = "your_api_key_here")
```

## Usage

* [Create ecocomDP Data](https://ediorg.github.io/ecocomDP/articles/create.html)
* [Use ecocomDP Data](https://ediorg.github.io/ecocomDP/articles/use.html)
* [Convert ecocomDP Data to Another Model Format](https://ediorg.github.io/ecocomDP/articles/convert.html)

## Model documentation
* [Model Overview](https://ediorg.github.io/ecocomDP/articles/model_overview.html)
* [Shared Practices for Creating ecocomDP Data](https://ediorg.github.io/ecocomDP/articles/shared_practices_create.html)

## Getting help

Use [GitHub Issues](https://github.com/EDIorg/ecocomDP/issues) for bug reporting, feature requests, and general questions/discussions. When filing bug reports, please include a minimal reproducible example. 

## Contributing

Community contributions are welcome! Please reference our [contributing guidelines](https://github.com/EDIorg/ecocomDP/blob/master/CONTRIBUTING.md) for details.

-----

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/EDIorg/ecocomDP/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
