# ecocomDP 1.1.0

## Enhancements
* Link to ecocomDP article.
* Enable full return of L0 location columns in `flatten_data()`.
* Implement a human readable row sorting based on location_id values in `create_location()`.
* Check create_ecocomDP.R for expected function and associated arguments in `create_eml()`.

## Fixes
* Fix alignment of categorical variable names and definitions in `create_eml()`.
* Allow only one basisOfRecord in `create_eml()`.
* Fix numeric type detection in `create_eml()`.
* Fix taxonomic hierarchy expander in `create_eml()`.
* Fix empty annotation defaults in `create_eml()`.
* Remove all L0 data entities from the L1 EML, these should not be inherited by the L1 in `create_eml()`.
* Handle both present/absent L0 taxonomic coverage in `create_eml()`.
* Fix assignment of self referencing ids in `create_location()`.
* Only return unique locations in `create_location()`.
* Fix methods in `calc_number_of_years_sampled()` and `calc_length_of_survey_years()`.
* Fix handling of datetimes with YYYY format in `read_data()`.
* Include ancillary table datetime in join operations of `flatten_data()`.
* Remove XML attributes to prevent id clashing and schema invalidation when constructing provenance nodes in `create_eml()` and `convert_to_dwca()`.
* Fix basisOfRecord reference in `convert_to_dwca()`.
* Incorporate L0 methods markdown blocks in `convert_to_dwca()`.
* Prevent namespace clash with the taxonomyCleanr package.

# ecocomDP 1.0.0

* The ecocomDP package is now available on CRAN