## Revision
This release fixes failing CRAN checks due to network issues.

## Test environments
* x86_64-w64-mingw32 (64-bit), (win-builder), R version 4.4.1 (2024-06-14 ucrt)
* x86_64-w64-mingw32 (64-bit), (win-builder), R version 4.3.3 (2024-02-29 ucrt)
* x86_64-w64-mingw32 (64-bit), (win-builder), R Under development (unstable) (2024-08-15 r87022 ucrt)
* x86_64-pc-linux-gnu (R-hub), R Under development (unstable) (2024-08-15 r87022)
* x86_64-pc-linux-gnu (R-hub), R version 4.4.1 Patched (2024-08-14 r87022)
* x86_64-pc-linux-gnu (R-hub), R version 4.4.1 (2024-06-14)
* x86_64-w64-mingw32 (R-hub), R Under development (unstable) (2024-08-15 r87022 ucrt)
* aarch64-apple-darwin20 (R-hub), R Under development (unstable) (2024-08-05 r86980)

## R CMD check results
0 ERROR | 0 WARNINGS | 1 NOTES

* checking CRAN incoming feasibility ... [22s] NOTE
  Maintainer: 'Colin Smith <colin.smith@wisc.edu>'
  
  Found the following (possibly) invalid URLs:
    URL: https://github.com/EDIorg/ecocomDP/commit/f2bdb91a4e1a3b633ebed3bffbf1773ac8565e80
      From: NEWS.md
      Status: 406
      Message: Not Acceptable
    URL: https://www.itis.gov/
      From: man/create_eml.Rd
            man/create_taxon.Rd
            inst/doc/shared_practices_create.html
      Status: 404
      Message: Not Found

  These URLs are valid and working. The first URL is a commit to the ecocomDP repository on GitHub. 
  The second URL is the Integrated Taxonomic Information System (ITIS) website. This note occurs on:

## Downstream dependencies
There are currently no downstream dependencies for this package.

Thank you! :)
