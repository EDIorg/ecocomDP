## Revision
This release fixes data mapping issues and improves plotting functions.

## Test environments
* x86_64-w64-mingw32 (64-bit), (win-builder), R 4.2.3 (2023-03-15 ucrt)
* x86_64-w64-mingw32 (64-bit), (win-builder), R 4.3.2 (2023-10-31 ucrt)
* x86_64-w64-mingw32 (64-bit), (win-builder), R-dev (2024-02-15 r85925 ucrt)
* x86_64-w64-mingw32 (R-hub), R 4.3.2 (2023-10-31 ucrt)
* x86_64-w64-mingw32 (R-hub), R-devel (2023-11-18 r85554 ucrt)
* aarch64-apple-darwin20, (local machine), R 4.3.0
* x86_64-pc-linux-gnu Debian (R-hub), R 4.3.2 (2023-10-31)
* x86_64-pc-linux-gnu Ubuntu (R-hub), 4.3.2 (2023-10-31)
* x86_64-pc-linux-gnu Ubuntu (R-hub), R-devel (2023-12-26 r85738)
* x86_64-pc-linux-gnu Fedora (R-hub), R-devel (2023-12-26 r85738)

## R CMD check results
0 ERROR | 0 WARNINGS | 4 NOTES

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

This note appears to be an issue with the R-hub platform. This note occurs on:
* x86_64-w64-mingw32 (R-hub), R-devel (2023-11-18 r85554 ucrt)
* x86_64-w64-mingw32 (R-hub), R 4.3.2 (2023-10-31 ucrt)
For more information see [here](https://github.com/r-hub/rhub/issues/560).

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

This note is a recognized bug on R-hub test platforms. This note occurs on:
* x86_64-w64-mingw32 (R-hub), R-devel (2023-11-18 r85554 ucrt)
* x86_64-w64-mingw32 (R-hub), R 4.3.2 (2023-10-31 ucrt)
For more information see [here](https://community.rstudio.com/t/prep-error-during-rhub-check-for-cran/159467).

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

This note appears to be an issue with the R-hub test platform. This note occurs on:
* x86_64-pc-linux-gnu Fedora (R-hub), R-devel (2023-12-26 r85738)
* x86_64-pc-linux-gnu Ubuntu (R-hub), R-devel (2023-12-26 r85738)
* x86_64-pc-linux-gnu Ubuntu (R-hub), 4.3.2 (2023-10-31)
For more information see [here](https://stackoverflow.com/questions/74857062/rhub-cran-check-keeps-giving-html-note-on-fedora-test-no-command-tidy-found).

* checking examples ... [25s/55s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
user system elapsed
plot_sites 3.198 0.110 7.257
plot_taxa_abund 3.182 0.013 7.063

This note indicates a couple plotting functions are taking a little more time than is optimal. We think the overage time is acceptable since it is on the order of seconds. This note occurs on:
* x86_64-pc-linux-gnu Fedora (R-hub), R-devel (2023-12-26 r85738)
* x86_64-pc-linux-gnu Debian (R-hub), R 4.3.2 (2023-10-31)
* x86_64-pc-linux-gnu Ubuntu (R-hub), R-devel (2023-12-26 r85738)
* x86_64-pc-linux-gnu Ubuntu (R-hub), 4.3.2 (2023-10-31)

## Downstream dependencies
There are currently no downstream dependencies for this package.

Thank you! :)
