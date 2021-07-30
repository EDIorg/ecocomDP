## Revision
This release fixes a few bugs and implements a few enhancements

## Test environments
* local Windows install, R 4.0.2
* Windows x86_64-w64-mingw32 (64-bit) (win-builder), R 4.1.0, R-devel
* macOS 10.13.6 High Sierra, (on R-hub), R 4.1.0
* Debian Linux, GCC (on R-hub), R 4.1.0
* Fedora Linux, clang, gfortran (on R-hub), R-devel

## R CMD check results
2 ERROR | 0 WARNINGS | 1 NOTE

### NOTE: (Windows x86_64-w64-mingw32 (R 4.1.0, R-devel), Debian Linux (R 4.1.0), Fedora Linux, clang, gfortran (R 4.1.0))
* "Possibly mis-spelled words in DESCRIPTION: al (4:371) et (4:368)"
  These are not mis-spelled words.
  
### ERROR: (Windows x86_64-w64-mingw32 (R 4.1.0))
* Package required and available but unsuitable version: 'neonUtilities'
  neonUtilities 2.1.1 is available on CRAN as of 2021-07-27

### ERROR: (Windows x86_64-w64-mingw32 (R 4.1.0))
* Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called 'pillar'
  pillar is not an import, dependency, or suggestion of ecocomDP.

## Downstream dependencies
There are currently no downstream dependencies for this package

Many thanks for your review!