## Revision
This release fixes failing CRAN Checks and makes one minor performance 
improvement to a non-exported function.

## Test environments
* local Windows install, R 4.1.0
* x86_64-w64-mingw32 (64-bit), (win-builder), R 4.0.5
* x86_64-w64-mingw32 (64-bit), (win-builder), R 4.1.2
* x86_64-w64-mingw32 (64-bit), (win-builder), R-dev (2022-03-03 r81847 ucrt)
* Windows Server 2008 R2 SP1, 32/64 bit (R-hub), R 4.1.2
* Windows Server 2008 R2 SP1, 32/64 bit (R-hub), R-devel (2022-02-21 r81789 ucrt)
* macOS 10.13.6 High Sierra, (R-hub), R 4.1.1
* Debian Linux, GCC (R-hub), R 4.1.2
* Ubuntu Linux 20.04.1 LTS, GCC (R-hub), R 4.1.2
* Ubuntu Linux 20.04.1 LTS, GCC (R-hub), R-devel (2022-02-06 r81658)
* Fedora Linux, clang, gfortran (R-hub), R-devel (2022-02-06 r81658)

## R CMD check results
0 ERROR | 0 WARNINGS | 2 NOTES

* Found the following (possibly) invalid URLs:
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=194&revision=3
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=247&revision=3
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=248&revision=2
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=251&revision=2
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=253&revision=3
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=338&revision=2
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=118&revision=33
    From: man/ants_L0_flat.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/metadataviewer?packageid=edi.291.2
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable
  URL: https://portal.edirepository.org/nis/metadataviewer?packageid=edi.334.2
    From: inst/doc/shared_practices_create.html
    Status: 503
    Message: Service Unavailable

These URLs are not invalid. They all resolve on major web browsers without error
(i.e. Google Chrome, Mozilla Firefox, Microsoft Edge, Internet Explorer). These 
URLs reference the landing pages of data sources in the Environmental Data 
Initiative Repository. This note occurs in two test environments (win-builder 
R 4.1.2, and win-builder R-devel 2022-03-04 r81849 ucrt).

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
This note occurs in one test environment (Windows, R-hub, R-devel 
(2022-02-21 r81789 ucrt)) and appears to be related to the virtual machine
configuration.

## Downstream dependencies
There are currently no downstream dependencies for this package

Thanks! :)