## Revision
This release fixes breaking changes introduced by the suggested usmap package 
and makes one minor performance improvement to a non-exported function.

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
0 ERROR | 0 WARNINGS | 1 NOTE

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
RESPONSE: This issue occurs in one test environment (Windows, R-hub, R-devel 
(2022-02-21 r81789 ucrt)) and appears to be related to the virtual machine
configuration.

## Downstream dependencies
There are currently no downstream dependencies for this package

Many thanks!