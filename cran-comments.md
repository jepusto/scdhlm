## Resubmission

This is a re-submission. In this version we have:

* Corrected a unit test that led to errors in the CRAN checks.
* Fixed a bug in shiny app when filtering variable(s) of the input data set.
* Updated shiny app to clean the variable names in the data read-in process.

## Test environments

* local Windows 10 Pro, R 4.0.2
* local Windows 10 Education, R 4.0.2
* ubuntu 16.04.6 LTS (on travis-ci), R-release, R-devel
* win-builder (devel, release, oldrelease)
* r-hub:
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Debian Linux, R-devel, GCC

## R CMD check results

There were no ERRORs and no WARNINGs. 

There was 1 NOTE:

Possibly mis-spelled words in DESCRIPTION:
  Pustejovsky (9:29, 9:70)
  Shadish (9:46, 9:95)
  
  All of the identified words are spelled correctly. 
