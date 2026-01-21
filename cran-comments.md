## Resubmission

This is a re-submission and a maintenance release. The only change is updating the DESCRIPTION to list all packages used in /demo scripts, to rectify a NOTE thrown with v0.7.3.

## Test environments

* local Windows 11 Education, R 4.4.3
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R devel, release
* win-builder (devel, release, oldrelease)
* mac-builder (release)

## R CMD check results

There were no ERRORs and no WARNINGs. 

There were several NOTEs:

Possibly mis-spelled words in DESCRIPTION:
  Klingbeil (16:28)
  Pustejovsky (13:29, 14:13, 15:5, 16:15)
  Shadish (13:46, 14:30, 15:30)
  
  The flagged words are spelled correctly.

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/jrsm.1052
    From: inst/doc/Estimating-effect-sizes.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1002/jrsm.1086
    From: inst/doc/Estimating-effect-sizes.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1177/0022466907310371
    From: inst/doc/Estimating-effect-sizes.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1901/jaba.2008.41-107
    From: inst/doc/Estimating-effect-sizes.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.3102/1076998614547577
    From: inst/doc/Estimating-effect-sizes.html
    Status: 403
    Message: Forbidden    
    
    The flagged URLs are all correct.

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/jrsm.1052
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
  DOI: 10.1002/jrsm.1086
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
  DOI: 10.3102/1076998614547577
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
    The flagged DOIs are all correct.
    
## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
