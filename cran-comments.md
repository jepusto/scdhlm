## Resubmission

This is a re-submission. This version includes extensive user-interface and back-end updates to the built-in shiny app (invoked by `shine_scd()`), as well as a few bug fixes. The vignette, examples, and unit tests have been updated so that the package can be compiled without any Suggested packages.

## Test environments

* local Windows 10 Education, R 4.2.0
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)

## R CMD check results

There were no ERRORs and no WARNINGs. 

There was 1 NOTE:

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/jrsm.1052
    From: inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1002/jrsm.1086
    From: inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0022466907310371
    From: inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1901/jaba.2008.41-107
    From: inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.3102/1076998614547577
    From: inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/001440290507200101
    From: man/BartonArwood.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/00224669070410010201
    From: man/AlberMorgan.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0022466907310371
    From: man/Saddler.Rd
          inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0022466919883397
    From: man/Peltier.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/002246699202600101
    From: man/CaseHarrisGraham.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0145445508317133
    From: man/Schutte.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/1098300713492858
    From: man/Rodriguez.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.3102/1076998614547577
    From: man/MB1results.Rd
          man/MB2results.Rd
          man/MB4results.Rd
          man/Schutte.Rd
          man/compare_RML_HPS.Rd
          man/g_REML.Rd
          man/simulate_MB2.Rd
          man/simulate_MB4.Rd
          inst/doc/Estimating-effect-sizes.html
    Status: 503
    Message: Service Unavailable
    
    The flagged URLs are correct.

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/jrsm.1052
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
  DOI: 10.1002/jrsm.1086
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
  DOI: 10.3102/1076998614547577
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
    
    The flagged DOIs are correct.
    
## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
