## Resubmission

This is a re-submission. This version adds a `Bayesian` option to `calc_BCSMD()` for fiting multilevel models and estimating effect sizes using Markov Chain Monte Carlo, along with the associated helper function `g_mlm_Bayes()`. Unit tests for the built-in Shiny web application have been migrated to shinytest2. All problems shown on the current CRAN checks have been resolved, including for the additional check on MKL.

## Test environments

* local Windows 11 Education, R 4.5.1
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R devel, release
* win-builder (devel, release, oldrelease)

## R CMD check results

There were no ERRORs and no WARNINGs. 

There was one NOTE:

Uses the superseded package: 'doSNOW'

  The doSNOW package is included in SUGGESTS because it is used in a /demo script that implements the original simulation studies that motivated the package. I would prefer to keep the demo script in its original form for purposes of reproducibility.

## reverse dependency checks

------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends  1

Check results summary:
scdhlm ... OK
rdepends_lmeInfo ... OK

------- Check for regressions ------
No changes between old and new version

