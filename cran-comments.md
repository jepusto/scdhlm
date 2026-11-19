## Resubmission

This is a re-submission. This version adds a `Bayesian` option to `calc_BCSMD()` for fiting multilevel models and estimating effect sizes using Markov Chain Monte Carlo, along with the associated helper function `g_mlm_Bayes()`. Unit tests for the built-in Shiny web application have been migrated to shinytest2. 
All problems shown on the current CRAN checks have been resolved, including for the additional check on MKL.

## Test environments

* local Windows 11 Education, R 4.5.1
* ubuntu 22.04.5 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R devel, release
* win-builder (devel, release, oldrelease)
* Fedora Linux 44 MKL (rhub container image), R devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

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

