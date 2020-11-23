# scdhlm 0.5.1

* Modified the `Schutte` example dataset to exclude the fourth case, for consistency with the analysis presented in Pustejovsky, Hedges, & Shadish (2014).

# scdhlm 0.5.0

* Added a new function `preprocess_SCD()` which handles initial data-cleaning steps for multiple baseline and treatment reversal designs.

* The `scdhlm` Shiny app now includes a tab with R code for replicating the app calculations.

# scdhlm 0.4.2

* Imported `g_mlm()` from `lmeInfo`.

* Updated vignette using `g_mlm()`.

* Updated README using `g_mlm()`.

* Updated shiny app using `g_mlm()`.

* Imported `extract_varcomp()` from `lmeInfo`. This function extracts variance components from a fitted `lme` model, which can then be used for `g_mlm()`.

* Updated `CI_g()` to allow calculating symmetric and asymmetric confidence intervals for `g_HPS` objects, `g_REML` objects, and `g_mlm()` objects. Note that symmetric confidence interval is the default.

# scdhlm 0.3.2

* Updated HPS estimation functions to work with datasets (issue #2 from austinj).

* Added additional example datasets (Ruiz, Salazar, Thiemann2001, Thiemann2004, Bryant2018).

* Updated web-app to allow use of .xlsx files.

* Fixed bug in web-app occurring when cases were listed out of alphabetical order.

* Fixed bug in web-app occurring when auto-correlation is very weakly identified.

# scdhlm 0.3.1

* Updated shiny app with additional documentation.

* Added additional example datasets (BartonArwood, Rodriguez, Romaniuk, AlberMorgan).

# scdhlm 0.3

* Shiny app for calculating between-case standardized mean difference effect sizes.

# scdhlm 0.2.2

* Bug fix in lme_AR1_cov_block_inv.

* Fixed bug in HPS effect size functions so that results are not dependent on order of data.

# scdhlm 0.2.1

* Added vignette demonstrating use of g_REML.

# scdhlm 0.2

* Initial release.
