# scdhlm 0.7.3

* Fixed a bug in `graph_SCD()` that caused an error in plotting data from treatment reversal designs with slopes based on a fitted models.
* Updated tests for compatibility with new version of ggplot2.

# scdhlm 0.7.2

* Modified handling of missing outcome observations in `preprocess_SCD()` so that the session-by-treatment interaction variable is computed prior to dropping missing outcome observations. 
* Modified `Bryant2018` example dataset to clarify the timing of treatment phases.
* Added a `newdata` argument to `graph_SCD()`, which allows for computing fitted values from `model_fit` based on a different design structure than the actual data.
* Updates to package documentation.

# scdhlm 0.7.1

* Fixed problem in unit tests leading to failure on the CRAN ATLAS build. 

# scdhlm 0.7.0

* New convenience functions:
  * Added a convenience function `calc_BCSMD()` for pre-processing data, fitting an `lme()` model, and calculating `g_mlm()` all in one go.
  * Added a convenience function `default_times()` for calculating default time-points for multiple baseline designs.
  * Added `batch_calc_BCSMD()` function for calculating BC-SMD effect size estimates for multiple studies of the same design.
* Changes to the shiny app:
  * Fixed bug in shiny app that occurred when uploading a multiple baseline design or a treatment reversal design.
  * Updated the layout of model output in the shiny app.
  * Added `rclipboard` package to the installation instruction of the shiny app.
  * Added Bryant et al. (2018) and Thiemann & Goldstein (2001) to the references in the shiny app.
  * Updated the study design labels in example datasets for multiple baselines across participants.
* Changes to example datasets:
  * Added the academic response outcome data from Lambert et al. (2006) to the package and the shiny app.
  * Updated the Bryant et al. (2018) dataset using the group instead of school as the cluster variable.


# scdhlm 0.6.0

* Six more example datasets added to the package and the `scdhlm` web app.
* Revisions to shiny app: 
  * The app now includes several options for modeling the dependence structure of level-1 error terms, including AR(1) (the default), MA(1), or independent errors.
  * The app now includes an option for allowing the variance of the level-1 errors to differ by phase.
  * More informative labels for the baseline trend and treatment phase trend options.
  * The centering, initial treatment time, and follow-up time sliders now only appear when they are relevant. 
  * The centering slider now appears in the "Model estimates" tab because it is only relevant for interpreting the raw estimates from the fitted model (i.e., it does not affect the graph of fitted values). 
  * The "Model" tab now includes a note regarding initial treatment time and follow-up time sliders, which only appears when relevant.
* Fixed a bug so that shine_scd() can take a tibble in the dataset argument.
* Fixed a bug in graph_SCD() function that occurred in treatment reversal designs with cases that had varying numbers of reversals.
* Updated vignette, examples, and unit tests so that the package can be compiled without any packages from SUGGESTS.

# scdhlm 0.5.2

* Fixed bug in shiny app occurring when filtering variable(s) of the input data set.

* Updated shiny app to clean the variable names of the input data.

# scdhlm 0.5.1

* Modified the `Schutte` example dataset to exclude the fourth case, for consistency with the analysis presented in Pustejovsky, Hedges, & Shadish (2014).

* Revised the definition of the treatment-by-time interaction variable calculated in `preprocess_SCD()` and in the shiny app, for consistency with Pustejovsky, Hedges, & Shadish (2014).

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
