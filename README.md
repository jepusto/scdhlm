
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scdhlm

<!-- badges: start -->

[![R-CMD-check](https://github.com/jepusto/scdhlm/workflows/R-CMD-check/badge.svg)](https://github.com/jepusto/scdhlm/actions)
[![Codecov
Status](https://codecov.io/gh/jepusto/scdhlm/branch/master/graph/badge.svg)](https://codecov.io/gh/jepusto/scdhlm?branch=master)
[![](http://www.r-pkg.org/badges/version/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/last-month/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
<!-- badges: end -->

## Estimating Hierarchical Linear Models for Single-Case Designs

`scdhlm` provides a set of tools for estimating hierarchical linear
models and effect sizes based on data from single-case designs. The
estimated effect sizes, as described in Pustejovsky, Hedges, and Shadish
(2014), are comparable in principle to standardized mean differences
(SMDs) estimated from between-subjects randomized experiments. The
package includes functions for estimating design-comparable SMDs based
on data from ABAB designs (or more general treatment reversal designs),
multiple baseline designs, and multiple probe designs. Two estimation
methods are available: moment estimation (Hedges, Pustejovsky, &
Shadish, 2012; 2013) and restricted maximum likelihood estimation
(Pustejovsky, Hedges, & Shadish, 2014). The package also includes an
interactive web interface implemented using Shiny.

## Acknowledgement

The development of this R package was supported in part by the Institute
of Education Sciences, U.S. Department of Education, through [Grant
R324U190002](https://ies.ed.gov/funding/grantsearch/details.asp?ID=3358)
to the University of Oregon. The contents of the package do not
necessarily represent the views of the Institute or the U.S. Department
of Education.

## Installation

You can install the released version of `scdhlm` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("scdhlm")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jepusto/scdhlm")
```

You can access a local version of the [interactive
web-app](https://jepusto.shinyapps.io/scdhlm/) by running the commands:

``` r
library(scdhlm)
shine_scd()
```

## Demonstration

Here we demonstrate how to use `scdhlm` to calculate design-comparable
SMDs based on data from different single-case designs. We will first
demonstrate the recommended approach, which uses restricted maximum
likelihood (REML) estimation. We will then demonstrate the older, moment
estimation methods. The moment estimation methods were the originally
proposed approach (described in Hedges, Pustejovsky, & Shadish, 2012,
2013). The package provides these methods for sake of completeness, but
we no longer recommend them for general use.

### Estimating SMDs using REML with `g_mlm()`

Laski, Charlop, and Schreibman (1988) used a multiple baseline across
individuals to evaluate the effect of a training program for parents on
the speech production of their autistic children, as measured using a
partial interval recording procedure. The design included eight
children. One child was measured separately with each parent; following
Hedges, Pustejovsky, and Shadish (2013), only the measurements taken
with the mother are included in our analysis.

For this study, we will estimate a design-comparable SMD using
restricted maximum likelihood (REML) methods, as described by
Pustejovsky and colleagues (2014). This is a two-step process. The first
step is to estimate a hierarchical linear model for the data, treated
the measurements as nested within cases. We fit the model using
`nlme::lme()`

``` r
library(nlme)
library(scdhlm)
data(Laski)

# Pustejovsky, Hedges, & Shadish (2014)
Laski_RML <- lme(fixed = outcome ~ treatment,
                 random = ~ 1 | case, 
                 correlation = corAR1(0, ~ time | case), 
                 data = Laski)
Laski_RML
#> Linear mixed-effects model fit by REML
#>   Data: Laski 
#>   Log-restricted-likelihood: -519.1424
#>   Fixed: outcome ~ treatment 
#>        (Intercept) treatmenttreatment 
#>           39.07612           30.68366 
#> 
#> Random effects:
#>  Formula: ~1 | case
#>         (Intercept) Residual
#> StdDev:    15.68278  13.8842
#> 
#> Correlation Structure: AR(1)
#>  Formula: ~time | case 
#>  Parameter estimate(s):
#>      Phi 
#> 0.252769 
#> Number of Observations: 128
#> Number of Groups: 8
```

The summary of the fitted model displays estimates of the component
parameters, including the within-case and between-case standard
deviations, auto-correlation, and (unstandardized) treatment effect
estimate. These estimated components will be used to calculate the
effect size in next step.

The estimated variance components from the fitted model can be obtained
using `extract_varcomp()`:

``` r
varcomp_Laski_RML <- extract_varcomp(Laski_RML)
varcomp_Laski_RML
#> $Tau
#> $Tau$case
#> case.var((Intercept)) 
#>              245.9497 
#> 
#> 
#> $cor_params
#> [1] 0.252769
#> 
#> $var_params
#> numeric(0)
#> 
#> $sigma_sq
#> [1] 192.7711
#> 
#> attr(,"class")
#> [1] "varcomp"
```

The estimated between-case variance is 245.95, the estimated
auto-correlation is 0.253, the estimated and the estimated within-case
variance is 192.771. These estimated variance components will be used to
calculate the effect size in next step.

The second step in the process is to estimate a design-comparable SMD
using `scdhlm::g_mlm()`. The SMD parameter can be defined as the ratio
of a linear combination of the fitted model’s fixed effect parameters
over the square root of a linear combination of the model’s variance
components. `g_mlm()` takes the fitted `lme` model object as input,
followed by the vectors `p_const` and `r_const`, which specify the
components of the fixed effects and variance estimates that are to be
used in constructing the design-comparable SMD. Note that `r_const` is a
vector of 0s and 1s which specify whether to use the variance component
parameters for calculating the effect size: random effects variances,
correlation structure parameters, variance structure parameters, and
level-1 error variance. The function calculates an effect size estimate
by first substituting maximum likelihood or restricted maximum
likelihood estimates in place of the corresponding parameters, then
applying a small-sample correction. The small-sample correction and the
standard error are based on approximating the distribution of the
estimator by a t distribution, with degrees of freedom given by a
Satterthwaite approximation (Pustejovsky, Hedges, & Shadish, 2014). The
`g_mlm()` function includes an option allowing use of the expected or
average form of the Fisher information matrix in the calculations.

In this example, we use the treatment effect in the numerator of the
effect size and the sum of the between-case and within-case variance
components in the denominator of the effect size. The constants are
therefore given by `p_const = c(0, 1)` and `r_const = c(1, 0, 1)`. The
effect size estimated is calculated as:

``` r
Laski_ES_RML <- g_mlm(Laski_RML, p_const = c(0, 1), r_const = c(1, 0, 1))

Laski_ES_RML
#>                           est    se
#> unadjusted effect size  1.465 0.299
#> adjusted effect size    1.405 0.286
#> degree of freedom      18.552
```

The adjusted SMD effect size estimate is 1.405 with standard error of
0.286 and degree of freedom 18.6.

A `summary()` method is included, which returns more detail about the
model parameter estimates and effect size estimate when setting
`returnModel = TRUE` (the default) in `g_mlm()`:

``` r
summary(Laski_ES_RML)
#>                                           est      se
#> Tau.case.case.var((Intercept))        245.950 142.179
#> cor_params                              0.253   0.100
#> sigma_sq                              192.771  28.265
#> total variance                        438.721 144.047
#> (Intercept)                            39.076   5.989
#> treatmenttreatment                     30.684   2.996
#> treatment effect at a specified time   30.684   2.996
#> unadjusted effect size                  1.465   0.299
#> adjusted effect size                    1.405   0.286
#> degree of freedom                      18.552        
#> constant kappa                          0.143        
#> logLik                               -519.142
```

The `CI_g()` calculates a symmetric confidence interval using a central
t distribution (the default) or an asymmetric confidence interval using
non-central t distribution (setting `symmetric = FALSE`).

``` r
CI_g(Laski_ES_RML)
#> [1] 0.8046224 2.0051521

CI_g(Laski_ES_RML, symmetric = FALSE)
#> [1] 0.9143684 2.0046719
```

The symmetric confidence interval is \[0.805, 2.005\] and the asymmetric
confidence interval is \[0.914, 2.005\].

### Estimating SMDs using `effect_size_ABk()`

Lambert, Cartledge, Heward, and Lo (2006) tested the effect of using
response cards (compared to single-student responding) during math
lessons in two fourth-grade classrooms. The investigators collected data
on rates of disruptive behavior for nine focal students, using an ABAB
design. This example is discussed in Hedges, Pustejovsky, and Shadish
(2012), who selected it because the design was close to balanced and
used a relatively large number of cases. Their calculations can be
replicated using the `effect_size_ABk()` function. To use this function,
the user must provide the names of five variables:

  - the outcome variable,
  - a variable indicating the treatment condition,
  - a variable listing the case on which the outcome was measured,
  - a variable indicating the phase of treatment (i.e., each replication
    of a baseline and treatment condition), and
  - a variable listing the session number.

In the `Lambert` dataset, these variables are called respectively
`outcome`, `treatment`, `case`, `phase`, and `time`. Given these inputs,
the design-comparable SMD is calculated as follows:

``` r
data(Lambert)

Lambert_ES <- effect_size_ABk(outcome = outcome, treatment = treatment, id = case, 
                              phase = phase, time = time, data = Lambert)

Lambert_ES
#>                            est    se
#> unadjusted effect size  -2.525 0.202
#> adjusted effect size    -2.513 0.201
#> degree of freedom      164.492
```

The adjusted effect size estimate `delta_hat` is equal to -2.513; its
variance `V_delta_hat` is equal to 0.041. A standard error for
`delta_hat` can be calculated by taking the square root of
`V_delta_hat`: `sqrt(Lambert_ES$V_delta_hat)` = 0.201. The effect size
estimate is bias-corrected in a manner analogous to Hedges’ g correction
for SMDs from a between-subjects design. The degrees of freedom `nu` are
estimated based on a Satterthwaite-type approximation, which is equal to
164.5 in this example.

A summary() method is included to return more detail about the model
parameter estimates and effect size estimates:

``` r
summary(Lambert_ES)
#>                                       est    se
#> within-case variance                4.534      
#> sample variance                     4.674      
#> intra-class correlation             0.030      
#> auto-correlation                    0.225      
#> numerator of effect size estimate  -5.458      
#> unadjusted effect size             -2.525 0.202
#> adjusted effect size               -2.513 0.201
#> degree of freedom                 164.492      
#> scalar constant                     0.145
```

### Estimating SMDs using `effect_size_MB()`

Saddler, Behforooz, and Asaro (2008) used a multiple baseline design to
investigate the effect of an instructional technique on the writing of
fourth grade students. The investigators assessed the intervention’s
effect on measures of writing quality, sentence complexity, and use of
target constructions.

Design-comparable SMDs can be estimated based on these data using the
`effect_size_MB()` function. The following code calculates a
design-comparable SMD estimate for the measure of writing quality:

``` r
data(Saddler)

Saddler_quality <- subset(Saddler, measure=="writing quality")
quality_ES <- effect_size_MB(outcome, treatment, case, time, data = Saddler_quality)

quality_ES
#>                          est    se
#> unadjusted effect size 2.149 0.634
#> adjusted effect size   1.963 0.579
#> degree of freedom      8.918
```

The adjusted effect size estimate `delta_hat` is equal to 1.963, with
sampling variance of `V_delta_hat` equal to 0.335 and a standard error
of 0.579.

`summary(quality_ES)` returns more detail about the model parameter
estimates and effect size estimates:

``` r
summary(quality_ES)
#>                                     est    se
#> within-case variance              0.349      
#> sample variance                   0.952      
#> intra-class correlation           0.633      
#> auto-correlation                  0.100      
#> numerator of effect size estimate 2.097      
#> unadjusted effect size            2.149 0.634
#> adjusted effect size              1.963 0.579
#> degree of freedom                 8.918      
#> scalar constant                   0.201
```

## References

Gilmour, A. R., Thompson, R., & Cullis, B. R. (1995). Average
information REML: An efficient algorithm for variance parameter
estimation in linear mixed models. *Biometrics, 51*(4), 1440–1450.
<https://doi.org/10.2307/2533274>

Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012). A
standardized mean difference effect size for single case designs.
*Research Synthesis Methods, 3*(3), 224-239. doi:
[10.1002/jrsm.1052](http://doi.org/10.1002/jrsm.1052)

Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). A
standardized mean difference effect size for multiple baseline designs
across individuals. *Research Synthesis Methods, 4*(4), 324-341. doi:
[10.1002/jrsm.1086](http://doi.org/10.1002/jrsm.1086)

Lambert, M. C., Cartledge, G., Heward, W. L., & Lo, Y. (2006). Effects
of response cards on disruptive behavior and academic responding during
math lessons by fourth-grade urban students. *Journal of Positive
Behavior Interventions, 8*(2), 88-99. doi:
[10.1177/10983007060080020701](https://doi.org/10.1177/10983007060080020701)

Laski, K. E., Charlop, M. H., & Schreibman, L. (1988). Training parents
to use the natural language paradigm to increase their autistic
children’s speech. *Journal of Applied Behavior Analysis, 21*(4),
391–400. doi:
[10.1901/jaba.1988.21-391](https://doi.org/10.1901/jaba.1988.21-391)

Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
Design-comparable effect sizes in multiple baseline designs: A general
modeling framework. *Journal of Educational and Behavioral Statistics,
39*(4), 211-227. doi:
[10.3102/1076998614547577](http://doi.org/10.3102/1076998614547577)

Saddler, B., Behforooz, B., & Asaro, K. (2008). The effects of
sentence-combining instruction on the writing of fourth-grade students
with writing difficulties. *The Journal of Special Education, 42*(2),
79–90. doi:
[10.1177/0022466907310371](http://doi.org/10.1177/0022466907310371)
