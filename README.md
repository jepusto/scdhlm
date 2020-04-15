
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scdhlm

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/jepusto/scdhlm.svg?branch=master)](https://travis-ci.org/jepusto/scdhlm)
[![Coverage
Status](https://img.shields.io/codecov/c/github/jepusto/scdhlm/master.svg)](https://codecov.io/github/jepusto/scdhlm?branch=master)
[![](http://www.r-pkg.org/badges/version/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/last-month/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
<!-- badges: end -->

## Estimating Hierarchical Linear Models for Single-Case Designs

`scdhlm` provides a set of tools for estimating hierarchical linear
models and effect sizes based on data from single-case designs. The
estimated effect sizes, as described in Pustejovsky, Hedges, and Shadish
(2014), are directly comparable to standardized mean differences (SMDs)
estimated from between-subjects randomized experiments. The package
includes functions for estimating design-comparable SMDs based on data
from ABAB designs (Hedges, Pustejovsky, and Shadish, 2012) and multiple
baseline designs (Hedges, Pustejovsky, and Shadish, 2013). The package
also includes an interactive web interface implemented using Shiny.

## Installation

You can install the released version of `scdhlm` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("scdhlm")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jepusto/scdhlm")
```

You can access the [interactive
web-app](https://jepusto.shinyapps.io/scdhlm/) with:

``` r
library(scdhlm)
shine_scd()
```

## Demonstration

We use three empirical datasets to demonstrate how `scdhlm` calculates
design-comparable SMDs with different estimation methods based on data
from different single-case designs.

### Estimating SMDs using `effect_size_ABk()`

Lambert, Cartledge, Heward, and Lo (2006) tested the effect of using
response cards (compared to single-student responding) during math
lessons in two fourth-grade classrooms. The investigators collected data
on rates of disruptive behavior for nine focal students, using an ABAB
design. This example is discussed in Hedges, Pustejovsky, and Shadish
(2012), who selected it because the design was close to balanced and
used a relatively large number of cases. Their calculations can be
replicated using the `effect_size_ABk()` function. To use this function,
the user must provide five pieces of data:

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
library(nlme)
library(scdhlm)
data(Lambert)

Lambert_ES <- effect_size_ABk(outcome = outcome, treatment = treatment, id = case, 
                              phase = phase, time = time, data= Lambert)

str(Lambert_ES)
#> List of 12
#>  $ M_a            : int [1:2, 1:2] 6 4 6 7
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:2] "SSR" "RC"
#>   .. ..$ : chr [1:2] "1" "2"
#>  $ M_dot          : int 23
#>  $ D_bar          : num -5.46
#>  $ S_sq           : num 4.67
#>  $ delta_hat_unadj: num -2.52
#>  $ phi            : num 0.225
#>  $ sigma_sq_w     : num 4.53
#>  $ rho            : num 0.0299
#>  $ theta          : num 0.145
#>  $ nu             : num 164
#>  $ delta_hat      : num -2.51
#>  $ V_delta_hat    : num 0.0405
#>  - attr(*, "class")= chr "g_HPS"
```

The function produces a list containing the estimated effect size
estimate, an estimate of its variance, and several pieces of auxiliary
information. The effect size estimate `delta_hat` is equal to -2.513;
its variance `V_delta_hat` is equal to 0.041. The effect size estimate
is bias-corrected in a manner analogous to the correction in Hedges’ g
for SMDs from a between-subjects design. The degrees of freedom `nu` are
estimated based on a Satterthwaite-type approximation, which is equal to
164.492 in this example.

### Estimating SMDs using `effect_size_MB()`

Saddler, Behforooz, and Asaro (2008) used a multiple baseline design to
investigate the effect of an instructional technique on the writing of
fourth grade students. The investigators assessed the intervention’s
effect on measures of writing quality, sentence complexity, and use of
target constructions.

Design-comparable SMDs can be estimated based on these data using the
`effect_size_MB()` function. The following code replicates the
calculations reported in Hedges, Pustejovsky, and Shadish (2013):

``` r
data(Saddler)

quality_ES <- effect_size_MB(outcome, treatment, case, time, 
                             data= subset(Saddler, measure=="writing quality"))
complexity_ES <- effect_size_MB(outcome, treatment, case, time , 
                                data= subset(Saddler, measure=="T-unit length"))
construction_ES <- effect_size_MB(outcome, treatment, case, time, 
                                  data= subset(Saddler, measure=="number of constructions"))

cbind(quality = unlist(quality_ES), 
      complexity = unlist(complexity_ES), 
      construction = unlist(construction_ES))[c("delta_hat","V_delta_hat","nu","phi","rho"),]
#>                quality  complexity construction
#> delta_hat   1.96307272  0.78540043   0.74755356
#> V_delta_hat 0.33491289  0.08023320   0.07847359
#> nu          8.91814603  9.60204004   7.57981360
#> phi         0.09965017 -0.07542229  -0.11159420
#> rho         0.63321198  0.61453091   0.73123744
```

### Estimating SMDs using `g_REML()`

Laski, Charlop, and Schreibman (1988) used a multiple baseline across
individuals to evaluate the effect of a training program for parents on
the speech production of their autistic children, as measured using a
partial interval recording procedure. The design included \(m = 8\)
children; one child was measured separately with each parent; for
purposes of simplicity, and following Hedges, Pustejovsky, and Shadish
(2013), only the measurements taken with the mother are included in the
analysis.

The following code estimates a design-comparable SMD with the
hierarchical linear modeling approach (Pustejovsky et al., 2014) using
the `g_REML()` function.

  - Step 1: fit the hierarchical model using `nlme::lme()`

<!-- end list -->

``` r
data(Laski)

# Pustejovsky, Hedges, & Shadish (2014)
Laski_RML <- lme(fixed = outcome ~ treatment,
                 random = ~ 1 | case, 
                 correlation = corAR1(0, ~ time | case), 
                 data = Laski)
summary(Laski_RML)
#> Linear mixed-effects model fit by REML
#>  Data: Laski 
#>        AIC      BIC    logLik
#>   1048.285 1062.466 -519.1424
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
#> Fixed effects: outcome ~ treatment 
#>                       Value Std.Error  DF   t-value p-value
#> (Intercept)        39.07612  5.989138 119  6.524498       0
#> treatmenttreatment 30.68366  2.995972 119 10.241637       0
#>  Correlation: 
#>                    (Intr)
#> treatmenttreatment -0.272
#> 
#> Standardized Within-Group Residuals:
#>         Min          Q1         Med          Q3         Max 
#> -2.72642154 -0.69387388  0.01454473  0.69861200  2.14528141 
#> 
#> Number of Observations: 128
#> Number of Groups: 8
```

The summary of the fitted model displays estimates of the component
parameters, including the within-case and between-case standard
deviations, auto-correlation, and (unstandardized) treatment effect
estimate. Those estimated components will be used to estimate the effect
size in next step.

  - Step 2: estimate design-comparable SMD using `scdhlm::g_REML()`

The SMD parameter can be defined as the ratio of a linear combination of
the fitted model’s fixed effect parameters over the square root of a
linear combination of the model’s variance components. `g_REML()` takes
the fitted `lme` model object as input, followed by the vectors
`p_const` and `r_const`, which specify the components of the fixed
effects and variance estimates that are to be used in constructing the
design-comparable SMD. In this example, we use the treatment effect in
the numerator of the effect size and the sum of the within-case and
between-case variance components in the denominator of the effect size.
The constants are therefore given by `p_const = c(0, 1)` and `r_const =
c(1, 0, 1)`. The effect size estimated is calculated as:

``` r
Laski_ES_RML <- g_REML(Laski_RML, p_const = c(0,1), r_const = c(1,0,1), returnModel=FALSE)

str(Laski_ES_RML)
#> List of 14
#>  $ p_beta   : num 30.7
#>  $ r_theta  : num 439
#>  $ delta_AB : num 1.46
#>  $ nu       : num [1, 1] 18.6
#>  $ kappa    : num [1, 1] 0.143
#>  $ g_AB     : num [1, 1] 1.4
#>  $ V_g_AB   : num [1, 1] 0.082
#>  $ cnvg_warn: logi FALSE
#>  $ sigma_sq : num 193
#>  $ phi      : num 0.253
#>  $ Tau      : Named num 246
#>   ..- attr(*, "names")= chr "case.var((Intercept))"
#>  $ I_E_inv  : num [1:3, 1:3] 798.92 1.32 -132.12 1.32 0.01 ...
#>  $ p_const  : num [1:2] 0 1
#>  $ r_const  : num [1:3] 1 0 1
#>  - attr(*, "class")= chr "g_REML"
```

The function returns a list containing the SMD effect size estimate
(`g_AB` = 1.405), its variance (`V_g_AB` = 0.082), the estimated
auto-correlation (`phi` = 0.253), estimated degrees of freedom (`nu` =
18.552), and several other pieces of auxiliary information.

## References

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
