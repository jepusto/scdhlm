[![Build Status](https://travis-ci.org/jepusto/scdhlm.svg?branch=master)](https://travis-ci.org/jepusto/scdhlm)
[![Coverage Status](https://img.shields.io/codecov/c/github/jepusto/scdhlm/master.svg)](https://codecov.io/github/jepusto/scdhlm?branch=master)
[![](http://www.r-pkg.org/badges/version/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scdhlm)](https://CRAN.R-project.org/package=scdhlm)
[![](http://cranlogs.r-pkg.org/badges/last-month/scdhlm)](https://CRAN.R-project.org/package=scdhlm)

An R package for estimating design-comparable standardized mean difference effect sizes based on data from single-case designs. Implements methods described in: 

* Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012). A standardized mean difference effect size for single case designs. Research Synthesis Methods, 3, 224-239. doi: [10.1002/jrsm.1052](http://doi.org/10.1002/jrsm.1052)
* Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). A standardized mean difference effect size for multiple baseline designs across individuals. Research Synthesis Methods, 4(4), 324-341. doi: [10.1002/jrsm.1086](http://doi.org/10.1002/jrsm.1086)
* Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). Design-comparable effect sizes in multiple baseline designs: A general modeling framework. Journal of Educational and Behavioral Statistics, 39(4), 211-227. doi: [10.3102/1076998614547577](http://doi.org/10.3102/1076998614547577)

The package also includes an [interactive web-app](https://jepusto.shinyapps.io/scdhlm/) (implemented using Shiny) that can be accessed within RStudio as follows:

```r
library(scdhlm)
shine_scd()
```

